package psp
package std

import all._

/** Foreach is the common parent of View and Each.
  *
  *  A View always wraps an indeterminate number of Views
  *  and a single Each which provides the original basis.
  *  An Each may be composed from smaller Eaches but is
  *  otherwise atomic. The size of an Each is known, the
  *  size of a View may not be.
  */
trait Foreach[+A] extends Any {
  def foreach(f: A => Unit): Unit
}
trait Each[+A] extends Any with Foreach[A]

trait Indexed[+A] extends Any with Each[A] {
  def apply(idx: Vdex): A
}
trait Direct[+A] extends Any with Indexed[A] {
  def size: Precise

  def foreach(f: A => Unit): Unit = ll.foreachDirect(this, f)
  def head: A                     = apply(_0) // depend on this
  def reverse: Direct[A]          = size.getInt |> (e => Makes.fromInts(n => apply(Index(e - 1 - n)), 0, e))
}

final class Suspend[A](c: Folded[A]) extends Each[A] {
  def foreach(f: A => Unit): Unit = c resume f
}

sealed trait Folded[+A] extends Any
object Folded {
  def apply[A](mf: Suspended[A]): Folded[A] = Opaque(mf)

  final case class Opaque[A](mf: Suspended[A])                    extends Folded[A]
  final case class Join[A](c1: Folded[A], c2: Folded[A])          extends Folded[A]
  final case class Filter[A](c: Folded[A], p: ToBool[A])          extends Folded[A]
  final case class Mapped[A, B](c: Folded[A], g: A => B)          extends Folded[B]
  final case class FlatMap[A, B](c: Folded[A], g: A => Folded[B]) extends Folded[B]
}
