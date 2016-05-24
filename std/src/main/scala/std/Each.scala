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

final class Suspend[A](c: Cont[A]) extends Each[A] {
  def foreach(f: A => Unit): Unit = c resume f
}

sealed trait Cont[A] extends Any
object Cont {
  def apply[A](mf: Suspended[A]): Cont[A] = Opaque(mf)

  final case class Opaque[A](mf: Suspended[A])                extends Cont[A]
  final case class Join[A](c1: Cont[A], c2: Cont[A])          extends Cont[A]
  final case class Filter[A](c: Cont[A], p: ToBool[A])        extends Cont[A]
  final case class Mapped[A, B](c: Cont[A], g: A => B)        extends Cont[B]
  final case class FlatMap[A, B](c: Cont[A], g: A => Cont[B]) extends Cont[B]

  implicit class ContOps[A](c: Cont[A]) {
    def join(that: Cont[A]): Cont[A]         = Join(c, that)
    def filter(p: ToBool[A]): Cont[A]        = Filter(c, p)
    def map[B](f: A => B): Cont[B]           = Mapped(c, f)
    def flatMap[B](f: A => Cont[B]): Cont[B] = FlatMap(c, f)

    def suspend(): Suspend[A] = new Suspend(c)
    def resume(f: ToUnit[A]): Unit = c match {
      case Opaque(mf)     => mf(f)
      case Join(c1, c2)   => c1 resume f; c2 resume f
      case Filter(c, p)   => c resume (x => if (p(x)) f(x))
      case Mapped(c, g)   => c resume (x => g andThen f)
      case FlatMap(c, g)  => c resume (x => g(x) resume f)
    }
  }
}
