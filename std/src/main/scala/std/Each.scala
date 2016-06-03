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
trait Each[+A] extends Any with Foreach[A] {
  def size: Size
}
trait Indexed[+A] extends Any with Each[A] {
  def size: Atomic
  def apply(idx: Vdex): A
}
trait Generated[+A] extends Any with Indexed[A] {
  def size = Infinite
}
trait Direct[+A] extends Any with Indexed[A] {
  def size: Precise

  def applyReverse(idx: Vdex): A  = apply(size.lastIndex - idx.indexValue)
  def foreach(f: A => Unit): Unit = ll.foreachDirect(this, f)
  def head: A                     = apply(_0) // depend on this
}

final class Suspend[+A](c: Folded[A]) extends Each[A] {
  def size                        = Size.Unknown
  def foreach(f: A => Unit): Unit = c.foldl(())((xs, x) => f(x))
}

final class Folded[+A](mf: Suspended[A]) {
  def suspend(): Suspend[A] = new Suspend(this)
  def foldl[B](zero: B)(f: (B, A) => B): B = {
    var z = zero
    mf(x => z = f(z, x))
    z
  }
}
object Folded {
  def apply[A](mf: Suspended[A]): Folded[A]                 = new Folded(mf)
  def each[R, A](xs: R)(implicit z: Walks[A, R]): Folded[A] = new Folded(z walk xs foreach _)
}
