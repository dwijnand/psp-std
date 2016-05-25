package psp
package std

import all._

/**  Fully generalized type classes.
  *
  *  The more familiar type classes extends these with
  *  R specialized in the conventional way.
  */
trait MEq[-A, +R] extends Any {
  def eqv(x: A, y: A): R
}
trait MHash[-A, +R] extends Any {
  def hash(x: A): R
}
trait MOrder[-A, +R] extends Any {
  def less(x: A, y: A): R
}
trait MShow[-A, +R] extends Any {
  def show(x: A): R
}
trait MIndexed[-R, -I, +A] extends Any {
  def elem(x: R, index: I): A
}
trait MEmpty[+A] extends Any {
  def empty: A
}

/** The classic type classes for encoding equality, inequality,
  *  and display, and less classic ones for split/join, indexed access,
  *  emptiness.
  */
trait Eq[-A] extends Any with MEq[A, Bool] {
  def eqv(x: A, y: A): Bool
}
trait Hash[-A] extends Any with MHash[A, Long] {
  def hash(x: A): Long
}
trait Order[-A] extends Any with MOrder[A, Bool] {
  def less(x: A, y: A): Bool
}
trait Show[-A] extends Any with MShow[A, String] {
  def show(x: A): String
}
trait VIndexed[-R, +A] extends Any with MIndexed[R, Vdex, A] {
  def elem(x: R, index: Vdex): A
}
trait Empty[+A] extends Any with MEmpty[A] {
  def empty: A
}


trait IsProduct[-R, +A, +B] extends Any {
  type Left <: A
  type Right <: B
  def split(x: R): A -> B
}
trait MakesProduct[+R, -A, -B] extends Any {
  type Left >: A
  type Right >: B
  def join(x: A -> B): R
}
trait Productize[R, A, B] extends Any with MakesProduct[R, A, B] with IsProduct[R, A, B] {
  type Left  = A
  type Right = B
}

sealed trait IsCollection[-R, +A] extends Any
sealed trait IsIndexed[-R, -I, +A] extends Any with IsCollection[R, A] with MIndexed[R, I, A]

trait IsFolded[-R, +A] extends Any with IsCollection[R, A] {
  def resume(xs: R): Folded[A]
}
trait IsPairs[R, +A, +B] extends Any with IsCollection[R, A->B] {
  def pairs(xs: R): View[A->B]
}
trait IsIntIndexed[-R, +A] extends Any with IsIndexed[R, Int, A] {
  def length(x: R): Int
}
trait IsFunction[-R, -A, +B] extends Any {
  def apply(xs: R): Fun[A, B]
}

/*************
 * Companions.
 *************/

object Order {
  def Inherited[A <: Comparable[A]]: Order[A] = apply((x, y) => (x compareTo y) < 0)

  def apply[A](r: Relation[A]): Order[A] = new OrderImpl(r)
  def by[A]: OrderBy[A]                  = new OrderBy[A]

  class OrderImpl[A](r: Relation[A]) extends Order[A] {
    def less(x: A, y: A): Bool = r(x, y)
  }
  final class OrderBy[A] {
    def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = z on f
  }
}
object Eq {
  val Inherited: Eq[Any]    = apply(_ == _)
  val Reference: Eq[AnyRef] = apply(_ eq _)

  def apply[A](r: Relation[A]): Eq[A] = new EqImpl(r)
  def by[A]: EqBy[A]                    = new EqBy[A]

  class EqImpl[A](r: Relation[A]) extends Eq[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
  }
  final class EqBy[A] {
    def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A] = z on f
  }
}
object Hash {
  val Inherited: Hash[Any]    = apply(_.##)
  val Reference: Hash[AnyRef] = apply(_.id_##)

  def apply[A](h: ToLong[A]): Hash[A] = new HashImpl(h)
  def by[A]: HashBy[A]                = new HashBy[A]

  class HashImpl[A](h: ToLong[A]) extends Hash[A] {
    def hash(x: A): Long = h(x)
  }
  final class HashBy[A] {
    def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A] = z on f
  }
}
object Show {
  /** This of course is not implicit as that would defeat the purpose of the endeavor.
    *  There is however an implicit universal instance in the Unsafe object.
    */
  val Inherited: Show[Any] = apply[Any](s => zcond(s != null, s.toString))

  def apply[A](f: ToString[A]): Show[A] = new Impl[A](f)
  def by[A]: ShowBy[A]                  = new ShowBy[A]

  final class Impl[-A](val f: ToString[A]) extends AnyVal with Show[A] {
    def show(x: A) = f(x)
  }
  final class ShowBy[A] {
    def apply[B](f: A => B)(implicit z: Show[B]): Show[A] = z on f
  }
}
object Empty {
  def empty[A]: Empty[A]             = new Throws[A]("empty") // the empty empty
  def apply[A](empty: => A): Impl[A] = new Impl[A](empty)
  def const[A](empty: A): Const[A]   = new Const[A](empty)

  final class Throws[+A](msg: String) extends Empty[A] { def empty: A = abort(msg) }
  final class Impl[+A](expr: => A)    extends Empty[A] { def empty: A = expr }
  final class Const[+A](val empty: A) extends Empty[A] {}
}
object IsIntIndexed {
  def apply[R, A](len: R => Int, get: (R, Int) => A): IsIntIndexed[R, A] = new Impl(len, get)

  final class Impl[R, A](len: R => Int, get: (R, Int) => A) extends IsIntIndexed[R, A] {
    def length(xs: R): Int      = len(xs)
    def elem(xs: R, i: Int): A  = get(xs, i)
  }
}
object IsPairs {
  def apply[R, A, B](f: R => View[A->B]): IsPairs[R, A, B] = new Impl(f)

  final class Impl[R, A, B](f: R => View[A->B]) extends IsPairs[R, A, B] {
    def pairs(xs: R): View[A->B] = f(xs)
  }
}
object IsFolded {
  def apply[R, A](f: R => Suspended[A]): IsFolded[R, A] = new Impl(x => Folded(f(x)))

  final class Impl[R, A](f: R => Folded[A]) extends IsFolded[R, A] {
    def resume(xs: R): Folded[A] = f(xs)
  }
}
object Productize {
  def apply[R, A, B](f: (A, B) => R, l: R => A, r: R => B): Productize[R, A, B] = new Impl(f, l, r)

  final class Impl[R, A, B](f: (A, B) => R, l: R => A, r: R => B) extends Productize[R, A, B] {
    def split(x: R): A->B = l(x) -> r(x)
    def join(x: A->B): R  = x app f
  }
}
