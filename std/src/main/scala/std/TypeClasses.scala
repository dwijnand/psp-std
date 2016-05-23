package psp
package std

import all._

/** Type classes for "inside" a context.
  *  The more familiar type classes extends these with M[X] = X.
  *
  *  TODO: Investigate utility of further generalization of the
  *  ground types which appear here: Bool, Long, Vdex, Cmp, String
  */
trait MEq[M[+X], -A] extends Any {
  def eqv(x: M[A], y: M[A]): M[Bool]
}
trait MHash[M[+X], -A] extends Any {
  def hash(x: M[A]): M[Long]
}
trait MOrder[M[+X], -A] extends Any {
  def less(x: M[A], y: M[A]): M[Bool]
}
trait MShow[M[+X], -A] extends Any {
  def show(x: M[A]): M[String]
}
trait MIndexer[M[+X], -R, +A] extends Any {
  def elemAt(x: M[R], index: M[Vdex]): M[A]
}
trait MEmpty[M[+X], +A] extends Any {
  def empty: M[A]
}

/** Splitter/Joiner type classes for composing and decomposing an R into A -> B.
  *  Somewhat conveniently for us, "cleave" is a word which has among its meanings
  *  "to adhere firmly and closely as though evenly and securely glued" as well
  *  as "to divide into two parts by a cutting blow".
  */
trait MSplitter[M[+X], -R, +A, +B] extends Any {
  // Consider: what is the significance of M[A] -> M[B], not M[A -> B] ?
  def split(x: M[R]): M[A] -> M[B]
}
trait MJoiner[M[+X], +R, -A, -B] extends Any {
  def join(x: M[A] -> M[B]): M[R]
}
trait MCleaver[M[+X], R, A, B] extends Any with MJoiner[M, R, A, B] with MSplitter[M, R, A, B]

/** Scala makes certain things impossible with type parameters and a
  *  nearly disjoint set of ambitions impossible with type members.
  *  We are left with building them in redundantly so we always have
  *  what we need.
  */
trait ASplitter[-R] extends Any {
  type Left
  type Right
  type Pair >: R
}
trait AJoiner[+R] extends Any {
  type Left
  type Right
  type Pair <: R
}
trait ACleaver[R] extends Any with AJoiner[R] with ASplitter[R] {
  type Pair = R
}

/** The classic type classes for encoding equality, inequality,
  *  and display, and less classic ones for split/join, indexed access,
  *  emptiness.
  */
trait Eq[-A] extends Any with MEq[Id, A] {
  def eqv(x: A, y: A): Bool
}
trait Hash[-A] extends Any with MHash[Id, A] {
  def hash(x: A): Long
}
trait Order[-A] extends Any with MOrder[Id, A] {
  def less(x: A, y: A): Bool
}

trait Show[-A] extends Any with MShow[Id, A] {
  def show(x: A): String
}
trait Indexer[-R, +A] extends Any with MIndexer[Id, R, A] {
  def elemAt(x: R, index: Vdex): A
}
trait Empty[+A] extends Any with MEmpty[Id, A] {
  def empty: A
}
trait Splitter[-R, +A, +B] extends Any with MSplitter[Id, R, A, B] with ASplitter[R] {
  type Left <: A
  type Right <: B
  def split(x: R): A -> B
}
trait Joiner[+R, -A, -B] extends Any with MJoiner[Id, R, A, B] with AJoiner[R] {
  type Left >: A
  type Right >: B
  def join(x: A -> B): R
}
trait Cleaver[R, A, B] extends Any with MCleaver[Id, R, A, B] with Joiner[R, A, B] with Splitter[R, A, B] with ACleaver[R] {
  type Left  = A
  type Right = B
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
