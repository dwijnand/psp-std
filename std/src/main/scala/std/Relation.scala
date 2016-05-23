package psp
package std

import api._, all._

object Order {
  val Inherited: HashOrder[String] = comparable[String] hashWith (_.##)
  val ToString: HashOrder[Any]     = Relation.allBy[Any](_.any_s)(Inherited)

  def apply[A](r: OrderRelation[A]): Order[A]      = new OrderImpl(r)
  def by[A]: OrderBy[A]                            = new OrderBy[A]
  def comparable[A <: Comparable[A]]: HashOrder[A] = apply[A]((x, y) => longCmp(x compareTo y)) hashWith (_.##)
  def shown[A](implicit z: Show[A]): Order[A]      = by[A](_.pp)(Inherited)

  class OrderImpl[A](r: OrderRelation[A]) extends Order[A] {
    def eqv(x: A, y: A): Bool = r(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = r(x, y)
  }
  final class OrderBy[A] {
    def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = z on f
  }
}
object Eq {
  def apply[A](r: EqRelation[A]): Eq[A] = new EqImpl(r)
  def by[A]: EqBy[A]                    = new EqBy[A]

  class EqImpl[A](r: EqRelation[A]) extends Eq[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
  }
  final class EqBy[A] {
    def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A] = z on f
  }
}

object Relation {
  val Inherited: Hash[Any]   = hash[Any](_ == _, _.##)
  val Reference: Hash[Any]   = hash[Any](_ id_== _, _.id_##)
  val Longs: HashOrder[Long] = all[Long]((x, y) => longCmp(x - y), identity)

  def hash[A](r: EqRelation[A], h: ToLong[A]): Hash[A]        = new HashImpl(r, h)
  def all[A](r: OrderRelation[A], h: ToLong[A]): HashOrder[A] = new AllImpl(r, h)
  def allBy[A]                                                = new AllBy[A]
  def inherited[A]: Hash[A]                                   = Inherited

  class HashImpl[A](r: EqRelation[A], h: ToLong[A]) extends Hash[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
    def hash(x: A): Long      = h(x)
  }
  class AllImpl[A](r: OrderRelation[A], h: ToLong[A]) extends HashOrder[A] {
    def eqv(x: A, y: A): Bool = r(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = r(x, y)
    def hash(x: A): Long      = h(x)
  }

  /** These classes all put the expected result type up front,
    *  where it can either be inferred from an existing value or
    *  supplied directly.
    */
  final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]           = z on f }
  final class AllBy[A]   { def apply[B](f: A => B)(implicit z: HashOrder[B]): HashOrder[A] = z on f }
}

trait StdRelation0 {
  implicit def combineHash[A](implicit z: Order[A], h: Hash[A]): HashOrder[A] = z hashWith h.hash
  implicit def sizeRelation: Hash[Size]                                       = Relation.hash(Size.equiv, _.##)
  implicit def comparableOrder[A <: Comparable[A]]: HashOrder[A]              = Order.comparable[A]
  implicit def viewsAsRelation[A, R](implicit b: Walks[A, R], z: Eq[A]): Eq[R] =
    Eq((xs, ys) => xs.m2 zip ys.m2 corresponds z.eqv)
}
trait StdRelation1 extends StdRelation0 {

  implicit def optionRelation[A: Eq]: Eq[Opt[A]] = Eq {
    case (Some(x), Some(y)) => x === y
    case (x, y)             => x.isEmpty && y.isEmpty
  }

  implicit def enumOrder[A <: jEnum[A]] : Order[A] = Order.comparable[A]

  implicit def longRelation: HashOrder[Long]       = Relation.Longs
  implicit def boolRelation: HashOrder[Bool]       = Relation allBy (x => if (x) 1L else 0L)
  implicit def charRelation: HashOrder[Char]       = Relation allBy (x => x: Long)
  implicit def intRelation: HashOrder[Int]         = Relation allBy (x => x: Long)
  implicit def vindexRelation: HashOrder[Vdex]     = Relation allBy (_.indexValue)
  implicit def preciseRelation: HashOrder[Precise] = Relation allBy (_.getLong)
  implicit def stringOrder: HashOrder[String]      = Order.Inherited
  implicit def classRelation: Hash[jClass]         = Relation.Inherited

  implicit def tryRelation[A: Eq](implicit z: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }
}
trait StdRelation extends StdRelation1 {
  implicit def intervalRelation: HashOrder[Interval] =
    Relation allBy (x => x.startLong -> x.size.preciseOrMaxLong)

  implicit def pairRelation[A: HashOrder, B: HashOrder]: HashOrder[A -> B] =
    Relation.all[A -> B](
      Order.by[A -> B](fst) | snd cmp,
      xy => fst(xy).hash + snd(xy).hash
    )
}
