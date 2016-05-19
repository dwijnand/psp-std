package psp
package std

import api._, all._

object Relation {
  val Inherited: Hash[Any]       = hash[Any](_ == _, _.##)
  val Reference: Hash[Any]       = hash[Any](_ id_== _, _.id_##)
  val Lexical: HashOrder[String] = byComparable[String]
  val ToString: HashOrder[Any]   = allBy[Any](_.any_s)(Lexical)
  val Longs: HashOrder[Long]     = all[Long]((x, y) => longCmp(x - y), identity)

  def equiv[A](r: EqRelation[A]): Eq[A]                       = new EqImpl(r)
  def hash[A](r: EqRelation[A], h: ToLong[A]): Hash[A]        = new HashImpl(r, h)
  def order[A](r: OrderRelation[A]): Order[A]                 = new OrderImpl(r)
  def all[A](r: OrderRelation[A], h: ToLong[A]): HashOrder[A] = new AllImpl(r, h)

  def allBy[A]                                       = new AllBy[A]
  def inherited[A]: Hash[A]                          = Inherited
  def shown[A](implicit z: Show[A]): HashOrder[A]    = allBy[A](z.show)
  def byComparable[A <: Comparable[A]]: HashOrder[A] = all[A]((x, y) => longCmp(x compareTo y), _.##)

  class EqImpl[A](r: EqRelation[A]) extends Eq[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
  }
  class HashImpl[A](r: EqRelation[A], h: ToLong[A]) extends Hash[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
    def hash(x: A): Long      = h(x)
  }
  class OrderImpl[A](r: OrderRelation[A]) extends Order[A] {
    def eqv(x: A, y: A): Bool = r(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = r(x, y)
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
  final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A]         = z on f }
  final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]           = z on f }
  final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]           = z on f }
  final class AllBy[A]   { def apply[B](f: A => B)(implicit z: HashOrder[B]): HashOrder[A] = z on f }
}

trait StdRelation0 {
  implicit def combineHash[A](implicit z: Order[A], h: Hash[A]): HashOrder[A] = z hashWith h.hash
  implicit def sizeRel: Hash[Size]                                            = Relation.hash(Size.equiv, _.##)
  implicit def comparableRel[A <: Comparable[A]]: HashOrder[A]                = Relation.all[A]((x, y) => longCmp(x compareTo y), _.##)
}
trait StdRelation1 extends StdRelation0 {
  implicit def eqViewsAs[R, A](implicit b: ViewsAs[A, R], z: Eq[A]): Eq[R] =
    Relation.equiv((xs, ys) => intoView(xs) zip intoView(ys) corresponds z.eqv)

  implicit def optionEq[A: Eq]: Eq[Opt[A]] = Relation equiv {
    case (Some(x), Some(y)) => x === y
    case (x, y)             => x.isEmpty && y.isEmpty
  }

  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A] = orderBy[A](_.ordinal)
  implicit def longHashOrder: HashOrder[Long]       = Relation.Longs
  implicit def boolHashOrder: HashOrder[Bool]       = Relation allBy (x => if (x) 1 else 0)
  implicit def charHashOrder: HashOrder[Char]       = Relation allBy (x => x: Long)
  implicit def intHashOrder: HashOrder[Int]         = Relation allBy (x => x: Long)
  implicit def vindexHashOrder: HashOrder[Vdex]     = Relation allBy (_.indexValue)
  implicit def preciseHashOrder: HashOrder[Precise] = Relation allBy (_.getLong)
  implicit def stringHashOrder: HashOrder[String]   = Relation.Lexical

  implicit def classEq: Hash[Class[_]] = Relation.Inherited

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Relation equiv {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }
}
trait StdRelation extends StdRelation1 {
  implicit def intervalRelation: HashOrder[Interval] =
    Relation allBy (x => x.startLong -> x.size.preciseOrMaxLong)

  implicit def product2HashOrder[A: HashOrder, B: HashOrder]: HashOrder[A -> B] =
    Relation.all[A -> B](
      orderBy[A -> B](fst) | snd cmp,
      xy => fst(xy).hash + snd(xy).hash
    )
}
