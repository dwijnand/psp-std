package psp
package std

import api._, all._

object Relation {
  val Inherited: Hash[Any]       = hash[Any](_ == _, _.##)
  val Reference: Hash[Any]       = hash[Any](_ id_== _, _.id_##)
  val Lexical: HashEqOrd[String] = byComparable[String]
  val ToString: HashEqOrd[Any]   = allBy[Any](_.any_s)(Lexical)
  val Longs: HashEqOrd[Long]     = all[Long]((x, y) => longCmp(x - y), identity)

  def equiv[A](r: EqRelation[A]): Eq[A]                       = new EqImpl(r)
  def hash[A](r: EqRelation[A], h: ToLong[A]): Hash[A]        = new HashImpl(r, h)
  def order[A](r: OrderRelation[A]): Order[A]                 = new OrderImpl(r)
  def all[A](r: OrderRelation[A], h: ToLong[A]): HashEqOrd[A] = new AllImpl(r, h)

  def allBy[A]                                       = new AllBy[A]
  def inherited[A]: Hash[A]                          = Inherited
  def shown[A](implicit z: Show[A]): HashEqOrd[A]    = allBy[A](z.show)
  def byComparable[A <: Comparable[A]]: HashEqOrd[A] = all[A]((x, y) => longCmp(x compareTo y), _.##)

  class EqImpl[A](val r: EqRelation[A]) extends Eq[A] {
    def eqv(x: A, y: A): Bool = r(x, y)
  }
  class HashImpl[A](r: EqRelation[A], h: ToLong[A]) extends EqImpl[A](r) with Hash[A] {
    def hash(x: A): Long = h(x)
  }
  class OrderImpl[A](r: OrderRelation[A]) extends Order[A] {
    def eqv(x: A, y: A): Bool = r(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = r(x, y)
  }
  class AllImpl[A](r: OrderRelation[A], h: ToLong[A]) extends HashEqOrd[A] {
    def eqv(x: A, y: A): Bool = r(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = r(x, y)
    def hash(x: A): Long      = h(x)
  }
}
