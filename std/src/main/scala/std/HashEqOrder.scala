package psp
package std

import api._, all._

object HashEqOrd {
  val Lexical = inherited[String]
  val Longs   = new Impl[Long](_ == _, (x, y) => longCmp(x - y), x => x)

  class Impl[A](f: Relation[A], g: OrderRelation[A], h: ToLong[A]) extends HashEqOrd[A] {
    def eqv(x: A, y: A): Bool = f(x, y)
    def cmp(x: A, y: A): Cmp  = g(x, y)
    def hash(x: A): Long      = h(x)
  }

  def apply[A](f: Relation[A], g: OrderRelation[A], h: ToLong[A]): HashEqOrd[A] = new Impl(f, g, h)

  def by[A] = new HashEqOrdBy[A]

  def shown[A](implicit z: Show[A]): HashEqOrd[A]  = by[A](z.show)
  def inherited[A <: Comparable[A]] : HashEqOrd[A] = apply[A](_ == _, (x, y) => longCmp(x compareTo y), _.##)
}
object Eq {
  val Inherited = hash[Any](_ == _)(_.##)
  val Reference = hash[AnyRef](_ eq _)(_.id_##)
  val ToString  = hashBy[Any](_.any_s)(Inherited)

  def apply[A](e: Relation[A]): Eq[A]                = new EqImpl[A](e)
  def hash[A](e: Relation[A])(h: ToLong[A]): Hash[A] = new HashImpl[A](e, h)

  final class HashImpl[A](e: Relation[A], h: ToLong[A]) extends EqImpl[A](e) with Hash[A] {
    def hash(x: A): Long = h(x)
  }
  sealed class EqImpl[A](val e: Relation[A]) extends Eq[A] {
    def eqv(x: A, y: A): Bool = e(x, y)
  }
  class EqComparator[A : Eq]() extends Comparator[A] {
    def compare(x: A, y: A): Int = if (x === y) 0 else x.id_## - y.id_##
  }

  def eqComparator[A : Eq](): Comparator[A] = new EqComparator[A]
}
object Order {
  def apply[A](f: OrderRelation[A]): Order[A] = new Impl[A](f)

  class Impl[A](f: OrderRelation[A]) extends Order[A] {
    def eqv(x: A, y: A): Bool = cmp(x, y) eq Cmp.EQ
    def cmp(x: A, y: A): Cmp  = f(x, y)
  }
}
