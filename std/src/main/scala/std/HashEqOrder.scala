package psp
package std

import api._, all._
import scala.math.Ordering

object Eq {
  val Inherited = hash[Any](_ == _)(_.##)
  val Reference = hash[AnyRef](_ eq _)(_.id_##)
  val ToString  = hashBy[Any](_.any_s)(Inherited)

  def apply[A](e: Relation[A]): Eq[A]                = new EqImpl[A](e)
  def hash[A](e: Relation[A])(h: ToLong[A]): Hash[A] = new HashImpl[A](e, h)

  final class HashImpl[A](e: Relation[A], h: ToLong[A]) extends EqImpl[A](e) with api.Hash[A] {
    def hash(x: A): Long = h(x)
  }
  sealed class EqImpl[A](val e: Relation[A]) extends api.Eq[A] {
    def eqv(x: A, y: A) = e(x, y)
  }
  class EqComparator[A: Eq]() extends Comparator[A] {
    def compare(x: A, y: A): Int = if (x === y) 0 else x.id_## - y.id_##
  }

  def eqComparator[A: Eq](): Comparator[A] = new EqComparator[A]
}

object Order {
  def comparator[A](implicit z: Order[A]): Comparator[A] = new ToOrdering(z)

  private def longCmp(diff: Long): Cmp = (
    if (diff < 0) Cmp.LT
    else if (diff > 0) Cmp.GT
    else Cmp.EQ
  )

  def lexical: Order[String]                   = Order.fromInt(_ compareTo _)
  def inherited[A <: Comparable[A]](): Impl[A] = fromInt[A](_ compareTo _)
  def apply[A](f: (A, A) => Cmp): Impl[A]      = new FromRelation[A](f)
  def fromInt[A](f: (A, A) => Int): Impl[A]    = apply[A]((x, y) => longCmp(f(x, y)))
  def fromLong[A](f: (A, A) => Long): Impl[A]  = apply[A]((x, y) => longCmp(f(x, y)))

  def impl[A](order: api.Order[A]): Impl[A] = new FromApiOrder(order)

  sealed abstract class Impl[A](f: OrderRelation[A]) extends api.Order[A] {
    def eqv(x: A, y: A): Bool    = cmp(x, y) == Cmp.EQ
    def cmp(x: A, y: A): Cmp     = f(x, y)
    def compare(x: A, y: A): Int = cmp(x, y).intValue

    def | [B](f: A => B)(implicit z: Order[B]): Order[A] = apply((x, y) => cmp(x, y) | z.cmp(f(x), f(y)))
  }
  final class FromApiOrder[A](z: api.Order[A])        extends Impl[A](z.cmp)
  final class FromComparator[A](c: Comparator[A])     extends Impl[A]((x, y) => longCmp(c.compare(x, y)))
  final class FromRelation[A](f: OrderRelation[A])    extends Impl[A](f)
  final class ToOrdering[A](z: Order[A])              extends Ordering[A] { def compare(x: A, y: A): Int = z.compare(x, y) }
}
