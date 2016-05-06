package psp
package std
package ops


import api._, all._

/** There are two kinds of extension methods associated with
 *  type classes.
 *
 *  1) Methods for use by objects which have the type
 *     class instance in scope. This is the more common one.
 *  2) Methods for use by the type classes themselves,
 *     generally to create a derived variety.
 */

final class OrderOps[A](val lhs: A) extends AnyVal {
  import Cmp._
  def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.cmp(lhs, rhs)
  def < (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) eq LT
  def <=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) ne GT
  def > (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) eq GT
  def >=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) ne LT

  def min(rhs: A)(implicit ord: Order[A]): A = if (this < rhs) lhs else rhs
  def max(rhs: A)(implicit ord: Order[A]): A = if (this > rhs) lhs else rhs
}
final class AlgebraOps[A](val lhs: A) extends AnyVal {
  // def ^(rhs: A)(implicit z: BooleanAlgebra[A]): A       = (lhs || rhs) && !(lhs && rhs) // xor
  // def implies(rhs: A)(implicit z: BooleanAlgebra[A]): A = !lhs || rhs
  // def && (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) z.zero else z.and(lhs, rhs)
  // def || (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) z.one else z.or(lhs, rhs)
  def unary_!(implicit z: BooleanAlgebra[A]): A         = if (isZero) z.one else if (isOne) z.zero else z.complement(lhs)
  def isZero(implicit z: BooleanAlgebra[A]): Boolean    = z.zero id_== lhs
  def isOne(implicit z: BooleanAlgebra[A]): Boolean     = z.one id_== lhs
}
final class EqOps[A](val lhs: A) extends AnyVal {
  def ===(rhs: A)(implicit z: Eq[A]): Boolean = z.eqv(lhs, rhs)
  def =!=(rhs: A)(implicit z: Eq[A]): Boolean = !z.eqv(lhs, rhs)
}
final class JavaIteratorOps[A](it: jIterator[A]) {
  def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
}
final class CmpEnumOps(val cmp: Cmp) {
  def |(that: => Cmp): Cmp = if (cmp == Cmp.EQ) that else cmp
}

/** The second variety begins here.
 */

final class BuildsTypeClassOps[Elem, To](z: Builds[Elem, To]) {
  def map[Next](f: To => Next): Builds[Elem, Next] = Builds(xs => f(z build xs))
  def scalaBuilder: scmBuilder[Elem, To]           = sciVector.newBuilder[Elem] mapResult (z build _.toEach)
}
