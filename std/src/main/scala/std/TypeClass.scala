package psp
package std

import api._, all._

/** There are two kinds of extension methods associated with
 *  type classes.
 *
 *  1) Methods for use by objects which have the type
 *     class instance in scope. This is the more common one.
 *  2) Methods for use by the type classes themselves,
 *     generally to create a derived variety.
 */

// final class OrderOps[A](val lhs: A) extends AnyVal {
//   import Cmp._
//   def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.cmp(lhs, rhs)
//   def < (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) eq LT
//   def <=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) ne GT
//   def > (rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) eq GT
//   def >=(rhs: A)(implicit ord: Order[A]): Boolean  = compare(rhs) ne LT
// }
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

final class BuildsTcOps[Elem, To](z: Builds[Elem, To]) {
  def map[Next](f: To => Next): Builds[Elem, Next] = Builds(xs => f(z build xs))
  def scalaBuilder: scmBuilder[Elem, To]           = sciVector.newBuilder[Elem] mapResult (z build _.toEach)
}
