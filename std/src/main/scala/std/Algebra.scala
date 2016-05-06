package psp
package std

import api._, exp._

trait BooleanAlgebra[A] extends Any with spire.Bool[A]

object Algebras {
  final case class Not[A](f: ToBool[A]) extends ToBool[A] with ShowSelf {
    def apply(x: A): Boolean = !f(x)
    def to_s = "!" + f
  }
  final class PredicateAlgebra[A] extends BooleanAlgebra[ToBool[A]] {
    private type R = ToBool[A]

    /** TODO - one of of the benefits of having constant true and false is an
     *  opportunity to optimize expressions away entirely with no evaluation,
     *  if e.g. y is ConstantTrue in x(p) || y(p). Obviously this won't mix well
     *  with side effects. How enthusiastic can we be about punishing side effects
     *  before we kill the patient?
     */
    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
    def complement(f: R): R = f match {
      case ConstantFalse => ConstantTrue
      case ConstantTrue  => ConstantFalse
      case Not(f)        => f
      case _             => Not(f)
    }
  }
}
