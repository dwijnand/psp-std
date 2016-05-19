package psp
package std

import api._, exp._

trait BooleanAlgebra[A] extends Any with spire.Bool[A]

object Algebras {
  object Identity extends BooleanAlgebra[Bool] {
    def and(x: Bool, y: Bool): Bool = x && y
    def or(x: Bool, y: Bool): Bool  = x || y
    def complement(x: Bool): Bool   = !x
    def zero: Bool                  = false
    def one: Bool                   = true
  }

  final case class Not1[A](f: ToBool[A]) extends ToBool[A] with ShowSelf {
    def apply(x: A): Boolean = !f(x)
    def to_s                 = "!" + f
  }
  final class Predicate1Algebra[A] extends BooleanAlgebra[ToBool[A]] {
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
      case Not1(f)       => f
      case _             => Not1(f)
    }
  }

  final case class Not2[A, B](f: ToBool2[A, B]) extends ToBool2[A, B] with ShowSelf {
    def apply(x: A, y: B): Bool = !f(x, y)
    def to_s                    = "!" + f
  }
  final class Predicate2Algebra[A, B] extends BooleanAlgebra[ToBool2[A, B]] {
    private type R = ToBool2[A, B]

    def and(x: R, y: R): R = (a, b) => x(a, b) && y(a, b)
    def or(x: R, y: R): R  = (a, b) => x(a, b) || y(a, b)
    def zero: R            = ConstantFalse2
    def one: R             = ConstantTrue2
    def complement(f: R): R = f match {
      case ConstantFalse2 => ConstantTrue2
      case ConstantTrue2  => ConstantFalse2
      case Not2(f)        => f
      case _              => Not2(f)
    }
  }
}
