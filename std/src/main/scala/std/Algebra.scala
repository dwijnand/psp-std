package psp
package std

import all._

object Algebra {
  object Identity extends BoolAlgebra[Bool] {
    def and(x: Bool, y: Bool): Bool = x && y
    def or(x: Bool, y: Bool): Bool  = x || y
    def complement(x: Bool): Bool   = !x
    def zero: Bool                  = false
    def one: Bool                   = true
  }
  class Fun1[A, B](implicit ba: BoolAlgebra[B]) extends BoolAlgebra[A => B] {
    type R = A => B

    def and(f: R, g: R): R  = x => ba.and(f(x), g(x))
    def or(f: R, g: R): R   = x => ba.or(f(x), g(x))
    def zero: R             = x => ba.zero
    def one: R              = x => ba.one
    def complement(f: R): R = x => ba.complement(f(x))
  }
  class Fun2[A, B, C](implicit ba: BoolAlgebra[C]) extends BoolAlgebra[(A, B) => C] {
    type R = (A, B) => C

    def and(f: R, g: R): R  = (x, y) => ba.and(f(x, y), g(x, y))
    def or(f: R, g: R): R   = (x, y) => ba.or(f(x, y), g(x, y))
    def zero: R             = (x, y) => ba.zero
    def one: R              = (x, y) => ba.one
    def complement(f: R): R = (x, y) => ba.complement(f(x, y))
  }

  /** TODO: These probably make no sense.
    */
  class ProductAlgebra[A, B](implicit ab: BoolAlgebra[A], bb: BoolAlgebra[B]) extends BoolAlgebra[A -> B] {
    def and(x: A -> B, y: A -> B): A -> B = (fst(x) && fst(y)) -> (snd(x) && snd(y))
    def or(x: A -> B, y: A -> B): A -> B  = (fst(x) || fst(y)) -> (snd(x) || snd(y))
    def complement(x: A -> B): A -> B     = !fst(x)            -> !snd(x)
    def zero: A -> B                      = ab.zero            -> bb.zero
    def one: A -> B                       = ab.one             -> bb.one
  }
  class OptionAlgebra[A](elem: A)(implicit ba: BoolAlgebra[A]) extends BoolAlgebra[Opt[A]] {
    val one: Some[A]    = Some(elem)
    val zero: None.type = None

    def and(x: Opt[A], y: Opt[A]): Opt[A] = for (a <- x; b <- y) yield a && b
    def or(x: Opt[A], y: Opt[A]): Opt[A] = (x, y) match {
      case Some(x) -> Some(y) => some(x || y)
      case Some(_) -> None    => x
      case None -> Some(_)    => y
      case _                  => none()
    }
    def complement(x: Opt[A]): Opt[A] = x match {
      case Some(_) => zero
      case _       => one
    }
  }

  /** TODO - one of of the benefits of having constant true and false is an
    *  opportunity to optimize expressions away entirely with no evaluation,
    *  if e.g. y is ConstantTrue in x(p) || y(p). Obviously this won't mix well
    *  with side effects. How enthusiastic can we be about punishing side effects
    *  before we kill the patient?
    */
  final class Predicate1[A] extends Fun1[A, Bool]()(Identity) {
    override val zero = ConstantFalse
    override val one  = ConstantTrue
    override def complement(f: R): R = f match {
      case `zero` => one
      case `one`  => zero
      case _      => super.complement(f)
    }
  }
  final class Predicate2[A, B] extends Fun2[A, B, Bool]()(Identity) {
    override val zero = ConstantFalse2
    override val one  = ConstantTrue2
    override def complement(f: R): R = f match {
      case `zero` => one
      case `one`  => zero
      case _      => super.complement(f)
    }
  }
}

trait StdAlgebra0 {
  implicit def identityBoolAlgebra: BoolAlgebra[Bool] = Algebra.Identity
}
trait StdAlgebra1 extends StdAlgebra0 {
  implicit def function1BoolAlgebra[A, B: BoolAlgebra]: BoolAlgebra[A => B]         = new Algebra.Fun1
  implicit def function2BoolAlgebra[A, B, C: BoolAlgebra]: BoolAlgebra[(A, B) => C] = new Algebra.Fun2
}
trait StdAlgebra extends StdAlgebra1 {
  implicit def predicate1BoolAlgebra[A]: BoolAlgebra[ToBool[A]]         = new Algebra.Predicate1[A]
  implicit def predicate2BoolAlgebra[A, B]: BoolAlgebra[(A, B) => Bool] = new Algebra.Predicate2[A, B]
}
object StdAlgebra extends StdAlgebra
