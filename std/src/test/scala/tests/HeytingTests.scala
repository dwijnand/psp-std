package psp
package tests

import org.scalacheck._, Prop.forAll
import std._, exp._, StdAlgebra._

class HeytingTests {
  def checkLaws[A](implicit bz: BoolAlgebra[A], az: Arbitrary[A], ez: Eq[A]): Prop = {
    import bz._
    val law = new AlgebraLaws[A] { }

    (    forAll(law.associative(and))
      && forAll(law.associative(or))
      && forAll(law.commutative(and))
      && forAll(law.commutative(or))
      && forAll(law.distributive(and, or))
      && forAll(law.distributive(or, and))
      && forAll(law.absorption(and, or))
      && forAll(law.absorption(or, and))
      && forAll(law.identity(or, zero))
      && forAll(law.identity(and, one))
      && forAll(law.complement(or, one))
      && forAll(law.complement(and, zero))
    )
  }

  @Test
  def identityAlgebra(): Unit = checkLaws[Bool].check

  @Test
  def pred1Algebra(): Unit    = {
    implicit def eqFunction: Eq[Char => Bool] = functionRelation[Char, Bool]
    checkLaws[Char => Bool].check
  }

  /** Stopped compiling with scalacheck 1.13.1.
  @Test
  def pred2Algebra(): Unit = {
    implicit def eqFunction: Eq[(Nth->Index) => Bool] = functionRelation[(Nth->Index), Bool]
    checkLaws[(Nth->Index) => Bool].check
  }
  **/
}
