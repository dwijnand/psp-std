package psp
package tests

import api._, std._, all._

abstract class Laws[A : Eq] {
  def associative(f: BinOp[A]): Forall3[A]               = (a, b, c) => f(a, f(b, c)) === f(f(a, b), c)
  def distributive(f: BinOp[A], g: BinOp[A]): Forall3[A] = (a, b, c) => f(a, g(b, c)) === g(f(a, b), f(a, c))
  def commutative(f: BinOp[A]): Forall2[A]               = (a, b)    => f(a, b) === f(b, a)
  def absorption(f: BinOp[A], g: BinOp[A]): Forall2[A]   = (a, b)    => f(a, g(a, b)) === a
  def identity(f: BinOp[A], id: A): Forall1[A]           = a         => f(a, id) === a
  def idempotence(f: BinOp[A]): Forall1[A]               = a         => f(a, a) === a
}
abstract class RelationLaws[A] {
  def reflexive(f: EqRelation[A]): Forall1[A]     = a => f(a, a)
  def transitive(f: EqRelation[A]): Forall3[A]    = (a, b, c) => (f(a, b) && f(b, c)) ==> f(a, c)
  def symmetric(f: EqRelation[A]): Forall2[A]     = (a, b) => f(a, b) === f(b, a)
  def antisymmetric(f: EqRelation[A]): Forall2[A] = (a, b) => f(a, b) =!= f(b, a)
}
abstract class AlgebraLaws[A : Eq : BoolAlgebra] extends Laws[A] {
  def complement(f: BinOp[A], id: A): Forall1[A] = a => f(a, !a) === id
}

trait GenTransform[CC[X], A] {
  def self: CC[A]
  def transform[B](f: Gen[A] => Gen[B]): CC[B]

  def ^^^[B](x: B): CC[B]                                               = transform(_ => Gen const x)
  def ^^[B](f: A => B): CC[B]                                           = transform(_ map f)
  def ?(p: A => Bool): CC[A]                                            = transform(_ filter p)
  def >>[B](f: A => Gen[B]): CC[B]                                      = transform(_ flatMap f)
  def +^^[B](f: A => B): CC[A->B]                                       = transform(_ map (x => x -> f(x)))
  def ^?[B](pf: A ?=> B): CC[B]                                         = transform(_ collect pf)
  def *(n: Int): CC[Vec[A]]                                             = transform(gen.directOfN(n, _))
  def *(r: Gen[Int]): CC[Vec[A]]                                        = transform(g => r flatMap (g * _))
  def zip[B](h: Gen[B]): CC[A -> B]                                     = transform(_ flatMap (x => h map (x -> _)))
  def zipWith[B, C](h: Gen[B])(f: (A, B) => C): CC[C]                   = transform(g => (g, h) map f)
  def collect[B](pf: A ?=> B): CC[B]                                    = transform(_ suchThat pf.isDefinedAt map pf.apply)
  def collectN[B](n: Int)(pf: Each[A] ?=> B)(implicit z: Arb[A]): CC[B] = transform(g => gen.eachOfN(n, g) collect pf)
}

trait Assertions {
  def failed(msg: => String): Unit
  def assert(assertion: Boolean, msg: => String): Unit = if (!assertion) failed(msg)
}
object Assertions {
  private[this] var instance: Assertions = DefaultAssertions
  def using[A](x: Assertions)(assertion: => Boolean, msg: => String): Unit = {
    val saved = instance
    instance = x
    try instance.assert(assertion, msg) finally instance = saved
  }
  implicit object DefaultAssertions extends Assertions {
    def failed(msg: => String): Unit = assertionError(msg)
  }
}
object ImmediateTraceAssertions extends Assertions {
  def failed(msg: => String): Unit =
    new AssertionError(msg) |> (t => sideEffect(t.printStackTrace, throw t))
}

final case class Pint(x: Int)
