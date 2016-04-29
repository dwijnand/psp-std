package psp
package ext

trait Spire {
  type AdditiveMonoid[A]          = spire.algebra.AdditiveMonoid[A]
  // type AdditiveSemigroup[A]       = spire.algebra.AdditiveSemigroup[A]
  type BooleanAlgebra[R]          = spire.algebra.Bool[R]
  // type Interval[A]                = spire.math.Interval[A]
  // type Monoid[A]                  = spire.algebra.Monoid[A]
  type MultiplicativeMonoid[A]    = spire.algebra.MultiplicativeMonoid[A]
  // type MultiplicativeSemigroup[A] = spire.algebra.MultiplicativeSemigroup[A]
  // type Natural                    = spire.math.Natural
  // type Rational                   = spire.math.Rational
  type SafeLong                   = spire.math.SafeLong
  type UInt                       = spire.math.UInt

  // val Interval = spire.math.Interval
  // val Rational = spire.math.Rational
  val SafeLong = spire.math.SafeLong
}
