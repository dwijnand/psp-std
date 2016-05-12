package psp
package std

import api._, all._, Fun._

/** A richer function abstraction.
  *
  *  No way to avoid at least having apply as a member method if there's
  *  to be any hope of seeing these converted into scala.Functions.
  */
sealed abstract class Fun[-A, +B] { self =>

  final def apply(x: A): B = this match {
    case Opaque(g)       => g(x)
    case OrElse(u1, u2)  => if (u1 isDefinedAt x) u1(x) else u2(x)
    case Defaulted(g, u) => if (u isDefinedAt x) u(x) else g(x)
    case FilterIn(_, u)  => u(x) // filter is checked at isDefinedAt
    case AndThen(u1, u2) => u2(u1(x))
    case FiniteFun(_, g) => g(x)
  }
  final def isDefinedAt(x: A): Boolean = this match {
    case Opaque(_)        => true
    case OrElse(u1, u2)   => (u1 isDefinedAt x) || (u2 isDefinedAt x)
    case FilterIn(p, u)   => p(x) && (u isDefinedAt x)
    case Defaulted(_, u)  => u isDefinedAt x
    case AndThen(u1, u2)  => (u1 isDefinedAt x) && (u2 isDefinedAt u1(x))
    case FiniteFun(ks, _) => ks(x)
  }
  def toPartial: A ?=> B = new (A ?=> B) {
    def isDefinedAt(x: A) = self isDefinedAt x
    def apply(x: A)       = self(x)
  }
}

object Fun {
  final case class Opaque[-A, +B](f: A => B)                      extends Fun[A, B]
  final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])     extends Fun[A, B]
  final case class FilterIn[-A, +B](p: A => Bool, u: Fun[A, B])   extends Fun[A, B]
  final case class OrElse[-A, +B](f: Fun[A, B], g: Fun[A, B])     extends Fun[A, B]
  final case class AndThen[-A, B, +C](f: Fun[A, B], g: Fun[B, C]) extends Fun[A, C]
  final case class FiniteFun[A, +B](keys: ExSet[A], f: Fun[A, B]) extends Fun[A, B]

  def apply[A, B](f: A => B): Opaque[A, B]                          = Opaque(f)
  def fromMap[A : Eq, B](xs: sciMap[A, B]): FiniteFun[A, B]         = FiniteFun(xs.keys.toExSet, Fun(xs apply _))
  def finite[A, B](keys: ExSet[A], lookup: A => B): FiniteFun[A, B] = FiniteFun(keys, Fun(lookup))
}
