package psp
package std

import api._, all._, Fun._

trait ExSet[A] extends Any {
  def size: Precise
  def basis: View[A]
  def equiv: Hash[A]
  def apply(x: A): Bool

  private implicit def heq: Hash[A] = equiv

  def toEach: Each[A] = basis.distinct.force
}

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
  def toPartial = new Partial(isDefinedAt, apply)
}

class Partial[-A, +B](p: ToBool[A], f: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A): Boolean = p(x)
  def apply(x: A): B             = f(x)
}

object Fun {
  final case class Opaque[-A, +B](f: A => B)                      extends Fun[A, B]
  final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])     extends Fun[A, B]
  final case class FilterIn[-A, +B](p: A => Bool, u: Fun[A, B])   extends Fun[A, B]
  final case class OrElse[-A, +B](f: Fun[A, B], g: Fun[A, B])     extends Fun[A, B]
  final case class AndThen[-A, B, +C](f: Fun[A, B], g: Fun[B, C]) extends Fun[A, C]
  final case class FiniteFun[A, +B](keys: ExSet[A], f: Fun[A, B]) extends Fun[A, B] {
    def zipped: Zip[A, B] = zipMap(keys.toEach, f)
  }

  def apply[A, B](f: A => B): Opaque[A, B]                          = Opaque(f)
  def fromMap[A : Eq, B](xs: sciMap[A, B]): FiniteFun[A, B]         = FiniteFun(xs.keys.toExSet, Fun(xs apply _))
  def finite[A, B](keys: ExSet[A], lookup: A => B): FiniteFun[A, B] = FiniteFun(keys, Fun(lookup))
}

object Partial {
  def apply[A, B](pf: A ?=> B): Partial[A, B] = pf match {
    case x: Partial[A, B] => x
    case _                => new Partial(pf.isDefinedAt, pf.apply)
  }
  def apply[A, B](p: ToBool[A], f: A => B): Partial[A, B] = new Partial(p, f)
}
