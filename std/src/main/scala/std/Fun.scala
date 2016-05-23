package psp
package std

import all._, Fun._

/** When you come down to it,
  *
  *  A function is an unrestricted K => V.
  *  A partial function is K => V augmented with a predicate.
  *  A predicate is a function which requires V=Bool.
  *
  *  An intensional set is a predicate.
  *  An extensional set is a view augmented with an equality relation.
  *  An extensional set is also a map which requires V=Bool.
  *
  *  A map is a function augmented with an extensional set of keys.
  */
final case class Pmap[A, +B](keySet: Pset[A], lookup: Fun[A, B]) {
  def apply(key: A): B              = lookup(key)
  def contains(x: A): Bool          = keySet contains x
  def keys: View[A]                 = keySet.basis
  def map[C](f: B => C): Pmap[A, C] = Pmap(keySet, lookup andThen f)
  def pairs: View[A -> B]           = zipped.pairs
  def values: View[B]               = keys map lookup.fn
  def zipped: Zip[A, B]             = zipMap(keys, lookup.fn)
}
final case class Pset[A](basis: View[A], table: HashFun[A])(implicit hz: Hash[A], ez: Eq[A]) {
  def map[B](f: A => B): Pmap[A, B]               = Pmap(this, Fun(f))
  def mapToSet[B : Eq : Hash](f: A => B): Pset[B] = basis map f toPset
  def contains(x: A): Bool                        = table(x.hash) exists (_ === x)
}

object Pset {
  def apply[A](xs: View[A])(implicit hz: Hash[A], ez: Eq[A]): Pset[A] = Pset(xs, xs.hashFun)
}

sealed abstract class Fun[-A, +B] { self =>
  final def apply(x: A): B = this match {
    case Opaque(g)       => g(x)
    case OrElse(u1, u2)  => if (u1 contains x) u1(x) else u2(x)
    case Defaulted(g, u) => if (u contains x) u(x) else g(x)
    case Filtered(_, u)  => u(x) // filter is checked at contains
    case AndThen(u1, u2) => u2(u1(x))
    case FiniteMap(pm)   => pm(x)
    case Const(x)        => x
  }
  final def contains(x: A): Bool = this match {
    case Opaque(_)       => true
    case Const(_)        => true
    case OrElse(u1, u2)  => (u1 contains x) || (u2 contains x)
    case Filtered(p, u)  => p(x) && (u contains x)
    case Defaulted(_, u) => u contains x
    case AndThen(u1, u2) => (u1 contains x) && (u2 contains u1(x))
    case FiniteMap(pm)   => pm contains x
  }
  def toPartial: A ?=> B = Fun.partial(contains, apply)
}

object Fun {
  final case class Opaque[-A, +B](f: A => B)                      extends Fun[A, B]
  final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])     extends Fun[A, B]
  final case class Filtered[-A, +B](p: ToBool[A], u: Fun[A, B])   extends Fun[A, B]
  final case class OrElse[-A, +B](f: Fun[A, B], g: Fun[A, B])     extends Fun[A, B]
  final case class AndThen[-A, B, +C](f: Fun[A, B], g: Fun[B, C]) extends Fun[A, C]
  final case class FiniteMap[A, +B](pm: Pmap[A, B])               extends Fun[A, B]
  final case class Const[A](value: A)                             extends Fun[Any, A]
  // final case class Kleisli[F[+X], -A, +B](f: A => F[B])           extends Fun[A, F[B]]

  class Partial[-A, +B](p: ToBool[A], f: A => B) extends (A ?=> B) {
    def isDefinedAt(x: A): Bool = p(x)
    def apply(x: A): B          = f(x)
    def unapply(x: A): Opt[B]   = cond(p(x), some(f(x)), none())
  }

  def apply[A, B](f: A => B): Opaque[A, B] = Opaque(f)
  def const[A](value: A): Const[A]         = Const(value)

  def partial[A, B](p: ToBool[A], f: A => B): Partial[A, B] = new Partial(p, f)
  def partial[A, B](pf: A ?=> B): Partial[A, B] = pf match {
    case x: Partial[A, B] => x
    case _                => partial(pf.isDefinedAt, pf.apply)
  }
}
