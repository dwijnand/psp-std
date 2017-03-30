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
  def zipped: Zip[A, B]             = zipMap(keys)(lookup.fn)
}
final case class Pset[A](basis: View[A], table: HashFun[A])(implicit hz: Hash[A], ez: Eq[A]) {
  def apply(x: A): Bool                         = this contains x
  def map[B](f: A => B): Pmap[A, B]             = Pmap(this, Fun(f))
  def mapToSet[B: Eq: Hash](f: A => B): Pset[B] = basis map f toPset
  def contains(x: A): Bool                      = table(x.hash) exists (_ === x)
}

object Pset {
  def apply[A](xs: View[A])(implicit hz: Hash[A], ez: Eq[A]): Pset[A] = Pset(xs, hashFun(xs))
}

class ExtractorCombine[A, B](Once: Extractor1[A, A -> B], combine: BinOp[B]) {
  def unapply(x: A): Opt[A -> B] = x match {
    case Once(Once(prev, a1), a2) => some(prev -> combine(a1, a2))
    case _                        => none()
  }
}
class ExtractorBool[A](p: ToBool[A]) {
  def unapply(x: A): Bool = p(x)
}
class Extractor1[A, B](pf: A ?=> B) {
  def map[C](f: B => C): Extractor1[A, C] = new Extractor1(pf andThen f)
  def unapply(x: A): Opt[B]               = pf lift x
}

sealed abstract class Fun[-A, +B] { self =>
  def undefined(x: A): Nothing = abort(any"$this undefined at $x")

  final def apply(x: A): B = this match {
    case AndThen(f, g)    => g(f(x))
    case Const(x)         => x
    case Filtered(p, u)   => if (p(x)) u(x) else undefined(x)
    case FiniteMap(ks, u) => if (ks contains x) u(x) else undefined(x)
    case Labeled(_, u)    => u(x)
    case Opaque(g)        => g(x)
    case OrElse(f, g)     => if (f contains x) f(x) else g(x)
  }
  final def contains(x: A): Bool = this match {
    case AndThen(f, g)    => f contains x
    case Const(_)         => true
    case Filtered(p, u)   => p(x) && (u contains x)
    case FiniteMap(ks, _) => ks contains x
    case Labeled(_, u)    => u contains x
    case Opaque(_)        => true
    case OrElse(f, _)     => f contains x
  }

  final def toFunction: ScalaFun.M[A, B] = this match {
    case Opaque(f)     => ScalaFun(f)
    case Labeled(_, f) => f.toFunction
    case x @ Const(_)  => x.fn
    case _             => ScalaFun(apply)
  }

  final def toPartial: ScalaFun.Partial[A, B] = ScalaFun.Partial(contains, toFunction)
}

object Fun {
  final case class AndThen[-A, B, +C](f: Fun[A, B], g: B => C)   extends Fun[A, C]
  final case class Const[+A](value: A)                           extends Fun[Any, A]
  final case class Filtered[-A, +B](p: ToBool[A], u: Fun[A, B])  extends Fun[A, B]
  final case class FiniteMap[A, +B](keys: Pset[A], u: Fun[A, B]) extends Fun[A, B]
  final case class Labeled[-A, +B](to_s: String, u: Fun[A, B])   extends Fun[A, B] with ShowSelf
  final case class Opaque[-A, +B](f: A => B)                     extends Fun[A, B]
  final case class OrElse[-A, +B](f: Fun[A, B], g: A => B)       extends Fun[A, B]
  // final case class Kleisli[F[+X], -A, +B](f: A => F[B])           extends Fun[A, F[B]]

  def apply[A, B](f: A => B): Opaque[A, B] = Opaque(f)
  def const[A](value: A): Const[A]         = Const(value)
}

object Pred {
  def True[A]: ScalaPred.M[A]  = ScalaPred.True
  def False[A]: ScalaPred.M[A] = ScalaPred.False
}

/** Scala function wrappers, trying to get better strings and equality.
  *  Predicates exist to preserve equality of constant true and constant false,
  *  so the view optimizer can respond appropriately.
  */
object ScalaPred {
  implicit def scalaPredToPsp[A](p: ToBool[A]): M[A] = ScalaPred(p)

  sealed abstract class M[-A](f: ToBool[A]) extends ToBool[A] with InheritedHashEq with ShowSelf {
    final def apply(x: A): Bool = f(x)
    def to_s: String = this match {
      case Label(s, _)     => s
      case False           => "<F>"
      case True            => "<T>"
      case And(p, q)       => any"$p && $q"
      case Or(p, q)        => any"$p || $q"
      case Not(p)          => any"!$p"
      case Id(p: ShowSelf) => p.to_s
      case Id(_)           => "?"
    }
  }
  final object False                                 extends M[Any](_ => false)
  final object True                                  extends M[Any](_ => true)
  final case class And[-A](p: M[A], q: M[A])         extends M[A](x => p(x) && q(x))
  final case class Or[-A](p: M[A], q: M[A])          extends M[A](x => p(x) || q(x))
  final case class Not[-A](p: M[A])                  extends M[A](x => !p(x))
  final case class Id[-A](p: ToBool[A])              extends M[A](p)
  final case class Label[-A](label: String, p: M[A]) extends M[A](p)

  def apply[A](p: ToBool[A]): M[A] = p match {
    case p: M[A] => p
    case _       => Id(p)
  }
}
object ScalaFun {
  type LeftFold[R, A]  = (R, A) => R
  type RightFold[A, R] = (A, R) => R

  object Identity {
    private lazy val IdFun: M[Any, Any] = Label("id", Id((x: Any) => x))

    def apply[A](): M[A, A]         = cast(IdFun)
    def unapply[A](f: A => A): Bool = f eq IdFun
  }
  def apply[A, B](f: A => B): M[A, B] = f match {
    case f: M[A, B] => f
    case _          => Id(f)
  }
  def partial[A, B](pf: A ?=> B): Partial[A, B] = pf match {
    case x: Partial[A, B] => x
    case _                => Partial(pf.isDefinedAt, apply(pf))
  }
  def const[A](x: A): Const[A]                        = Const(x)
  def compose[A, B, C](f: A => B, g: B => C): M[A, C] = Compose(apply(f), apply(g))

  sealed abstract class M[-A, +B](u: A => B) extends (A => B) with InheritedHashEq with ShowSelf {
    def labeled(label: String): M[A, B] = Label(label, this)
    def apply(x: A): B                  = u(x)
    def to_s: String = this match {
      case Compose(f, g)   => any"$f andThen $g"
      case Label(s, _)     => s
      case Const(x)        => any"_=>$x"
      case Id(x: ShowSelf) => x.to_s
      case _               => "<fn>"
    }
  }
  final case class Compose[-A, B, +C](f: M[A, B], g: M[B, C]) extends M(f andThen g)
  final case class Id[A, B](f: A => B)                        extends M(f)
  final case class Const[A](value: A)                         extends M[Any, A](_ => value)
  final case class Label[A, B](label: String, f: M[A, B])     extends M(f)
  final case class Partial[-A, +B](p: ToBool[A], f: M[A, B]) extends M(f) with (A ?=> B) with InheritedHashEq with ShowSelf {
    def isDefinedAt(x: A): Bool = p(x)
    def unapply(x: A): Opt[B]   = cond(p(x), some(f(x)), none())
  }

}
