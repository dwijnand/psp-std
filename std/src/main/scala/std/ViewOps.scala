package psp
package std
package views

import exp._
import Size.Zero
import all.{ emptyView, HasHeytingOps, predicate1BoolAlgebra }

sealed trait Op
sealed trait Op0 extends Op
sealed trait Op1[A] extends Op
sealed trait Op2[A, B] extends Op

final case class Joined[A, R](ls: RView[A, R], rs: RView[A, R])   extends RView[A, R]
final case class View0[A, R](prev: RView[A, R], op: Op0)          extends RView[A, R]
final case class View1[A, R](prev: RView[A, R], op: Op1[A])       extends RView[A, R]
final case class View2[Z, A, R](prev: RView[Z, R], op: Op2[Z, A]) extends RView[A, R]

final case class Drop(n: Precise)      extends Op0
final case class DropRight(n: Precise) extends Op0
final case class Take(n: Precise)      extends Op0
final case class TakeRight(n: Precise) extends Op0
final case object Reverse              extends Op0

final case class Filter[A](p: ToBool[A])        extends Op1[A]
final case class TakeWhile[A](p: ToBool[A])     extends Op1[A]
final case class DropWhile[A](p: ToBool[A])     extends Op1[A]

final case class Mapped[A, B](f: A => B)        extends Op2[A, B]
final case class FlatMap[A, B](f: A => View[B]) extends Op2[A, B]
final case class Collect[A, B](pf: A ?=> B)     extends Op2[A, B]

object Run {
  def unoptimized[C](xs: View[C])(f: ToUnit[C]): Unit = xs match {
    case IdView(xs)              => xs foreach f
    case Joined(xs, ys)          => unoptimized(xs)(f); unoptimized(ys)(f)
    case View0(xs, Reverse)      => unoptimized(View(Makes reversed xs.toVec))(f)
    case View2(xs, Mapped(g))    => unoptimized(xs)(g andThen f)
    case View2(xs, FlatMap(g))   => unoptimized(xs)(x => g(x) foreach f)
    case View1(xs, Filter(p))    => unoptimized(xs)(x => if (p(x)) f(x))
    case View2(xs, Collect(pf))  => unoptimized(xs)(x => if (pf isDefinedAt x) f(pf(x)))
    case SlicedView(xs, range)   => ll.foreachSlice(xs, range, f)
    case View1(xs, TakeWhile(p)) => ll.foreachTakeWhile(xs, f, p)
    case View1(xs, DropWhile(p)) => ll.foreachDropWhile(xs, f, p)
    case View0(xs, DropRight(n)) => ll.foreachDropRight(xs, f, n)
    case View0(xs, TakeRight(n)) => ll.foreachTakeRight(xs, f, min(n, xs.size.preciseOrMaxLong))
    case View0(xs, Drop(n))      => ll.foreachSlice(xs, Interval(n.getLong) map Index, f)
    case View0(xs, Take(n))      => ll.foreachSlice(xs, n.indices, f)
  }

  def optimized[C](xs: View[C])(f: ToUnit[C]): Unit = xs match {
    case Optimize(xs) => optimized(xs)(f)
    case _            => unoptimized(xs)(f)
  }
}
object Optimize {
  def apply[A, R](xs: RView[A, R]): RView[A, R] = xs match {
    case Optimize(xs) => View(xs)
    case _            => xs
  }
  private def finish[A, R](xs: RView[A, R]): Some[RView[A, R]] = Some(apply(xs))

  def unapply[A, R](xs: RView[A, R]) = xs match {
    case View0(_, Take(Zero) | TakeRight(Zero))                        => some(emptyValue[RView[A, R]])
    case View1(_, TakeWhile(ConstantFalse) | DropWhile(ConstantTrue))  => some(emptyValue[RView[A, R]])
    case View0(xs, Drop(Zero) | DropRight(Zero))                       => finish(xs)
    case View1(xs, TakeWhile(ConstantTrue) | DropWhile(ConstantFalse)) => finish(xs)

    case Joined(EmptyView(), v2) => finish(v2)
    case Joined(v1, EmptyView()) => finish(v1)

    // Looks like there's no way to include these...
    // they would require us to write the return type of this method.
    // Good luck.
    //
    // case Joined(Optimize(v1), v2) => finish(Joined(v1, v2))
    // case Joined(v1, Optimize(v2)) => finish(Joined(v1, v2))

    case View2(View2(xs, Collect(pf)), Mapped(f)) => finish(xs collect (pf andThen f))
    case View2(View1(xs, Filter(p)), Collect(pf)) => finish(xs collect Fun.partial(p && pf.isDefinedAt, pf))
    case View2(View1(xs, Filter(p)), Mapped(f))   => finish(xs collect Fun.partial(p, f))
    case View2(View2(xs, Mapped(f)), FlatMap(g))  => finish(xs flatMap (f andThen g))

    // !!! The seductive appearing FlatMap case is type-incorrect.
    // Scala lets it through with no warnings and then runs into a CCE later.
    case View0(View0(xs, Reverse), Reverse)             => finish(xs)
    case View2(View2(xs, Mapped(f)), Mapped(g))         => finish(xs map (f andThen g))
    case View1(View1(xs, Filter(p)), Filter(q))         => finish(xs withFilter p && q)
    case View0(View0(xs, Drop(n1)), Drop(n2))           => finish(xs drop n1 + n2)
    case View0(View0(xs, DropRight(n1)), DropRight(n2)) => finish(xs dropRight n1 + n2)
    case View0(View0(xs, Take(n1)), Take(n2))           => finish(xs take min(n1, n2))
    case View0(View0(xs, TakeRight(n1)), TakeRight(n2)) => finish(xs takeRight min(n1, n2))
    case View1(View1(xs, TakeWhile(p)), TakeWhile(q))   => finish(xs takeWhile p && q)
    case View1(View1(xs, DropWhile(p)), DropWhile(q))   => finish(xs dropWhile p || q)
    case _                                              => none()
  }
}
