package psp
package std
package views

import exp._
import all.{ PspAtomicOps }

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
final case class Take(n: Atomic)       extends Op0
final case class TakeRight(n: Atomic)  extends Op0
final case object Reverse              extends Op0

final case class Filter[A](p: ToBool[A])        extends Op1[A]
final case class TakeWhile[A](p: ToBool[A])     extends Op1[A]
final case class DropWhile[A](p: ToBool[A])     extends Op1[A]

final case class Mapped[A, B](f: A => B)        extends Op2[A, B]
final case class FlatMap[A, B](f: A => View[B]) extends Op2[A, B]
final case class Collect[A, B](pf: A ?=> B)     extends Op2[A, B]

object Run {
  def apply[C](xs: View[C])(f: ToUnit[C]): Unit = xs match {
    case IdView(xs)              => xs foreach f
    case Joined(xs, ys)          => apply(xs)(f); apply(ys)(f)
    case View0(xs, Reverse)      => apply(View(Makes reversed xs.toVec))(f)
    case View2(xs, Mapped(g))    => apply(xs)(g andThen f)
    case View2(xs, FlatMap(g))   => apply(xs)(x => g(x) foreach f)
    case View1(xs, Filter(p))    => apply(xs)(x => if (p(x)) f(x))
    case View2(xs, Collect(pf))  => apply(xs)(x => if (pf isDefinedAt x) f(pf(x)))
    case SlicedView(xs, range)   => ll.foreachSlice(xs, range, f)
    case View1(xs, TakeWhile(p)) => ll.foreachTakeWhile(xs, f, p)
    case View1(xs, DropWhile(p)) => ll.foreachDropWhile(xs, f, p)
    case View0(xs, DropRight(n)) => ll.foreachDropRight(xs, f, n)
    case View0(xs, TakeRight(n)) => ll.foreachTakeRight(xs, f, min(n, xs.size.preciseOrMaxLong))
    case View0(xs, Drop(n))      => ll.foreachSlice(xs, Interval(n.getLong) map Index, f)
    case View0(xs, Take(n))      => ll.foreachSlice(xs, n.indices, f)
  }
}
