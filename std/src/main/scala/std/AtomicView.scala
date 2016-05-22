package psp
package std

import api._, all._, StdShow._
import View._

trait View[+A] extends Any with Foreach[A] {
  def size = Size.Unknown
}

object View {
  final case class Joined[A, R](prev: RView[A, R], ys: View[A])            extends CView[A, A, R](_ + ys.size)
  final case class Filtered[A, R](prev: RView[A, R], p: ToBool[A])         extends CView[A, A, R](_.atMost)
  final case class Dropped[A, R](prev: RView[A, R], n: Precise)            extends CView[A, A, R](_ - n)
  final case class DroppedR[A, R](prev: RView[A, R], n: Precise)           extends CView[A, A, R](_ - n)
  final case class Taken[A, R](prev: RView[A, R], n: Precise)              extends CView[A, A, R](Size.min(n, _))
  final case class TakenR[A, R](prev: RView[A, R], n: Precise)             extends CView[A, A, R](Size.min(n, _))
  final case class TakenWhile[A, R](prev: RView[A, R], p: ToBool[A])       extends CView[A, A, R](_.atMost)
  final case class DropWhile[A, R](prev: RView[A, R], p: ToBool[A])        extends CView[A, A, R](_.atMost)
  final case class Mapped[A, B, R](prev: RView[A, R], f: A => B)           extends CView[A, B, R](x => x)
  final case class FlatMapped[A, B, R](prev: RView[A, R], f: A => View[B]) extends CView[A, B, R](x => cond(x.isZero, x, Size.Unknown))
  final case class Collected[A, B, R](prev: RView[A, R], pf: A ?=> B)      extends CView[A, B, R](_.atMost)
  final case class Reversed[A, R](prev: RView[A, R])                       extends CView[A, A, R](x => x)

  def unapplySeq[A](xs: View[A]): Some[scSeq[A]] = Some(xs.seq)
}
object CView {
  def unapply[A, B, R](xs: CView[A, B, R]): Some[RView[A, R] -> ToSelf[Size]] = Some(xs.prev -> xs.sizeEffect)
}

object RunView {
  def loop[C](xs: View[C])(f: C => Unit): Unit = {
    type Pred = (ToBool[C] @unchecked) // silencing patmat warnings
    xs match {
      case Reversed(Reversed(xs))            => loop(xs)(f)
      case Reversed(xs)                      => loop(xs.toVec.reverse)(f)
      case FlatSlice(Mapped(prev, g), range) => sliced(prev, range, g andThen f)
      case FlatSlice(xs, range)              => sliced(xs, range, f)
      case Mapped(xs, g)                     => loop(xs)(g andThen f)
      case FlatMapped(xs, g)                 => loop(xs)(x => g(x) foreach f)
      case Filtered(xs, p: Pred)             => loop(xs)(x => if (p(x)) f(x))
      case TakenWhile(xs, p: Pred)           => ll.foreachTakeWhile(xs, f, p)
      case DropWhile(xs, p: Pred)            => ll.foreachDropWhile(xs, f, p)
      case Collected(xs, pf)                 => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
      case Joined(xs, ys)                    => loop(xs)(f); loop(ys)(f)
      case DroppedR(xs, Precise(0))          => loop(xs)(f)
      case TakenR(xs, Precise(0))            => ()
      case DroppedR(xs, n)                   => ll.foreachDropRight(xs, f, n)
      case TakenR(xs, n)                     => ll.foreachTakeRight(xs, f, n)
      case Dropped(xs, n: Precise)           => ll.foreachSlice(xs, n.getLong indexUntil MaxLong, f)
      case Taken(xs, n: Precise)             => ll.foreachSlice(xs, n.indices, f)
      case xs: View[_]                       => xs foreach f
      case _                                 => abort(doc"Unexpected view class ${ classNameOf(xs) }")
    }
  }

  def sliced[A, R](xs: RView[A, R], range: VdexRange, f: ToUnit[A]): Unit = xs match {
    case xs: CView[_, _, _] => loop(xs)(f)
    case _                  => ll.foreachSlice(xs, range, f)
  }

  object FlatSlice {
    def unapply[A, R](xs: RView[A, R]): Option[(RView[A, R], VdexRange)] = xs match {
      case Mapped(xs, f)           => unapply(xs) map { case (xs, range) => (xs map f, range) }
      case Dropped(xs, Size.Zero)  => unapply(xs)
      case DroppedR(xs, Size.Zero) => unapply(xs)
      case Taken(xs, Size.Zero)    => Some(emptyValue)
      case TakenR(xs, Size.Zero)   => Some(emptyValue)
      case DroppedR(xs, n)         => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
      case TakenR(xs, n)           => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
      case Dropped(xs, n)          => unapply(xs) map { case (xs, range) => (xs, range drop n) }
      case Taken(xs, n)            => unapply(xs) map { case (xs, range) => (xs, range take n) }
      case _                       => xs.size matchIf { case x: Precise => some(xs -> x.indices) }
    }
  }
}

sealed trait RView[A, R] extends View[A] {
  type MapTo[X] = RView[X, R]
  type This     = MapTo[A]

  def xs: RView[A, R] = this
  def foreach(f: ToUnit[A]): Unit = this match {
    case IdView(underlying) => underlying foreach f
    case _                  => RunView.loop(this)(f)
  }

  def collect[B](pf: A ?=> B): MapTo[B]     = Collected(this, pf)
  def drop(n: Precise): This                = Dropped(this, n)
  def dropRight(n: Precise): This           = DroppedR(this, n)
  def dropWhile(p: ToBool[A]): This         = DropWhile(this, p)
  def flatMap[B](f: A => View[B]): MapTo[B] = FlatMapped(this, f)
  def join(that: View[A]): View[A]          = Joined(this, that)
  def map[B](f: A => B): MapTo[B]           = Mapped(this, f)
  def take(n: Precise): This                = Taken(this, n)
  def takeRight(n: Precise): This           = TakenR(this, n)
  def takeWhile(p: ToBool[A]): This         = TakenWhile(this, p)
  def withFilter(p: ToBool[A]): This        = Filtered(this, p)
  def reverseView: This                     = Reversed(this)

  def build(implicit z: Makes[A, R]): R = xs.force[R]
}
final case class IdView[A, R](underlying: Foreach[A]) extends RView[A, R] {
  override def size = underlying match {
    case xs: Direct[_] => xs.size
    case _: Indexed[_] => Infinite
    case _             => super.size
  }
}

sealed abstract class CView[A, B, R](val sizeEffect: ToSelf[Size]) extends RView[B, R] {
  def prev: RView[A, R]
}
