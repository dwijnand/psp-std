package psp
package std

import api._, all._, StdShow._

sealed abstract class AtomicView[A, Repr] extends IBaseView[A, Repr] {
  type This <: AtomicView[A, Repr]
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit
}

object FlattenSlice {
  def unapply[A, Repr](xs: BaseView[A, Repr]): Option[(BaseView[A, Repr], VdexRange)] = xs match {
    case xs: DirectView [_, _]   => Some(xs -> xs.size.indices)
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

final class LinearView[A, Repr](underlying: Each[A]) extends AtomicView[A, Repr] {
  type This = LinearView[A, Repr]

  def size: Size                          = underlying.size
  @inline def foreach(f: A => Unit): Unit = underlying foreach f
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit = {
    if (range.isEmpty) return
    val start = range.head.indexValue
    val last  = range.last.indexValue
    var current = 0L

    underlying foreach { x =>
      if (start <= current && current <= last) f(x)
      current += 1
      if (current > last) return
    }
  }
}

final class DirectView[A, Repr](underlying: Direct[A]) extends AtomicView[A, Repr] {
  type This = DirectView[A, Repr]

  def size: Precise                                      = underlying.size
  def elemAt(i: Vdex): A                                 = underlying elemAt i
  def foreach(f: A => Unit): Unit                        = size.indices foreach (i => f(elemAt(i)))
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit = size.indices slice range foreach (i => f(elemAt(i)))
}

sealed trait BaseView[+A, Repr] extends AnyRef with View[A] {
  def foreach(f: A => Unit): Unit
  def toEach: Each[A] = Each(foreach)

  type This <: BaseView[A, Repr]
  type MapTo[+X] = BaseView[X, Repr]

  def xs: this.type = this

  final def collect[B](pf: A ?=> B): MapTo[B]        = Collected(this, pf)
  final def drop(n: Precise): MapTo[A]               = Dropped(this, n)
  final def dropRight(n: Precise): MapTo[A]          = DroppedR(this, n)
  final def dropWhile(p: ToBool[A]): MapTo[A]        = DropWhile(this, p)
  final def flatMap[B](f: A => Foreach[B]): MapTo[B] = FlatMapped(this, f)
  final def map[B](f: A => B): MapTo[B]              = Mapped(this, f)
  final def take(n: Precise): MapTo[A]               = Taken(this, n)
  final def takeRight(n: Precise): MapTo[A]          = TakenR(this, n)
  final def takeWhile(p: ToBool[A]): MapTo[A]        = TakenWhile(this, p)
  final def withFilter(p: ToBool[A]): MapTo[A]       = Filtered(this, p)

  final def force[That](implicit z: Builds[A, That]): That = z build this
  final def build(implicit z: Builds[A, Repr]): Repr       = force[Repr]
}

sealed trait IBaseView[A, Repr] extends BaseView[A, Repr] with View[A] with ConversionsMethods[A] {
  final def join(that: View[A]): View[A] = Joined(this, that)
}

sealed abstract class CompositeView[A, B, Repr](val sizeEffect: ToSelf[Size]) extends IBaseView[B, Repr] {
  def prev: View[A]
  def size = sizeEffect(prev.size)

  final def foreach(f: B => Unit): Unit = {
    def loop[C](xs: View[C])(f: C => Unit): Unit = {
      type Pred = (ToBool[C] @unchecked) // silencing patmat warnings
      xs match {
        case FlattenSlice(xs, range) => foreachSlice(xs, range, f)
        case Mapped(xs, g)           => loop(xs)(g andThen f)
        case FlatMapped(xs, g)       => loop(xs)(x => g(x) foreach f)
        case Filtered(xs, p: Pred)   => loop(xs)(x => if (p(x)) f(x))
        case TakenWhile(xs, p: Pred) => ll.foreachTakeWhile(xs, f, p)
        case DropWhile(xs, p: Pred)  => ll.foreachDropWhile(xs, f, p)
        case Collected(xs, pf)       => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
        case Joined(xs, ys)          => loop(xs)(f); loop(ys)(f)
        case DroppedR(xs, Finite(0)) => loop(xs)(f)
        case TakenR(xs, Finite(0))   => ()
        case DroppedR(xs, n)         => ll.foreachDropRight(xs, f, n)
        case TakenR(xs, n)           => ll.foreachTakeRight(xs, f, n)
        case Dropped(xs, Finite(n))  => foreachSlice(xs, n until MaxLong map Index, f)
        case Taken(xs, n: Precise)   => foreachSlice(xs, n.indices, f)
        case xs: View [_]            => xs foreach f
        case _                       => abort(pp"Unexpected view class ${ classNameOf(xs) }")
      }
    }
    if (!size.isZero) loop(this)(f)
  }
  private def foreachSlice[A](xs: Foreach[A], range: VdexRange, f: A => Unit): Unit = xs match {
    case Mapped(prev, g) => foreachSlice(prev, range, g andThen f)
    case _               => ll.foreachSlice(xs, range, f)
  }
}

final case class Joined[A, Repr](prev: IBaseView[A, Repr], ys: View[A]) extends CompositeView[A, A, Repr](_ + ys.size)
final case class Filtered[A, Repr](prev: BaseView[A, Repr], p: ToBool[A]) extends CompositeView[A, A, Repr](_.atMost)
final case class Dropped[A, Repr](prev: BaseView[A, Repr], n: Precise) extends CompositeView[A, A, Repr](_ - n)
final case class DroppedR[A, Repr](prev: BaseView[A, Repr], n: Precise) extends CompositeView[A, A, Repr](_ - n)
final case class Taken[A, Repr](prev: BaseView[A, Repr], n: Precise) extends CompositeView[A, A, Repr](_ min n)
final case class TakenR[A, Repr](prev: BaseView[A, Repr], n: Precise) extends CompositeView[A, A, Repr](_ min n)
final case class TakenWhile[A, Repr](prev: BaseView[A, Repr], p: ToBool[A]) extends CompositeView[A, A, Repr](_.atMost)
final case class DropWhile[A, Repr](prev: BaseView[A, Repr], p: ToBool[A]) extends CompositeView[A, A, Repr](_.atMost)
final case class Mapped[A, B, Repr](prev: BaseView[A, Repr], f: A => B) extends CompositeView[A, B, Repr](x => x)
final case class FlatMapped[A, B, Repr](prev: BaseView[A, Repr], f: A => Foreach[B]) extends CompositeView[A, B, Repr](x => cond(x.isZero, x, Size.Unknown))
final case class Collected[A, B, Repr](prev: BaseView[A, Repr], pf: A ?=> B) extends CompositeView[A, B, Repr](_.atMost)
