package psp
package std

import api._, all._, StdEq._, StdShow._, Unsafe.inheritedShow
import lowlevel.CircularBuffer

sealed abstract class AtomicView[A, Repr] extends IBaseView[A, Repr] {
  type This <: AtomicView[A, Repr]
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit
}

object FlattenSlice {
  def unapply[A, Repr](xs: BaseView[A, Repr]): Option[(BaseView[A, Repr], VdexRange)] = xs match {
    case xs: DirectView[_, _]    => Some(xs -> xs.size.indices)
    case Mapped(xs, f)           => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case Dropped(xs, Size.Zero)  => unapply(xs)
    case DroppedR(xs, Size.Zero) => unapply(xs)
    case Taken(xs, Size.Zero)    => Some(emptyValue)
    case TakenR(xs, Size.Zero)   => Some(emptyValue)
    case DroppedR(xs, n)         => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, n)           => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, n)          => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, n)            => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case _                       => xs.size matchOpt { case x: Precise => xs -> x.indices }
  }
}

// final class StreamView[A, Repr](underlying: jStream[A]) extends AtomicView[A, Repr] {
//   type This = StreamView[A, Repr]
//   lazy val size: Precise = Size(underlying.count)

//   @inline def foreach(f: A => Unit): Unit                  = underlying.iterator foreach f
//   def foreachSlice(range: VdexRange)(f: A => Unit): Unit = underlying slice range foreach f
// }

final class LinearView[A, Repr](underlying: Each[A]) extends AtomicView[A, Repr] {
  type This = LinearView[A, Repr]

  def size: Size = underlying.size
  @inline def foreach(f: A => Unit): Unit = underlying foreach f
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit = {
    if (range.isEmpty) return
    val start   = range.head.indexValue
    val last    = range.last.indexValue
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

  def size: Precise                                        = underlying.size
  def elemAt(i: Vdex): A                                   = underlying elemAt i
  def foreach(f: A => Unit): Unit                          = size.indices foreach (i => f(elemAt(i)))
  def foreachSlice(range: VdexRange)(f: A => Unit): Unit = size.indices slice range foreach (i => f(elemAt(i)))
}

sealed trait BaseView[+A, Repr] extends AnyRef with View[A] with ops.ViewOps[A] {
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

sealed trait IBaseView[A, Repr] extends BaseView[A, Repr] with IView[A] {
  final def join(that: IView[A]): IView[A]    = Joined(this, that)
  final def span(p: ToBool[A]): Split[A]      = Split(takeWhile(p), dropWhile(p))
  final def partition(p: ToBool[A]): Split[A] = Split(withFilter(p), withFilter(!p))
}

sealed abstract class CompositeView[A, B, Repr](val description: Doc, val sizeEffect: ToSelf[Size]) extends IBaseView[B, Repr] {
  def prev: View[A]
  def size = sizeEffect(prev.size)

  final def foreach(f: B => Unit): Unit = {
    def loop[C](xs: View[C])(f: C => Unit): Unit = {
      type Pred = (ToBool[C] @unchecked) // silencing patmat warnings
      xs match {
        case FlattenSlice(xs, range)  => foreachSlice(xs, range, f)
        case Mapped(xs, g)            => loop(xs)(g andThen f)
        case FlatMapped(xs, g)        => loop(xs)(x => g(x) foreach f)
        case Filtered(xs, p: Pred)    => loop(xs)(x => if (p(x)) f(x))
        case TakenWhile(xs, p: Pred)  => foreachTakeWhile(xs, f, p)
        case DropWhile(xs, p: Pred)   => foreachDropWhile(xs, f, p)
        case Collected(xs, pf)        => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
        case Joined(xs, ys)           => loop(xs)(f) ; loop(ys)(f)
        case DroppedR(xs, n: Precise) => foreachDropRight(xs, f, n)
        case TakenR(xs, n: Precise)   => foreachTakeRight(xs, f, n)
        case Dropped(xs, Finite(n))   => foreachSlice(xs, n until MaxLong map Index, f)
        case Taken(xs, n: Precise)    => foreachSlice(xs, n.indices, f)
        case xs: View[_]              => xs foreach f
        case _                        => abort(pp"Unexpected view class ${xs.shortClass}")
      }
    }
    if (!size.isZero)
      loop(this)(f)
  }

  private def foreachTakeWhile[A](xs: Each[A], f: A => Unit, p: ToBool[A]): Int = {
    var taken = 0
    xs foreach { x =>
      if (p(x)) sideEffect(f(x), taken += 1)
      else return taken
    }
    taken
  }
  private def foreachDropWhile[A](xs: Each[A], f: A => Unit, p: ToBool[A]): Int = {
    var dropping = true
    var dropped  = 0
    xs foreach { x =>
      if (dropping && p(x)) dropped += 1
      else {
        if (dropping) dropping = false
        f(x)
      }
    }
    dropped
  }

  private def foreachTakeRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit = (
    if (n > 0)
      (CircularBuffer[A](n) ++= xs) foreach f
  )
  private def foreachDropRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit = (
    if (n.isZero)
      xs foreach f
    else
      xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) sideEffect(buf, f(buf push x)) else buf += x)
  )

  private def foreachSlice[A](xs: View[A], range: VdexRange, f: A => Unit): Unit = xs match {
    case Mapped(prev, g) => foreachSlice(prev, range, g andThen f)
    case _               => xs.foreachSlice(range)(f)
  }
}

final case class Joined [A,         Repr](prev: IBaseView[A, Repr], ys: IView[A])   extends CompositeView[A, A, Repr](pp"++ $ys",      _ + ys.size)
final case class Filtered    [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](pp"filter $p",   _.atMost)
final case class Dropped     [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"drop $n",     _ - n)
final case class DroppedR    [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"dropR $n",    _ - n)
final case class Taken       [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"take $n",     _ min n)
final case class TakenR      [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](pp"takeR $n",    _ min n)
final case class TakenWhile  [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](pp"takeW $p",    _.atMost)
final case class DropWhile   [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](pp"dropW $p",    _.atMost)
final case class Mapped      [A, B, Repr](prev: BaseView[A, Repr], f: A => B)       extends CompositeView[A, B, Repr](pp"map $f",      x => x)
final case class FlatMapped  [A, B, Repr](prev: BaseView[A, Repr], f: A => Foreach[B]) extends CompositeView[A, B, Repr](pp"flatMap $f",  x => if (x.isZero) x else Size.Unknown)
final case class Collected   [A, B, Repr](prev: BaseView[A, Repr], pf: A ?=> B)     extends CompositeView[A, B, Repr](pp"collect $pf", _.atMost)
