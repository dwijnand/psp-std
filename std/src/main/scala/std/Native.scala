package psp
package std

import all._, StdShow._

/** "Native" psp collections.
  */
sealed abstract class Plist[A] extends Each[A] {
  def ::(head: A): Pcons[A] = Pcons(head, this)
  @inline final def foreach(f: A => Unit): Unit = {
    @tailrec def loop(xs: Plist[A]): Unit = xs match {
      case Pcons(hd, tl) => f(hd); loop(tl)
      case _             =>
    }
    loop(this)
  }
}
final case class Pcons[A](head: A, tail: Plist[A]) extends Plist[A] {
  def size = Size.One.atLeast
}
final case object Pnil extends Plist[Nothing] {
  def size                           = _0
  def apply[A](): Plist[A]           = cast(this)
  def unapply[A](xs: Plist[A]): Bool = xs eq this
}

object Pstream {
  def empty[A] : Pstream[A] = Psnil()
  def apply[A](hd: => A, tl: => Pstream[A]): Pscons[A] = new Pscons(hd, tl)
  def unapply[A](x: Pstream[A]): Opt[A -> Pstream[A]] = x match {
    case x: Pscons[A] => some(x.head -> x.tail)
    case _            => none()
  }
}
sealed abstract class Pstream[+A] extends Each[A] {
  @inline final def foreach(f: A => Unit): Unit = {
    @tailrec def loop(xs: Pstream[A]): Unit = xs match {
      case Pstream(hd, tl) => f(hd); loop(tl)
      case _               =>
    }
    loop(this)
  }
}
final class Pscons[A](h: => A, t: => Pstream[A]) extends Pstream[A] {
  def size = Size.One.atLeast
  lazy val head = h
  lazy val tail = t
}
final case object Psnil extends Pstream[Nothing] {
  def size = _0
  def apply[A](): Pstream[A] = cast(this)
}

sealed class PunapplySeq[A](it: () => scIterator[A]) extends scSeq[A] {
  def iterator        = it()
  def apply(idx: Int) = iterator drop idx next
  def length: Int     = if (iterator.hasNext) MaxInt else 0
}

sealed abstract class Consecutive[+A] extends Indexed[A] with HasToS {
  type CC [X] <: Consecutive[X]

  def in: Interval
  def map[B](g: A => B): CC[B]
  def applyLong: Long => A
  def isAfter(n: Long): Bool

  def drop(n: Precise): Consecutive[A]
  def dropRight(n: Precise): Consecutive[A]
  def take(n: Precise): Consecutive.Closed[A]
  def takeRight(n: Precise): Consecutive.Closed[A]

  def prelude: Precise            = startLong.size
  def startLong: Long             = in.startLong
  def containsLong(n: Long): Bool = in contains n

  def take(n: Atomic): Consecutive[A]      = n.foldInfinite(this, take)
  def takeRight(n: Atomic): Consecutive[A] = n.foldInfinite(this, takeRight)

  def slice(start: Index, len: Precise): ClosedRange[A] = Consecutive(in.slice(start, len), applyLong)
  def slice(r: SliceRange): Consecutive[A]              = cond(r.isEmpty, Consecutive.empty[A], this drop r.head.excluding take r.size)
}

object Consecutive {
  private val Empty = new Closed[Nothing](Interval.empty, indexOutOfBoundsException)

  def empty[A]: Closed[A]                                    = Empty
  def apply[A](in: Interval.Closed, f: Long => A): Closed[A] = new Closed(in, f)
  def apply[A](in: Interval.Open, f: Long => A): Open[A]     = new Open(in, f)

  def unapply[A](r: Consecutive[A]): Opt[A -> Opt[A]] = r match {
    case r: Closed[A] if r.size.isZero => none()
    case r: Closed[A]                  => some(r.head -> some(r.last))
    case r: Open[A]                    => some(r.head -> none())
  }

  final class Open[+A](val in: Interval.Open, val applyLong: Long => A) extends Consecutive[A] with Indexed[A] {
    type CC[X] = Open[X]

    def isAfter(n: Long): Bool      = false
    def size                        = Infinite
    def apply(vdex: Index): A       = applyLong(in(vdex))
    def foreach(g: A => Unit): Unit = in foreach (applyLong andThen g)

    def map[B](g: A => B): Open[B]  = in map (applyLong andThen g)

    def drop(n: Precise): Open[A]        = Consecutive(in drop n, applyLong)
    def dropRight(n: Precise): Open[A]   = Consecutive(in dropRight n, applyLong)
    def take(n: Precise): Closed[A]      = Consecutive(in take n, applyLong)
    def takeRight(n: Precise): Closed[A] = Consecutive(in takeRight n, applyLong)

    def to_s = showConsecutive show this
  }
  final class Closed[+A](val in: Interval.Closed, val applyLong: Long => A) extends Consecutive[A] with Direct[A] {
    type CC[X] = Closed[X]

    def isAfter(n: Long)             = in.exclusiveEnd <= n
    def apply(vdex: Index): A        = applyLong(in(vdex))
    def map[B](g: A => B): Closed[B] = in map (applyLong andThen g)
    def size: Precise                = in.size
    def last: A                      = apply(size.lastIndex)

    def drop(n: Precise): Closed[A]      = Consecutive(in drop n, applyLong)
    def dropRight(n: Precise): Closed[A] = Consecutive(in dropRight n, applyLong)
    def take(n: Precise): Closed[A]      = Consecutive(in take n, applyLong)
    def takeRight(n: Precise): Closed[A] = Consecutive(in takeRight n, applyLong)
  }
}
