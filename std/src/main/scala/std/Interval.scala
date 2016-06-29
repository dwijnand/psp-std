package psp
package std

import all._, Interval._
import StdShow._

/** TODO: deal with Vdex potentially having -1 for a value.
  */
sealed abstract class Interval extends UseToS {
  type This <: Interval

  /** These properties define the interval. */
  def startLong: Long
  def size: Atomic

  def contains(n: Long): Bool
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def isEmpty: Bool
  def lastLong: Long
  def map[A](f: Long => A): Consecutive[A]
  def <<(n: Precise): This
  def >>(n: Precise): This
  def take(n: Precise): Closed
  def takeRight(n: Precise): Closed

  def foreach(f: Long => Unit): Unit
  def foldl[R](zero: R)(f: (R, Long) => R): R = {
    var res = zero
    foreach(x => res = f(res, x))
    res
  }

  private def checkValid[A](idx: Index)(f: Index => A): A =
    cond(idx.isInvalid, abort("Operation requires valid index"), f(idx))

  def apply(vdex: Index): Long                   = checkValid(vdex)(startLong + _.indexValue)
  def slice(start: Index, len: Precise): Closed  = checkValid(start)(this drop _.excluding take len)
  def slice(start: Index, len: Atomic): Interval = checkValid(start)(s => len.foldInfinite(drop(s.excluding), slice(s, _)))
  def slice(r: SliceRange): Interval             = r.size.foldZero(Interval.empty, len => slice(r.head, len))
  def take(n: Atomic): Interval                  = n.foldInfinite(this, take)
  def takeRight(n: Atomic): Interval             = n.foldInfinite(this, takeRight)

  def to_s: String = this match {
    case Interval(_, Size.Zero)    => "[0,0)"
    case Interval(start, Infinite) => pp"[$start..]"
    case Interval(start, Size.One) => pp"[$start]"
    case Interval(start, len)      => pp"[$start..${ start + len - 1 }]"
  }
}
object Interval {
  val Empty = new Closed(0L, _0)

  def apply(start: Long): Open                   = Open(start)
  def apply(start: Long, size: Atomic): Interval = size.foldInfinite(apply(start), apply(start, _))
  def apply(start: Long, size: Precise): Closed  = if (size.isZero) empty else Closed(start, size)
  def unapply(r: Interval): Some[Long -> Atomic] = Some(r.startLong -> r.size)

  def empty: Closed                         = Empty
  def until(start: Long, end: Long): Closed = Interval(start, Size(end - start))
  def to(start: Long, end: Long): Closed    = Interval(start, Size(end - start + 1))
  def point(start: Long): Closed            = Interval(start, start + 1)

  final case class Closed private[Interval](startLong: Long, size: Precise) extends Interval {
    type This = Closed

    def <<(n: Precise): Closed = Closed(startLong - n.getLong, size)
    def >>(n: Precise): Closed = Closed(startLong + n.getLong, size)

    def exclusiveEnd: Long = startLong + size.getLong
    def lastLong: Long     = exclusiveEnd - 1

    def foldr[R](zero: R)(f: (Long, R) => R): R = {
      var res = zero
      foreachReverse(x => res = f(x, res))
      res
    }

    def takeWhile(p: ToBool[Long]): Closed = take(prefixLength(p))
    def dropWhile(p: ToBool[Long]): Closed = drop(prefixLength(p))

    def longWhere(p: ToBool[Long]): Opt[Long] = {
      foreach(x => if (p(x)) return some(x))
      none()
    }
    def forall(p: ToBool[Long]): Bool = {
      foreach(x => if (!p(x)) return false)
      true
    }
    def prefixLength(p: ToBool[Long]): Precise = {
      var len = 0L
      foreach(x => if (p(x)) len += 1 else return Precise(len))
      size
    }
    def filter(p: ToBool[Long]): Pstream[Closed] = (
      if (isEmpty) Pstream.empty
      else if (p(startLong)) Pstream(take(prefixLength(p)), drop(prefixLength(p)) filter p)
      else this drop prefixLength(!p) filter p
    )

    def contains(n: Long): Bool                  = startLong <= n && n <= lastLong
    def drop(n: Precise): Closed                 = Interval(startLong + n.getLong, size - n)
    def dropRight(n: Precise): Closed            = Interval(startLong, size - n)
    def foreach(f: Long => Unit): Unit           = if (!isEmpty) ll.foreachLongNonEmpty(startLong, lastLong, f)
    def foreachReverse(f: Long => Unit): Unit    = if (!isEmpty) ll.foreachLongNonEmptyReverse(lastLong, startLong, f)
    def isEmpty: Bool                            = size.isZero
    def isPoint: Bool                            = size.getLong === 1L
    def map[A](f: Long => A): ClosedRange[A]     = Consecutive(this, f)
    def take(n: Precise): Closed                 = Interval(startLong, min(size, n))
    def takeRight(n: Precise): Closed            = min(size, n) |> (s => Interval(exclusiveEnd - s.getLong, s))
    def span(p: ToBool[Long]): PairOf[Closed]    = splitAfter(prefixLength(p))
    def splitAfter(len: Precise): PairOf[Closed] = take(len) -> drop(len)
  }

  final case class Open private[Interval](startLong: Long) extends Interval {
    type This = Open

    def <<(n: Precise): Open = Open(startLong - n.getLong)
    def >>(n: Precise): Open = Open(startLong + n.getLong)

    def lastLong: Long                     = MaxLong
    def contains(n: Long): Bool            = startLong <= n
    def drop(n: Precise): Open             = this >> n
    def dropRight(n: Precise): Open        = this
    def foreach(f: Long => Unit): Unit     = ll.foreachLong(startLong, MaxLong, f)
    def map[A](f: Long => A): OpenRange[A] = Consecutive(this, f)
    def isEmpty: Bool                      = false
    def size                               = Infinite
    def take(n: Precise): Closed           = Interval(startLong, n)
    def takeRight(n: Precise): Closed      = abort(pp"takeRight($n) on infinite indices")
  }
}
