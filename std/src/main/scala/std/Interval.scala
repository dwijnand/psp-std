package psp
package std

import all._, Interval._

/** TODO: deal with Vdex potentially having -1 for a value.
  */
sealed abstract class Interval extends (Vdex => Long) {
  type This <: Interval

  def contains(n: Long): Bool
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def foreach(f: Long => Unit): Unit
  def isEmpty: Bool
  def lastLong: Long
  def map[A](f: Long => A): Indexed[A]
  def size: Atomic
  def startLong: Long
  def take(n: Precise): Closed
  def <<(n: Precise): This
  def >>(n: Precise): This

  private def checkValid[A](vdex: Vdex)(f: Vdex => A): A =
    cond(vdex.isInvalid, abort("Operation requires valid index"), f(vdex))

  def apply(vdex: Vdex): Long                  = checkValid(vdex)(startLong + _.indexValue)
  def slice(start: Vdex, len: Precise): Closed = checkValid(start)(this drop _.excluding take len)
  def slice(r: VdexRange): Closed              = zcond(!r.isEmpty, slice(r.head, r.size))
}
object Interval {
  val Empty = new Closed(0L, _0)

  def apply(start: Long): Open                   = Open(start)
  def apply(start: Long, size: Precise): Closed  = if (size.isZero) empty else Closed(start, size)
  def unapply(r: Interval): Some[Long -> Atomic] = Some(r.startLong -> r.size)

  def empty: Closed                         = Empty
  def until(start: Long, end: Long): Closed = Interval(start, Size(end - start))
  def to(start: Long, end: Long): Closed    = Interval(start, Size(end - start + 1))

  final case class Closed private[Interval](startLong: Long, size: Precise) extends Interval {
    type This = Closed

    def <<(n: Precise): Closed = Closed(startLong - n.getLong, size)
    def >>(n: Precise): Closed = Closed(startLong + n.getLong, size)

    def exclusiveEnd: Long = startLong + size.getLong
    def lastLong: Long     = exclusiveEnd - 1

    def contains(n: Long): Bool              = startLong <= n && n <= lastLong
    def drop(n: Precise): Closed             = Interval(startLong + n.getLong, size - n)
    def dropRight(n: Precise): Closed        = Interval(startLong, size - n)
    def foreach(f: Long => Unit): Unit       = ll.foreachLong(startLong, lastLong, f)
    def isEmpty: Bool                        = size.isZero
    def isPoint: Bool                        = size.getLong === 1L
    def map[A](f: Long => A): ClosedRange[A] = Consecutive(this, f)
    def take(n: Precise): Closed             = Interval(startLong, min(size, n))
    def takeRight(n: Precise): Closed        = min(size, n) |> (s => Interval(exclusiveEnd - s.getLong, s))
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
  }
}
