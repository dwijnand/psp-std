package psp
package std

import api._, all._, Interval._

/** TODO: deal with Vdex potentially having -1 for a value.
  */
sealed abstract class Interval extends (Vdex => Long) with ShowSelf {
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

  def apply(vdex: Vdex): Long                  = startLong + vdex.indexValue
  def slice(start: Vdex, len: Precise): Closed = longSlice(start.indexValue, start.indexValue + len.getLong)
  def slice(r: VdexRange): Closed              = if (r.isEmpty) Interval.empty else slice(r.head, r.size)

  private def longSlice(s: Long, e: Long): Closed =
    (if (s < 0) longSlice(0, e)
     else if (e <= 0 || e <= s) Interval.empty
     else this drop s take e - s)

  override def hashCode = startLong.## + size.##
  override def equals(that: Any): Bool = that match {
    case that: Interval => startLong === that.startLong && size === that.size
    case _              => false
  }
}
object Interval {
  val Empty = new Closed(0L, Size.Zero)

  def unapply(r: Interval): Some[Long -> Atomic] = Some(r.startLong -> r.size)

  def empty: Closed                                  = Empty
  def closed(startLong: Long, size: Precise): Closed = if (size.isZero) empty else Closed(startLong, size)
  def until(start: Long, end: Long): Closed          = closed(start, Size(end - start))
  def to(start: Long, end: Long): Closed             = closed(start, Size(end - start + 1))
  def open(start: Long): Open                        = Open(start)

  final case class Closed private[Interval](startLong: Long, size: Precise) extends Interval {
    type This = Closed

    def <<(n: Precise): Closed = Closed(startLong - n.getLong, size)
    def >>(n: Precise): Closed = Closed(startLong + n.getLong, size)

    def exclusiveEnd: Long = startLong + size.getLong
    def lastLong: Long     = exclusiveEnd - 1

    def contains(n: Long): Bool              = startLong <= n && n <= lastLong
    def drop(n: Precise): Closed             = closed(startLong + n.getLong, size - n)
    def dropRight(n: Precise): Closed        = closed(startLong, size - n)
    def foreach(f: Long => Unit): Unit       = ll.foreachLong(startLong, lastLong, f)
    def isEmpty: Bool                        = size.isZero
    def isPoint: Bool                        = size.getLong == 1L
    def map[A](f: Long => A): ClosedRange[A] = Consecutive(this, f)
    def take(n: Precise): Closed             = closed(startLong, size min n)
    def takeRight(n: Precise): Closed        = (size min n) |> (s => closed(exclusiveEnd - s.getLong, s))

    def to_s: String = if (isEmpty) "[0,0)" else if (isPoint) s"[$startLong]" else s"[$startLong..$lastLong]"
  }

  final case class Open private[Interval](startLong: Long) extends Interval {
    type This = Open

    def <<(n: Precise): Open = Open(startLong - n.getLong)
    def >>(n: Precise): Open = Open(startLong + n.getLong)

    def lastLong: Long                     = MaxLong
    def contains(n: Long): Bool            = startLong <= n
    def drop(n: Precise): Open             = open(startLong + n.getLong)
    def dropRight(n: Precise): Open        = this
    def foreach(f: Long => Unit): Unit     = ll.foreachLong(startLong, MaxLong, f)
    def map[A](f: Long => A): OpenRange[A] = Consecutive(this, f)
    def isEmpty: Bool                      = false
    def size                               = Infinite
    def take(n: Precise): Closed           = closed(startLong, n)
    def to_s: String                       = s"[$startLong..)"
  }
}
