package psp
package std

import api._, all._

sealed abstract class LongInterval {
  def size: Atomic
  def startLong: Long

  def contains(n: Long): Bool
  def take(n: Precise): LongInterval.Closed
  def drop(n: Precise): LongInterval
  def dropRight(n: Precise): LongInterval

  def map[A](f: Long => A): Consecutive[A]
}
object LongInterval {
  val Empty = new Closed(0L, Size.Zero)

  def empty: Closed                                 = Empty
  def apply(startLong: Long): Open                  = Open(startLong)
  def apply(startLong: Long, size: Precise): Closed = if (size.isZero) empty else Closed(startLong, size)

  final case class Closed private[LongInterval] (startLong: Long, size: Precise) extends LongInterval {
    def exclusiveEnd: Long                          = startLong + size.get
    def lastLong: Long                              = exclusiveEnd - 1
    def drop(n: Precise): Closed                    = LongInterval(startLong + n.get, size - n)
    def take(n: Precise): Closed                    = LongInterval(startLong, size min n)
    def takeRight(n: Precise): Closed               = (size min n) |> (s => LongInterval(exclusiveEnd - s.get, s))
    def dropRight(n: Precise): Closed               = LongInterval(startLong, size - n)
    def slice(s: Long, e: Long): Closed             = if (s < 0) slice(0, e) else if (e <= 0 || e <= s) empty else this drop s take e - s
    def foreach(f: Long => Unit): Unit              = ll.foreachLong(startLong, lastLong, f)
    def contains(n: Long): Bool                     = startLong <= n && n <= lastLong
    def map[A](f: Long => A): Consecutive.Closed[A] = Consecutive(this, f)
  }
  final case class Open private[LongInterval] (startLong: Long) extends LongInterval {
    def size                                      = Infinite
    def dropRight(n: Precise): Open               = this
    def drop(n: Precise): Open                    = LongInterval(startLong + n.get)
    def take(n: Precise): Closed                  = LongInterval(startLong, n)
    def foreach(f: Long => Unit): Unit            = ll.foreachLong(startLong, MaxLong, f)
    def contains(n: Long): Bool                   = startLong <= n
    def map[A](f: Long => A): Consecutive.Open[A] = Consecutive(this, f)
  }
}

sealed abstract class Consecutive[+A] extends Indexed[A] with ShowSelf {
  def in: LongInterval
  def startLong: Long = in.startLong
  def to_s = s"Consec($in, <f>)"
}
object Consecutive {
  private val id: Long => Long = x => x
  private val Empty = new Closed[Nothing](LongInterval.empty, indexOutOfBoundsException)

  def empty[A] : Closed[A]                                       = Empty
  def apply[A](in: LongInterval.Closed, f: Long => A): Closed[A] = new Closed(in, f)
  def apply[A](in: LongInterval.Open, f: Long => A): Open[A]     = new Open(in, f)

  def until(start: Long, end: Long): Closed[Long]               = until(start, end, id)
  def until[A](start: Long, end: Long, f: Long => A): Closed[A] = apply(LongInterval(start, Size(end - start)), f)

  def to(start: Long, end: Long): Closed[Long]               = to(start, end, id)
  def to[A](start: Long, end: Long, f: Long => A): Closed[A] = apply(LongInterval(start, Size(end - start + 1)), f)

  final class Open[+A](val in: LongInterval.Open, f: Long => A) extends Consecutive[A] with Indexed[A] {
    def size                        = Infinite
    def elemAt(index: Vdex): A      = f(in.startLong + index.indexValue)
    def foreach(g: A => Unit): Unit = in foreach (f andThen g)
  }
  final class Closed[+A](val in: LongInterval.Closed, f: Long => A) extends Consecutive[A] with Direct[A] {
    private def create(in: LongInterval.Closed): Closed[A] = new Closed[A](in, f)

    def asIndices: VdexRange               = in map (i => Index(i))
    def containsLong(n: Long): Bool        = in contains n
    def drop(n: Precise): Closed[A]        = create(in drop n)
    def dropRight(n: Precise): Closed[A]   = create(in dropRight n)
    def elemAt(index: Vdex): A             = f(in.startLong + index.indexValue)
    def foreach(g: A => Unit): Unit        = in foreach (f andThen g)
    def map[B](g: A => B): Closed[B]       = new Closed(in, f andThen g)
    def size                               = in.size
    // def slice(r: VdexRange): Closed[A]  = create(in slice r)
    def slice(s: Long, e: Long): Closed[A] = create(in.slice(s, e))
    def take(n: Precise): Closed[A]        = create(in take n)
    def takeRight(n: Precise): Closed[A]   = create(in takeRight n)
  }
}
