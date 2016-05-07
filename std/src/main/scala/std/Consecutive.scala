package psp
package std

import api._, all._

sealed abstract class LongInterval extends (Vdex => Long) {
  type This <: LongInterval

  def size: Atomic
  def startLong: Long
  def contains(n: Long): Bool
  def take(n: Precise): LongInterval.Closed
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def map[A](f: Long => A): Consecutive[A]

  def apply(vdex: Vdex): Long = startLong + vdex.indexValue
  def slice(s: Long, e: Long): LongInterval.Closed = (
    if (s < 0) slice(0, e)
    else if (e <= 0 || e <= s) LongInterval.empty
    else this drop s take e - s
  )
}
object LongInterval {
  val Empty = new Closed(0L, Size.Zero)

  def empty: Closed                                 = Empty
  def apply(startLong: Long): Open                  = Open(startLong)
  def apply(startLong: Long, size: Precise): Closed = if (size.isZero) empty else Closed(startLong, size)

  final case class Closed private[LongInterval] (startLong: Long, size: Precise) extends LongInterval {
    type This = Closed

    def exclusiveEnd: Long                          = startLong + size.get
    def lastLong: Long                              = exclusiveEnd - 1
    def drop(n: Precise): Closed                    = LongInterval(startLong + n.get, size - n)
    def take(n: Precise): Closed                    = LongInterval(startLong, size min n)
    def takeRight(n: Precise): Closed               = (size min n) |> (s => LongInterval(exclusiveEnd - s.get, s))
    def dropRight(n: Precise): Closed               = LongInterval(startLong, size - n)
    def foreach(f: Long => Unit): Unit              = ll.foreachLong(startLong, lastLong, f)
    def contains(n: Long): Bool                     = startLong <= n && n <= lastLong
    def map[A](f: Long => A): Consecutive.Closed[A] = Consecutive(this, f)
  }
  final case class Open private[LongInterval] (startLong: Long) extends LongInterval {
    type This = Open

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
  def until(start: Long, end: Long): Closed[Long]                = until(start, end, id)
  def until[A](start: Long, end: Long, f: Long => A): Closed[A]  = apply(LongInterval(start, Size(end - start)), f)
  def to(start: Long, end: Long): Closed[Long]                   = to(start, end, id)
  def to[A](start: Long, end: Long, f: Long => A): Closed[A]     = apply(LongInterval(start, Size(end - start + 1)), f)

  final class Open[+A](val in: LongInterval.Open, f: Long => A) extends Consecutive[A] with Indexed[A] {
    def size                        = Infinite
    def elemAt(vdex: Vdex): A       = f(in(vdex))
    def foreach(g: A => Unit): Unit = in foreach (f andThen g)
  }
  final class Closed[+A](val in: LongInterval.Closed, f: Long => A) extends Consecutive[A] with Direct[A] {
    def containsLong(n: Long): Bool  = in contains n
    def elemAt(vdex: Vdex): A        = f(in(vdex))
    def foreach(g: A => Unit): Unit  = in foreach (f andThen g)
    def map[B](g: A => B): Closed[B] = in map (f andThen g)
    def size: Precise                = in.size

    def drop(n: Precise): Closed[A]        = Consecutive(in drop n, f)
    def dropRight(n: Precise): Closed[A]   = Consecutive(in dropRight n, f)
    def take(n: Precise): Closed[A]        = Consecutive(in take n, f)
    def takeRight(n: Precise): Closed[A]   = Consecutive(in takeRight n, f)
    def slice(s: Long, e: Long): Closed[A] = Consecutive(in.slice(s, e), f)
  }
}
