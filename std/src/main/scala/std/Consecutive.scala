package psp
package std

import api._, all._

sealed abstract class LongInterval extends (Vdex => Long) with ShowSelf {
  type This <: LongInterval

  def contains(n: Long): Bool
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def foreach(f: Long => Unit): Unit
  def map[A](f: Long => A): Consecutive[A]
  def size: Atomic
  def startLong: Long
  def take(n: Precise): LongInterval.Closed

  def apply(vdex: Vdex): Long = startLong + vdex.indexValue
  def slice(s: Long, e: Long): LongInterval.Closed = (
    if (s < 0) slice(0, e)
    else if (e <= 0 || e <= s) LongInterval.empty
    else this drop s take e - s
  )
}
object LongInterval {
  val Empty = new Closed(0L, Size.Zero)

  def empty: Closed                                  = Empty
  def closed(startLong: Long, size: Precise): Closed = if (size.isZero) empty else Closed(startLong, size)
  def until(start: Long, end: Long): Closed          = closed(start, Size(end - start))
  def to(start: Long, end: Long): Closed             = closed(start, Size(end - start + 1))
  def open(start: Long): Open                        = Open(start)

  final case class Closed private[LongInterval] (startLong: Long, size: Precise) extends LongInterval {
    type This = Closed

    def exclusiveEnd: Long = startLong + size.get
    def lastLong: Long     = exclusiveEnd - 1

    def contains(n: Long): Bool                     = startLong <= n && n <= lastLong
    def drop(n: Precise): Closed                    = closed(startLong + n.get, size - n)
    def dropRight(n: Precise): Closed               = closed(startLong, size - n)
    def foreach(f: Long => Unit): Unit              = ll.foreachLong(startLong, lastLong, f)
    def isEmpty: Bool                               = size.isZero
    def isPoint: Bool                               = size.get == 1L
    def map[A](f: Long => A): Consecutive.Closed[A] = Consecutive(this, f)
    def take(n: Precise): Closed                    = closed(startLong, size min n)
    def takeRight(n: Precise): Closed               = (size min n) |> (s => closed(exclusiveEnd - s.get, s))

    def to_s: String = if (isEmpty) "[0,0)"else if (isPoint) s"[$startLong]" else s"[$startLong..$lastLong]"
  }

  final case class Open private[LongInterval] (startLong: Long) extends LongInterval {
    type This = Open

    def contains(n: Long): Bool                   = startLong <= n
    def drop(n: Precise): Open                    = open(startLong + n.get)
    def dropRight(n: Precise): Open               = this
    def foreach(f: Long => Unit): Unit            = ll.foreachLong(startLong, MaxLong, f)
    def map[A](f: Long => A): Consecutive.Open[A] = Consecutive(this, f)
    def size                                      = Infinite
    def take(n: Precise): Closed                  = closed(startLong, n)
    def to_s: String                              = s"[$startLong..)"
  }
}

sealed abstract class Consecutive[+A] extends Indexed[A] with ShowSelf {
  type CC[X] <: Consecutive[X]
  def in: LongInterval
  def map[B](g: A => B): CC[B]
  def applyLong(x: Long): A

  def viewLongs: View[Long]  = Each.construct[Long](in.size, in foreach _).m
  def zipLongs: Zip[Long, A] = zipWith(viewLongs, applyLong)
  def startLong: Long        = in.startLong
  def to_s: String           = in.to_s
}
object Consecutive {
  private val Empty = new Closed[Nothing](LongInterval.empty, indexOutOfBoundsException)

  def empty[A] : Closed[A]                                       = Empty
  def apply[A](in: LongInterval.Closed, f: Long => A): Closed[A] = new Closed(in, f)
  def apply[A](in: LongInterval.Open, f: Long => A): Open[A]     = new Open(in, f)

  final class Open[+A](val in: LongInterval.Open, f: Long => A) extends Consecutive[A] with Indexed[A] {
    type CC[X] = Open[X]

    def applyLong(x: Long): A       = f(x)
    def size                        = Infinite
    def elemAt(vdex: Vdex): A       = f(in(vdex))
    def foreach(g: A => Unit): Unit = in foreach (f andThen g)
    def map[B](g: A => B): Open[B]  = in map (f andThen g)
  }
  final class Closed[+A](val in: LongInterval.Closed, f: Long => A) extends Consecutive[A] with Direct[A] {
    type CC[X] = Closed[X]

    def applyLong(x: Long): A        = f(x)
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
