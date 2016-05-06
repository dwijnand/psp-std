package psp
package std

import api._, all._
import Consecutive.empty

final class Consecutive[+A] private (val startInt: Int, val lastInt: Int, f: Int => A) extends Direct[A] with ShowSelf {
  private def create(startInt: Int, size: Precise): Consecutive[A] =
    if (isEmpty || size.isZero) empty
    else new Consecutive[A](startInt, startInt + size.getInt - 1, f)

  def hops: Int = lastInt - startInt
  def isEmpty   = hops < 0
  def isPoint   = hops == 0

  def asIndices: VdexRange                    = startInt to lastInt map (i => Index(i))
  def containsInt(n: Int): Bool               = startInt <= n && n <= lastInt
  def drop(n: Precise): Consecutive[A]        = create(startInt + n.toInt, size - n)
  def dropRight(n: Precise): Consecutive[A]   = create(startInt, size - n)
  def elemAt(index: Vdex): A                  = f(startInt + index.getInt)
  def exclusiveEnd: Int                       = lastInt + 1
  def foreach(g: A => Unit): Unit             = if (!isEmpty) ll.foreachConsecutive(startInt, lastInt, f andThen g)
  def map[B](g: A => B): Consecutive[B]       = new Consecutive(startInt, lastInt, f andThen g)
  def size                                    = Size(hops + 1)
  def slice(r: VdexRange): Consecutive[A]     = slice(r.startInt, r.exclusiveEnd)
  def slice(s: Long, e: Long): Consecutive[A] = if (s < 0) slice(0, e) else if (e <= 0 || e <= s) empty else this drop s take e - s
  def take(n: Precise): Consecutive[A]        = create(startInt, size min n)
  def takeRight(n: Precise): Consecutive[A]   = (size min n) |> (s => create(exclusiveEnd - s.toInt, s))

  def to_s = if (isEmpty) "[0,0)" else if (isPoint) s"[$startInt]" else s"[$startInt..$lastInt]"
}

object Consecutive {
  private val id: Int => Int = x => x
  private val Empty = new Consecutive[Nothing](0, -1, indexOutOfBoundsException)

  def empty: Consecutive[Nothing]                                 = Empty
  def to(start: Int, end: Int): IntRange                          = if (end < start) empty else new Consecutive(start, end, id)
  def until(start: Int, end: Int): IntRange                       = if (end <= start) empty else new Consecutive(start, end - 1, id)
  def until[A](start: Int, end: Int, f: Int => A): Consecutive[A] = if (end <= start) empty else new Consecutive(start, end - 1, f)
}
