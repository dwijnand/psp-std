package psp
package std

import scala.collection.immutable.VectorBuilder
import api._, all._, StdShow._

object Vec {
  def empty[@fspec A] : Vec[A]                        = NIL.castTo[Vec[A]]
  def apply[@fspec A](xs: A*): Vec[A]                 = newBuilder[A] build (Direct scala xs)
  def unapplySeq[@fspec A](x: Vec[A]): Some[scSeq[A]] = Some(x.seq)
  def newBuilder[@fspec A](): Builder[A]              = new Builder[A](new VectorBuilder[A])

  private[std] val NIL = new Vec[Any](sciVector())

  final class Builder[@fspec A](buf: VectorBuilder[A]) extends Builds[A, Vec[A]] {
    def build(xs: Foreach[A]): Vec[A] = { xs foreach add ; result }
    def add(elem: A): Unit            = buf += elem
    def result(): Vec[A]              = new Vec(buf.result)
  }
}

final class Vec[@fspec A](private val underlying: sciVector[A]) extends AnyVal with Direct[A] with ShowSelf {
  def isEmpty       = length <= 0
  def nonEmpty      = length > 0
  def lastIntIndex  = length - 1
  def length: Int   = underlying.length
  def size: Precise = Size(length)

  def updated(i: Vindex, elem: A): Vec[A] = new Vec[A](underlying.updated(i.getInt, elem))
  def :+(elem: A): Vec[A] = new Vec[A](underlying :+ elem)
  def +:(elem: A): Vec[A] = new Vec[A](elem +: underlying)
  def ++(that: Vec[A]): Vec[A] = (
    if (that.isEmpty) this
    else if (this.isEmpty) that
    else {
      val b = Vec.newBuilder[A]
      this foreach b.add
      that foreach b.add
      b.result
    }
  )

  def applyInt(index: Int): A      = underlying(index)
  def drop(n: Vindex): Vec[A]      = new Vec[A](underlying drop n.getInt)
  def dropRight(n: Vindex): Vec[A] = new Vec[A](underlying dropRight n.getInt)
  def elemAt(i: Vindex): A         = underlying(i.getInt)
  def take(n: Vindex): Vec[A]      = new Vec[A](underlying take n.getInt)
  def takeRight(n: Vindex): Vec[A] = new Vec[A](underlying takeRight n.getInt)

  @inline def foreach(f: A => Unit): Unit = {
    if (!isEmpty)
      lowlevel.ll.foreachConsecutive(0, lastIntIndex, i => f(applyInt(i)))
  }
  def to_s = "[ " + (this map (_.any_s) mk_s ", ") + " ]"
}
