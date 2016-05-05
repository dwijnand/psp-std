package psp
package std

import scala.collection.immutable.VectorBuilder
import api._, all._, StdShow._

object Vec {
  private val NIL = new Vec[Any](sciVector())

  def empty[@fspec A] : Vec[A]                        = NIL.castTo[Vec[A]]
  def apply[@fspec A](xs: A*): Vec[A]                 = newBuilder[A] build (Direct scala xs)
  def unapplySeq[@fspec A](x: Vec[A]): Some[scSeq[A]] = Some(x.seq)
  def newBuilder[@fspec A](): Builds[A, Vec[A]]       = Builds { xs =>
    val buf = new VectorBuilder[A]
    xs foreach (buf += _)
    new Vec(buf.result)
  }
}
final class Vec[@fspec A](private val underlying: sciVector[A]) extends AnyVal with Direct[A] with ShowSelf {
  def isEmpty       = length <= 0
  def nonEmpty      = length > 0
  def lastIntIndex  = length - 1
  def length: Int   = underlying.length
  def size: Precise = Size(length)

  def updated(i: Vdex, elem: A): Vec[A] = new Vec[A](underlying.updated(i.getInt, elem))
  def :+(elem: A): Vec[A] = new Vec[A](underlying :+ elem)
  def +:(elem: A): Vec[A] = new Vec[A](elem +: underlying)
  def ++(that: Vec[A]): Vec[A] = (
    if (that.isEmpty) this
    else if (this.isEmpty) that
    else new Vec[A](underlying ++ that.trav)
  )

  def applyInt(index: Int): A    = underlying(index)
  def drop(n: Vdex): Vec[A]      = new Vec[A](underlying drop n.getInt)
  def dropRight(n: Vdex): Vec[A] = new Vec[A](underlying dropRight n.getInt)
  def elemAt(i: Vdex): A         = underlying(i.getInt)
  def take(n: Vdex): Vec[A]      = new Vec[A](underlying take n.getInt)
  def takeRight(n: Vdex): Vec[A] = new Vec[A](underlying takeRight n.getInt)

  @inline def foreach(f: A => Unit): Unit = {
    if (!isEmpty)
      ll.foreachConsecutive(0, lastIntIndex, i => f(applyInt(i)))
  }
  def to_s = "[ " + (this map (_.any_s) mk_s ", ") + " ]"
}
