package psp
package std

import api._, all._

final class DirectOps[A](val xs: Direct[A]) extends AnyVal {
  def reverse: Direct[A] = Each reversed xs
  def apply(i: Vdex): A  = xs elemAt i
  def indices: VdexRange = xs.size.indices
  def lastIndex: Index   = Index(xs.size.getLong - 1) // effectively maps both undefined and zero to no index.

  def containsIndex(vdex: Vdex): Boolean = indices containsLong vdex.indexValue

  @inline def foreachIndex(f: Vdex => Unit): Unit =
    ll.foreachLong(0, lastIndex.indexValue, i => f(Index(i)))
}

final class ForeachOps[A](val xs: Foreach[A]) {
  private[this] def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build each

  def trav: scTraversable[A]      = to[scTraversable] // flatMap, usually
  def seq: scSeq[A]               = to[scSeq] // varargs or unapplySeq, usually
  def toRefs: Each[Ref[A]]        = each map castRef
  def toRefArray(): Array[Ref[A]] = toRefs.force
  def each: Each[A]               = Each(xs foreach _)
  def view: View[A]               = each.m

  def map[B](f: A => B): Foreach[B] = Each(g => xs foreach (f andThen g))
}
