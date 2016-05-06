package psp
package std

import api._, all._

object Fun {
  // private val Undefined: Opaque[Any, Nothing] = Opaque[Any, Nothing](illegalArgumentException)
  // private val Empty: FilterIn[Any, Nothing]   = FilterIn[Any, Nothing](false, Undefined)

  // def empty[A, B] : Fun[A, B]                     = Empty
  def apply[A, B](f: A => B): Opaque[A, B]        = Opaque(f)
  // def partial[A, B](pf: A ?=> B): FilterIn[A, B]  = FilterIn(pf.isDefinedAt, Opaque(pf))
  // def const[B](value: B): Opaque[Any, B]          = Opaque(_ => value)
  // def finite[A, B](kvs: (A->B)*): FiniteDom[A, B] = FiniteDom((kvs map fst).byEquals.toSet, partial(kvs map tuple toMap))
}

object ExSet {
  def fromJava[A](xs: jSet[A]): ExSet[A]   = fromScala(xs.to[sciSet].toSet) // toSet is actually needed
  def fromScala[A](xs: scSet[A]): ExSet[A] = new Impl[A](xs.toSet)
  def impl[A](xs: ExSet[A]): Impl[A]       = xs match {
    case xs: Impl[A] => xs
    case _           => fromScala(xs.to[sciSet])
  }
  final class Impl[A](xs: sciSet[A]) extends ExSet[A] with ToBool[A] {
    def size: Precise                   = Size(xs.size)
    @inline def foreach(f: A => Unit)   = xs foreach f
    def apply(elem: A): Bool            = xs(elem)
    def filter(p: ToBool[A]): ExSet[A]  = new Impl(xs filter p)
    def union(that: ExSet[A]): ExSet[A] = new Impl(that.foldl(xs)(_ + _))
  }
}

object ExMap {
  // def empty[K, V] : ExMap[K, V]                                = apply(Fun.finite())
  def apply[K, V](f: FiniteDom[K, V]): ExMap[K, V]                = new Impl(f)
  def apply[K, V](keys: ExSet[K], lookup: Fun[K, V]): ExMap[K, V] = apply(FiniteDom(keys, lookup))
  def fromJava[K, V](xs: jMap[K, V]): ExMap[K, V]                 = apply[K, V](ExSet fromJava xs.keySet, Opaque[K, V](xs get _))
  def fromScala[K, V](xs: scMap[K, V]): ExMap[K, V]               = apply[K, V](xs.keys.byEquals.toExSet, Opaque(xs))

  def impl[K, V](xs: ExMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.lookup)
  }
  final class Impl[K, V](val lookup: FiniteDom[K, V]) extends ExMap[K, V] {
    import lookup._
    type Entry = K -> V

    def keys: View[K]                     = keySet.m
    // def values: View[V]                = keyVector map xs.lookup
    def keySet: ExSet[K]                  = lookup.keys
    def keyVector: Vec[K]                 = keys.toVec
    def entries: ZipView[K, V]            = keyVector mapZip lookup
    def map[V1](g: V => V1): ExMap[K, V1] = ExMap(keySet, f mapOut g)
    def apply(key: K): V                  = lookup(key)
  }
}
