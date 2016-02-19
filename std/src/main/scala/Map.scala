package psp
package std

import api._, all._

object Fun {
  private val Undefined: Opaque[Any, Nothing] = Opaque[Any, Nothing](illegalArgumentException)
  private val Empty: FilterIn[Any, Nothing]   = FilterIn[Any, Nothing](false, Undefined)

  def empty[A, B] : Fun[A, B]                     = Empty
  def apply[A, B](f: A => B): Opaque[A, B]        = Opaque(f)
  def partial[A, B](pf: A ?=> B): FilterIn[A, B]  = FilterIn(pf.isDefinedAt, Opaque(pf))
  def const[B](value: B): Opaque[Any, B]          = Opaque(_ => value)
  def finite[A, B](kvs: (A->B)*): FiniteDom[A, B] = FiniteDom((kvs map fst).byEquals.toSet, partial(kvs map tuple toMap))
}

object ExMap {
  def empty[K, V] : ExMap[K, V]                                   = apply(Fun.finite())
  def apply[K, V](f: FiniteDom[K, V]): ExMap[K, V]                = new Impl(f)
  def apply[K, V](keys: ExSet[K], lookup: Fun[K, V]): ExMap[K, V] = apply(FiniteDom(keys, lookup))
  def fromJava[K, V](xs: jMap[K, V]): ExMap[K, V]                 = apply[K, V](ExSet fromJava xs.keySet, Opaque[K, V](xs get _))
  def fromScala[K, V](xs: scMap[K, V]): ExMap[K, V]               = apply[K, V](xs.keys.byEquals.toSet, Opaque(xs))

  def impl[K, V](xs: ExMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.lookup)
  }

  def unapplySeq[K, V](map: ExMap[K, V]): Some[scSeq[K]] = Some(map.keyVector.seq)

  final class Impl[K, V](val lookup: FiniteDom[K, V]) extends ExMap[K, V] {
    import lookup._

    def apply(key: K): V                      = lookup(key)
    def map[V1](g: V => V1): ExMap[K, V1]     = ExMap(keys, f mapOut g)
    def filterKeys(p: ToBool[K]): ExMap[K, V] = ExMap(keys filter p, f)
  }
}
