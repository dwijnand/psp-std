package psp
package std

import all._, StdShow._

sealed trait Vdex extends Any {
  def indexValue: Long
  def nthValue: Long

  def isInvalid: Bool    = indexValue < 0
  def toIndex: Index     = Index(indexValue)
  def toNth: Nth         = Nth(nthValue)
  def excluding: Precise = Size(indexValue)
  def including: Precise = Size(nthValue)
}

/** Index.
  */
final class Index private[std] (val indexValue: Long) extends AnyVal with ShowSelf with Vdex {
  private def mapLong(f: Long => Long): Index = if (isInvalid) this else Index(f(indexValue))

  def +(n: Long): Index = mapLong(_ + n)
  def -(n: Long): Index = mapLong(_ - n)
  def getInt: Int       = safeLongToInt(indexValue) // depend on this
  def isEmpty: Bool     = indexValue < 0
  def isValid: Bool     = indexValue >= 0
  def next: Index       = this + 1
  def nthValue: Long    = indexValue + 1
  def prev: Index       = this - 1

  def fromRight(size: Precise): Index            = mapLong(size.getLong - 1 - _)
  def takeNext(len: Atomic): Consecutive[Index]  = len.foldInfinite(takeAfter, takeNext)
  def takeNext(len: Precise): ClosedRange[Index] = Interval(indexValue, len) map Index
  def takeAfter: OpenRange[Index]                = Interval(indexValue) map Index

  def to_s = pp"$indexValue"
}
final class Nth private[std] (val nthValue: Long) extends AnyVal with ShowSelf with Vdex {
  def indexValue: Long = nthValue - 1
  def to_s = pp"#$nthValue"
}

/** A valid index is always non-negative. All negative indices are
  *  mapped to NoIndex, which has an underlying value of -1.
  *  Manipulations of invalid values remain invalid, like NaN.
  *  All valid indices give rise to a corresponding Nth which is
  *  one larger, i.e. Index(3) is equivalent to Nth(4).
  */
object Index extends (Long => Index) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get < 0 }

  def invalid: Index            = new Index(-1L)
  def apply(value: Long): Index = if (value < 0) invalid else new Index(value)
  def unapply(x: Index)         = new Extractor(x.indexValue)
}

/** Nth is a 1-based index.
  */
object Nth extends (Long => Nth) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get <= 0 }

  def invalid: Nth            = new Nth(0L)
  def apply(value: Long): Nth = if (value <= 0) invalid else new Nth(value)
  def unapply(x: Nth)         = new Extractor(x.nthValue)
}
