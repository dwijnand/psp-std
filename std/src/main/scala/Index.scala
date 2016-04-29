package psp
package std

import api._, all._

trait VindexImpl extends Any with ShowSelf {
  self: Vindex =>

  type This <: Vindex
  def create(indexValue: Long): This

  private def mapLong(f: Long => Long): This = if (isInvalid) this.asInstanceOf[This] else create(f(indexValue))

  def nthValue: Long         = indexValue + 1
  def get: Long              = indexValue
  def getInt: Int            = indexValue.safeInt
  def isEmpty                = indexValue < 0
  def isInvalid              = indexValue < 0
  def toIndex: Index         = Index(indexValue)
  def toNth: Nth             = Nth(nthValue)
  def sizeExcluding: Precise = Size(indexValue)
  def sizeIncluding: Precise = Size(nthValue)

  // def to(that: This): Consecutive[This]    = indexValue to that.indexValue map create
  def until(that: This): Consecutive[This] = indexValue until that.indexValue map create
  // def %(size: Precise): This               = mapLong(_ % sizeIncluding.getLong)
  def +(n: Long): This                     = mapLong(_ + n)
  def -(n: Long): This                     = mapLong(_ - n)
  // def /(size: Precise): This               = mapLong(_ / sizeIncluding.getLong)
  def next: This                           = this + 1
  // def prev: This                           = this - 1
}

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  Manipulations of invalid values remain invalid, like NaN.
 *  All valid indices give rise to a corresponding Nth which is
 *  one larger, i.e. Index(3) is equivalent to Nth(4).
 */
object Index extends (Long => Index) {
  def invalid: Index                   = new Impl(-1)
  // def zero: Index                      = new Impl(0)
  def apply(value: Long): Index        = if (value < 0) invalid else new Impl(value)
  def unapply(x: Vindex): Option[Long] = if (x.isEmpty) none() else some(x.indexValue)
  def impl(x: Vindex): Impl            = new Impl(x.indexValue)

  final class Impl private[std] (val indexValue: Long) extends AnyVal with api.Index with VindexImpl {
    type This                     = Index
    def create(value: Long): This = Index(value)
    def to_s                      = if (isInvalid) "<invalid>" else s"$indexValue"
  }
}

/** Nth is a 1-based index.
 */
object Nth extends (Long => Nth) {
  def invalid: Nth                     = new Impl(-1)
  def apply(value: Long): Nth          = if (value <= 0) invalid else new Impl(value - 1)
  def unapply(x: Vindex): Option[Long] = if (x.isEmpty) none() else some(x.nthValue)
  // def impl(x: Nth): Impl               = new Impl(x.indexValue)

  final class Impl private[std] (val indexValue: Long) extends AnyVal with api.Nth with VindexImpl {
    type This                         = Nth
    def create(indexValue: Long): Nth = Nth(indexValue + 1)
    def to_s                          = if (isInvalid) "<invalid>" else s"#$nthValue"
  }
}
