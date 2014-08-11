package psp
package std

import java.{ lang => jl }

final class IntExtensionOps(val self: Int) extends AnyVal with Ordered[Int] {
  private type This = Int
  def compare(rhs: This): This = jl.Integer.compare(self, rhs)
  def abs: This                = math.abs(self)
  def max(that: This): This    = math.max(self, that)
  def min(that: This): This    = math.min(self, that)
  def signum: This             = math.signum(self)

  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)
}

final class LongExtensionOps(val self: Long) extends AnyVal with Ordered[Long] {
  private type This = Long
  def compare(rhs: This): Int = jl.Long.compare(self, rhs)
  def abs: This               = math.abs(self)
  def max(that: This): This   = math.max(self, that)
  def min(that: This): This   = math.min(self, that)
  def signum: This            = math.signum(self)
}
