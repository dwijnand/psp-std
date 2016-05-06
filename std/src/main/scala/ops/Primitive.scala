package psp
package std
package ops

import java.{ lang => jl }
import api._, exp._, all.opsInt

final class AnyOps[A](val x: A) extends AnyVal {
  def any_s: String                         = s"$x"
  def id_## : Int                           = java.lang.System.identityHashCode(x)
  def id_==(y: Any): Boolean                = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]  // Calling eq on Anys.
  def matchIf[B: Empty](pf: A ?=> B): B     = matchOr(emptyValue[B])(pf)
  def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt

  @inline def |>[B](f: A => B): B = f(x)  // The famed forward pipe.
}

final class CharOps(val ch: Char) extends AnyVal {
  // def isAlphabetic = jl.Character isAlphabetic ch
  def isControl    = jl.Character isISOControl ch
  // def isDigit      = jl.Character isDigit ch
  // def isLetter     = jl.Character isLetter ch
  // def isLower      = jl.Character isLowerCase ch
  // def isUpper      = jl.Character isUpperCase ch
  // def toLower      = jl.Character toLowerCase ch
  // def isSpace      = jl.Character isWhitespace ch
  def toUpper      = jl.Character toUpperCase ch
  def to_s         = ch.toString
}
final class IntOps(val self: Int) extends AnyVal {
  // def abs: Int                         = scala.math.abs(self)
  def downTo(end: Int): Direct[Int]    = Consecutive.downTo(self, end)
  def takeNext(len: Precise): IntRange = until(self + len.getInt)
  def to(end: Int): IntRange           = Consecutive.to(self, end)
  def until(end: Int): IntRange        = Consecutive.until(self, end)
}

final class LongOps(val self: Long) extends AnyVal {
  def to(end: Long): LongRange    = safeLongToInt(self) to safeLongToInt(end) map (_.toLong)
  def until(end: Long): LongRange = safeLongToInt(self) until safeLongToInt(end) map (_.toLong)
}
