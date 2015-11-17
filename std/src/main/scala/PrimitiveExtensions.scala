package psp
package std
package ops

import java.{ lang => jl }
import api._

final object UnitOps {
  @inline def thenReturn[A](value: A): A = value
}

final class FixType[A, R](x: A) { def apply(f: A => R): R = f(x) }

final class AnyOps[A](val x: A) extends AnyVal {
  private def lastOf[A](xs: Array[A]): A = xs match { case Array(xs @ _*) => xs.last }
  // Short decoded class name.
  // Note - don't use any implicit ops here as it sends us into loops when debugging.
  def shortClass: String   = NameTransformer decode lastOf(x.getClass.getName split "[.]")
  def shortPackage: String = x.getClass.getPackage.toString

  def isClass[A: CTag]        = classOf[A] isAssignableFrom x.getClass

  @inline def castTo[U] : U                    = x.asInstanceOf[U]
  @inline def doto(f: A => Unit): A            = sideEffect(f(x))
  @inline def isNull: Boolean                  = toRef eq null
  @inline def isOr(p: ToBool[A])(alt: => A): A = if (p(x)) x else alt
  @inline def sideEffect(body: Unit): A        = x
  @inline def toRef: Ref[A]                    = castTo[Ref[A]]
  @inline def |>[B](f: A => B): B              = f(x)  // The famed forward pipe.
  @inline def id_==(y: Any): Boolean           = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]  // Calling eq on Anys.
  @inline def id_## : Int                      = identityHashCode(x)

  def reflect[B](m: jMethod)(args: Any*): B = m.invoke(x, args.map(_.castTo[AnyRef]): _*).castTo[B]

  def fix[R]: FixType[A, R] = new FixType[A, R](x)
  def same: FixType[A, A]   = new FixType[A, A](x)

  def optionally[B](pf: A ?=> B): Option[B] = if (pf isDefinedAt x) Some(pf(x)) else None
  def requiring(p: ToBool[A]): Option[A] = if (p(x)) Some(x) else None
  def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt
  def try_s(implicit z: Show[A] = null): String = if (!z.isNull) z show x else x match {
    case x: ShowDirect => x.to_s
    case _             => any_s
  }
  def any_s: String = "" + x
  def anydoc: Doc   = ("" + x).s
}

final class AnyRefOps[A <: AnyRef](val x: A) extends AnyVal {
  @inline def foldNull[B](zero: => B)(f: A => B): B = if (x eq null) zero else f(x)
  @inline def doto(f: A => Unit): x.type            = sideEffect(f(x))
  @inline def sideEffect(body: Unit): x.type        = x
}

final class CharOps(val ch: Char) extends AnyVal {
  def to_s: String                = ch.toString
  def prev: Char                  = (ch - 1).toChar
  def next: Char                  = (ch + 1).toChar
  def isAlphabetic                = jl.Character isAlphabetic ch
  def isDigit                     = jl.Character isDigit ch
  def toUpper                     = jl.Character toUpperCase ch
  def toLower                     = jl.Character toLowerCase ch
  def isUpper                     = jl.Character isUpperCase ch
  def isLower                     = jl.Character isLowerCase ch
  def isWhitespace                = jl.Character isWhitespace ch
  def isControl                   = jl.Character isISOControl ch
  def isLetter                    = jl.Character isLetter ch
  def getType                     = jl.Character getType ch
  def to(end: Char): Direct[Char] = ch.toInt to end.toInt mapNow (_.toChar)
}

final class BooleanOps(val self: Boolean) extends AnyVal {
  def toInt: Int   = if (self) 1 else 0
  def toLong: Long = if (self) 1L else 0L
  def || (that: => Unit): Boolean = self || andFalse(that)
}

final class IntOps(val self: Int) extends AnyVal {
  private type This = Int

  def offset: Offset = Offset(self)
  def size: IntSize  = api.Size(self)

  /** Make a 64-bit long by concatenating two 32-bit Ints.
   *  Retrieve the original Ints with left32 and right32.
   */
  @inline def join64(that: Int): Long = (self.toLong << 32) | that.toLong

  def abs: This               = scala.math.abs(self)
  def max(that: This): This   = scala.math.max(self, that)
  def min(that: This): This   = scala.math.min(self, that)
  def signum: This            = scala.math.signum(self)
  def lowerBound(n: Int): Int = max(n)
  def zeroPlus: Int           = lowerBound(0)

  def isOr(p: Int => Boolean)(alt: => Int): Int = if (p(self)) self else alt

  def until(end: Int): IntRange        = Consecutive.until(self, end)
  def to(end: Int): IntRange           = Consecutive.to(self, end)
  def downTo(end: Int): Direct[Int]    = Consecutive.downTo(self, end)
  def takeNext(len: Precise): IntRange = Consecutive.until(self, self + len.getInt)

  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)
}

final class LongOps(val self: Long) extends AnyVal {
  private type This = Long

  def nth: Nth      = Nth(self)
  def index: Index  = Index(self)
  def size: LongSize = api.Size(self)

  def abs: This                 = scala.math.abs(self)
  def max(that: This): This     = scala.math.max(self, that)
  def min(that: This): This     = scala.math.min(self, that)
  def signum: This              = scala.math.signum(self)
  def lowerBound(n: Long): Long = max(n)
  def zeroPlus: Long            = lowerBound(0)
  def isNonZero: Boolean        = self != 0L

  def left32: Int  = (self >>> 32).toInt
  def right32: Int = self.toInt

  /** Safe in the senses that it won't silently truncate values, and
   *  will translate MaxLong to MaxInt instead of -1.
   */
  def safeInt: Int = self match {
    case MaxLong => MaxInt
    case MinLong => MinInt
    case _       =>
      assert(self <= (MaxInt: Long), s"$self < $MaxInt")
      self.toInt
  }

  def binary: String = jl.Long.toBinaryString(self)
  def hex: String    = jl.Long.toHexString(self)
  def octal: String  = jl.Long.toOctalString(self)
}
