package psp
package std

import java.{ lang => jl }
import api._, all._

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

/** Extension methods for scala library classes.
 *  We'd like to get away from all such classes,
 *  but scala doesn't allow it.
 */
final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def or(alt: => A): A              = x getOrElse alt
  def toVec: Vec[A]                 = this zfold (x => vec(x))
  def zfold[B: Empty](f: A => B): B = x.fold[B](emptyValue)(f)
  def zget(implicit z: Empty[A]): A = x getOrElse z.empty
  def | (alt: => A): A              = x getOrElse alt
}
final class TryOps[A](val x: Try[A]) extends AnyVal {
  def | (expr: => A): A = x.toOption | expr
  def fold[B](f: Throwable => B, g: A => B): B = x match {
    case Success(x) => g(x)
    case Failure(t) => f(t)
  }
}

/** Methods requiring us to have additional knowledge, by parameter or type class.
 *  We keep the interface simple and foolproof by establishing thet instance
 *  first and only offering the methods after that.
 *
 *  But.
 *
 *  This approach, so nice in principle, stretches scala's willingness to connect
 *  implicits past its abilities. It works on psp collections, but when we depend on
 *  an implicit to entire Viewville in the first place, then these methods become
 *  out of reach without an implicit call to .m to become a view.
 *
 *  The search continues.
 */
class HasEqOps[A](xs: View[A])(implicit z: Eq[A]) {
  def contains(x: A): Boolean = xs exists (_ === x)
  def distinct: View[A]       = xs.zfoldl[Vec[A]]((res, x) => if (new HasEqOps(res) contains x) res else res :+ x)
  def indexOf(x: A): Index    = xs indexWhere (_ === x)
  def toSet: ExSet[A]         = xs.toExSet

  // def indicesOf(x: A): View[Index]                = xs indicesWhere (_ === x)
  // def mapOnto[B](f: A => B): ExMap[A, B]          = toSet mapOnto f
  // def toBag: Bag[A]                               = xs groupBy identity map (_.size.getInt)
  // def without(x: A): View[A]                      = xs filterNot (_ === x)
  // def withoutEmpty(implicit z: Empty[A]): View[A] = this without z.empty
}
class HasHashOps[A](xs: View[A])(implicit z: Hash[A]) extends HasEqOps[A](xs)(z) {
  override def toSet: ExSet[A] = xs.toHashSet
}
