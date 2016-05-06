package psp
package std
package ops


import api._, all._

/** Extension methods for scala library classes.
 *  We'd like to get away from all such classes,
 *  but scala doesn't allow it.
 */
final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def or(alt: => A): A                 = x getOrElse alt
  // def orFail(msg: String): A           = x getOrElse abort(msg)
  def toVec: Vec[A]                    = this zfold (x => vec(x))
  // def up[A1 >: A] : OptionOps[A1]      = new OptionOps(x)
  def zfold[B: Empty](f: A => B): B    = x.fold[B](emptyValue)(f)
  def zget(implicit z: Empty[A]): A    = x getOrElse z.empty
  def | (alt: => A): A                 = x getOrElse alt
  // def ||(alt: => A): Option[A]         = x orElse Some(alt)
}
final class TryOps[A](val x: Try[A]) extends AnyVal {
  def | (expr: => A): A       = x.toOption | expr
  // def || (expr: => A): Try[A] = x collectSelf { case Failure(_) => Try(expr) }
  def fold[B](f: Throwable => B, g: A => B): B = x match {
    case Success(x) => g(x)
    case Failure(t) => f(t)
  }
}
