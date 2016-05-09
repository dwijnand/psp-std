package psp
package std

import api._, all._

/** Motley objects for which a file of residence is not obvious.
  */
class FormatFun(val fmt: String) extends (Any => String) with ShowSelf {
  def apply(x: Any): String = stringFormat(fmt, x)
  def to_s                  = fmt
}
class Partial[A, B](p: ToBool[A], f: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A): Boolean            = p(x)
  def apply(x: A): B                        = f(x)
  def applyOr(x: A, alt: => B): B           = if (p(x)) f(x) else alt
  def zapply(x: A)(implicit z: Empty[B]): B = applyOr(x, z.empty)
}
object Partial {
  implicit def liftPartial[A, B](pf: A ?=> B): Partial[A, B] = apply(pf)
  def apply[A, B](pf: A ?=> B): Partial[A, B]                = apply(pf isDefinedAt _, pf apply _)
  def apply[A, B](p: ToBool[A], f: A => B): Partial[A, B]    = new Partial(p, f)
}
final class JvmName(val raw: String) extends ShowSelf {
  def segments: Vec[String] = raw splitChar '.'
  def short: String         = segments.last
  def to_s: String          = raw
}
object JvmName {
  import scala.reflect.NameTransformer.decode

  def asJava(clazz: jClass): JvmName  = new JvmName(clazz.getName)
  def asScala(clazz: jClass): JvmName = new JvmName(clazz.getName.mapSplit('.')(decode))
}
// final class Utf8(val bytes: Array[Byte]) extends AnyVal with ShowSelf {
//   def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
//   def to_s: String       = new String(chars)
// }

trait StdEq0 {
  implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] = Order.fromInt[A](_ compareTo _)
}
trait StdEq extends StdEq0 {
  implicit def eqUnbuilds[R, A](implicit b: UnbuildsAs[A, R], z: Eq[A]): Eq[R] =
    Eq[R]((xs, ys) => (b unbuild xs).view zip (b unbuild ys).view corresponds z.eqv)

  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A] =
    orderBy[A](_.ordinal)

  implicit def boolOrder: Order[Bool]                           = orderBy[Bool](x => if (x) 1 else 0)
  implicit def charOrder: Order[Char]                           = Order.fromInt[Char](_ - _)
  implicit def intOrder: Order[Int]                             = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]                           = Order.fromLong[Long](_ - _)
  implicit def vindexOrder: Order[Vdex]                         = orderBy[Vdex](_.indexValue)
  implicit def preciseOrder: Order[Precise]                     = orderBy[Precise](_.get)
  implicit def stringOrder: Order[String]                       = Order.fromLong[String](_ compareTo _)
  implicit def tuple2Order[A : Order, B : Order]: Order[(A, B)] = orderBy[(A, B)](fst) | snd

  implicit def classEq: Hash[Class[_]] = byEquals
  implicit def sizeEq: Hash[Size]      = byEquals

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }
}
object StdShow extends ShowInstances
object Unsafe {
  // implicit def inheritedEq[A] : Hash[A]       = inheritEq
  implicit def promoteIndex(x: Long): Index = Index(x)
  implicit def inheritedShow[A]: Show[A]    = inheritShow
  // implicit def shownOrder[A: Show] : Order[A] = orderBy[A](render[A])
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y))) }
final class ShowBy[A] { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]    = Show[A](f andThen z.show) }
final class HashBy[A] { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]    = Eq.hash[A]((x, y) => z.eqv(f(x), f(y)))(x => z hash f(x)) }
final class EqBy[A] { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]          = Eq[A]((x, y) => z.eqv(f(x), f(y))) }

object +: {
  def unapply[A](xs: View[A]): Option[A -> View[A]] =
    cond(xs.isEmpty, none, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Option[View[A] -> A] =
    cond(xs.isEmpty, none, some(xs.init -> xs.last))
}

sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A]            extends <:<[A, A] { def apply(x: A): A = x }

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction [_, _] => f isDefinedAt x
    case _                               => true
  }
  def apply(x: T): R    = f(x)
  override def toString = to_s
}
