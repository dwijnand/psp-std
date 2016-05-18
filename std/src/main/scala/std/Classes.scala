package psp
package std

import api._, all._

/** Motley objects for which a file of residence is not obvious.
  */
class FormatFun(val fmt: String) extends (Any => String) with ShowSelf {
  def apply(x: Any): String = stringFormat(fmt, x)
  def to_s                  = fmt
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
final class Utf8(val bytes: Array[Byte]) extends AnyVal with ShowSelf {
  def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
  def to_s: String       = new String(chars)
}
object StdShow extends ShowInstances
object Unsafe {
  implicit def promoteIndex(x: Long): Index = Index(x)
  implicit def inheritedShow[A]: Show[A]    = inheritShow
}

object +: {
  def unapply[A](xs: View[A]): Opt[A -> View[A]] = zcond(!xs.isEmpty, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Opt[View[A] -> A] = zcond(!xs.isEmpty, some(xs.init -> xs.last))
}

sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A]            extends <:<[A, A] { def apply(x: A): A = x }

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction[_, _] => f isDefinedAt x
    case _                              => true
  }
  def apply(x: T): R    = f(x)
  override def toString = to_s
}
