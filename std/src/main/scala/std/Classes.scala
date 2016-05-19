package psp
package std

import api._, all._

/** Motley objects for which a file of residence is not obvious.
  */

final class JvmName(val raw: String) extends ShowSelf {
  def segments: Vec[String] = raw splitChar '.' toVec
  def short: String         = segments.last
  def to_s: String          = raw
}
object JvmName {
  import scala.reflect.NameTransformer.decode

  def asJava(clazz: jClass): JvmName  = new JvmName(clazz.getName)
  def asScala(clazz: jClass): JvmName = new JvmName(clazz.getName.mapSplit('.')(decode))
}
final class Utf8(val bytes: Array[Byte]) extends ShowSelf {
  def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
  def to_s: String       = new String(chars)
}

sealed abstract class <:<[-From, +To] extends (From => To) { }
final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }

object +: {
  def unapply[A](xs: View[A]): Opt[A -> View[A]] = zcond(!xs.isEmpty, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Opt[View[A] -> A] = zcond(!xs.isEmpty, some(xs.init -> xs.last))
}

