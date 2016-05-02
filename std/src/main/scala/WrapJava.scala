package psp
package std

import api._, all._
import java.nio.file.{ attribute => jnfa }
import scala.reflect.NameTransformer.decode

final class JvmName(val raw: String) extends ShowSelf {
  def segments: Vec[String] = raw splitChar '.'
  def short: String         = segments.last
  def to_s: String          = raw
}
object JvmName {
  def asJava(clazz: jClass): JvmName  = new JvmName(clazz.getName)
  def asScala(clazz: jClass): JvmName = new JvmName(clazz.getName.mapSplit('.')(decode))
}
object FileTime {
  val NoFileTime                        = this fromMillis MinLong
  def empty: FileTime                   = NoFileTime
  def fromMillis(value: Long): FileTime = jnfa.FileTime fromMillis value
}
