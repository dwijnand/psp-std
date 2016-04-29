package psp
package std

import api._, all._, StdEq._
import java.nio.file.{ attribute => jnfa }
import java.net.URLClassLoader
import scala.reflect.NameTransformer

/** Wrapper for java.lang.Class/ClassLoader and other java friends.
 */
object NullClassLoader extends jClassLoader
object NullInputStream extends InputStream { def read(): Int = -1 }

final class JavaClassImpl(val clazz: jClass) extends AnyVal with ShowSelf with JavaClass
final class JavaClassLoaderImpl(val loader: jClassLoader) extends AnyVal with ShowSelf with JavaClassLoader
// final case class JavaMapEntry[K, V](pair: K -> V) extends jMapEntry[K, V] with (K -> V) {
//   def _1                = fst(pair)
//   def _2                = snd(pair)
//   def getKey            = _1
//   def getValue          = _2
//   def setValue(x: V): V = unsupportedOperationException("setValue")
// }

class JavaEnumeration[A](enum: jEnumeration[A]) {
  def iterator: BiIterator[A] = BiIterator enumeration enum
}

final case class JvmName(value: String) {
  def segments: Vec[String] = value splitChar '.'
  def short: String         = segments.last
}

object JavaClass {
  def apply(x: jClass): JavaClass = new JavaClassImpl(x)
}

trait JavaClass extends Any {
  def clazz: jClass

  private def toPsp(x: jClass): JavaClass = new JavaClassImpl(x)
  def javaClass: JavaClass = this // implicit creation hook

  def isAnnotation     = clazz.isAnnotation
  def isAnonymousClass = clazz.isAnonymousClass
  def isArray          = clazz.isArray
  def isEnum           = clazz.isEnum
  def isInterface      = clazz.isInterface
  def isLocalClass     = clazz.isLocalClass
  def isMemberClass    = clazz.isMemberClass
  def isPrimitive      = clazz.isPrimitive
  def isSynthetic      = clazz.isSynthetic

  def rawName: JvmName   = new JvmName(clazz.getName)
  def scalaName: JvmName = new JvmName(clazz.getName.mapSplit('.')(NameTransformer.decode))

  def ancestorNames: Vec[JvmName]        = ancestors mapNow (_.rawName)
  def ancestors: Vec[JavaClass]          = transitiveClosure(this)(_.parents).toVec
  def exists: Boolean                    = clazz != null
  def fields: Vec[jField]                = clazz.getFields.toVec
  def getCanonicalName: String           = clazz.getCanonicalName
  def getClassLoader: ClassLoader        = clazz.getClassLoader
  def getClasses: Vec[JavaClass]         = clazz.getClasses.toVec mapNow toPsp
  def getComponentType: JavaClass        = clazz.getComponentType
  def getDeclaredClasses: Vec[JavaClass] = clazz.getDeclaredClasses.toVec mapNow toPsp
  def getDeclaringClass: JavaClass       = clazz.getDeclaringClass
  def getEnclosingClass: JavaClass       = clazz.getEnclosingClass
  def getInterfaces: Vec[JavaClass]      = clazz.getInterfaces.toVec mapNow toPsp
  def getSuperclass: Option[JavaClass]   = Option(clazz.getSuperclass) map toPsp
  def hasModuleName: Boolean             = rawName.value endsWith "$"
  def methods: Vec[jMethod]              = clazz.getMethods.toVec
  def parentInterfaces: Vec[JavaClass]   = clazz.getInterfaces.toVec mapNow toPsp
  def parents: Vec[JavaClass]            = getSuperclass.toVec ++ parentInterfaces
  def shortName: String                  = scalaName.short
  def to_s: String                       = s"$clazz"
}

trait JavaClassLoader extends Any {
  def loader: jClassLoader

  def parentChain: Each[jClassLoader] = {
    def loop(cl: jClassLoader): View[jClassLoader] = cl match {
      case null => emptyValue
      case _    => cl +: loop(cl.getParent)
    }
    loop(loader)
  }
  def uris: Each[jUri] = loader match {
    case cl: URLClassLoader => cl.getURLs map (_.toURI)
    case _                  => vec()
  }
  def to_s: String = s"$loader"
}

/** Static forwarder objects.
 */

object FileTime {
  val NoFileTime                        = this fromMillis MinLong
  def empty: FileTime                   = NoFileTime
  def fromMillis(value: Long): FileTime = jnfa.FileTime fromMillis value
}
