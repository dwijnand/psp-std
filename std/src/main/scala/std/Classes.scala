package psp
package std

import api._, all._

/** Motley objects for which a file of residence is not obvious.
 */
class FormatFun(val fmt: String) extends (Any => String) with ShowSelf {
  def apply(x: Any): String = stringFormat(fmt, x)
  def to_s = fmt
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

object StdEq extends EqOrderInstances
object StdShow extends ShowInstances {
  implicit def convertHasShowDocOps[A: Show](x: A): ops.DocOps      = new ops.DocOps(Doc(x))
  implicit def convertHasShowDoc[A](x: A)(implicit z: Show[A]): Doc = Doc(x)
}
object Unsafe {
  // implicit def inheritedEq[A] : Hash[A]       = inheritEq
  implicit def promoteIndex(x: Long): Index      = Index(x)
  implicit def inheritedShow[A] : Show[A]        = inheritShow
  // implicit def shownOrder[A: Show] : Order[A] = orderBy[A](render[A])
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y)))                     }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]   = Show[A](f andThen z.show)                                 }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]   = Eq.hash[A]((x, y) => z.eqv(f(x), f(y)))(x => z hash f(x)) }
final class EqBy[A]    { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]       = Eq[A]((x, y) => z.eqv(f(x), f(y)))                        }

object +: {
  def unapply[A](xs: View[A]): Option[A -> View[A]] =
    cond(xs.isEmpty, none, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Option[View[A] -> A] =
    cond(xs.isEmpty, none, some(xs.init -> xs.last))
}
object Java {
  implicit def javaSetBuilder[A]: Builds[A, jSet[A]]            = genericJavaSetBuilder(new jHashSet[A])
  implicit def javaListBuilder[A]: Builds[A, jList[A]]          = genericJavaListBuilder(new jArrayList[A])
  implicit def javaMapBuilder[K, V]: Builds[K -> V, jMap[K, V]] = genericJavaMapBuilder(new jHashMap[K, V])

  def genericJavaListBuilder[A, M[A] <: jList[A]](z: M[A]): Builds[A, M[A]]                 = Builds(xs => doto(z)(z => xs foreach (x => z add x)))
  def genericJavaSetBuilder[A, M[A] <: jSet[A]](z: M[A]): Builds[A, M[A]]                   = Builds(xs => doto(z)(z => xs foreach (x => z add x)))
  def genericJavaMapBuilder[K, V, M[K, V] <: jMap[K, V]](z: M[K, V]): Builds[K->V, M[K, V]] = Builds(xs => doto(z)(z => xs foreach (x => z.put(fst(x), snd(x)))))

  def List[A](xs: A*): jList[A]            = Built(xs)
  def Map[K, V](xs: (K -> V)*): jMap[K, V] = Built(xs)
  def Set[A](xs: A*): jSet[A]              = Built(xs)
}
