package psp
package std

import all._

/** Holding area for things which should later be made configurable.
 */
object Pconfig {
  val renderer = new FullRenderer(3L sizeTo 9)
}

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
class LabeledFunction[-T, +R](f: T => R, str: () => String) extends (T ?=> R) with ShowSelf {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction[_, _] => f isDefinedAt x
    case _                              => true
  }
  def apply(x: T): R = f(x)
  def to_s: String   = str()
}
final class ZipIterator[A1, A2](ls: scIterator[A1], rs: scIterator[A2]) extends scIterator[A1 -> A2] {
  def hasNext: Bool    = ls.hasNext && rs.hasNext
  def hasMore: Bool    = ls.hasNext || rs.hasNext
  def next(): A1 -> A2 = ls.next -> rs.next
}

/** Various objects for construction, extraction, alias storage.
 */
object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: MakesProduct[R, A, B]): R     = z.join(pair(x, y))
  def unapply[R, A, B](x: R)(implicit z: IsProduct[R, A, B]): Some[A -> B] = scala.Some(z split x)
}
object :: {
  def apply[R, A, B](x: A, y: B)(implicit z: MakesProduct[R, A, B]): R = Pair(x, y)
  def unapply[R, A, B](x: R)(implicit z1: IsProduct[R, A, B], z2: Empty[R], z3: Eq[R]): Option[A -> B] =
    if (z3.eqv(x, z2.empty)) none() else some(z1 split x)
}
object IsClass {
  def unapply[A: CTag](x: Any): Option[A] = classFilter[A] lift x
}
object +: {
  def unapply[A](xs: View[A]): Opt[A -> View[A]] = zcond(!xs.isEmpty, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Opt[View[A] -> A] = zcond(!xs.isEmpty, some(xs.init -> xs.last))
}

/** A valid index is always non-negative. All negative indices are
  *  mapped to NoIndex, which has an underlying value of -1.
  *  Manipulations of invalid values remain invalid, like NaN.
  *  All valid indices give rise to a corresponding Nth which is
  *  one larger, i.e. Index(3) is equivalent to Nth(4).
  */
object Index extends (Long => Index) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get < 0 }

  def invalid: Index            = new Index(-1L)
  def apply(value: Long): Index = if (value < 0) invalid else new Index(value)
  def unapply(x: Vdex)          = new Extractor(x.indexValue)
}

/** Nth is a 1-based index.
  */
object Nth extends (Long => Nth) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get <= 0 }

  def invalid: Nth            = new Nth(-1L)
  def apply(value: Long): Nth = if (value <= 0) invalid else new Nth(value - 1)
  def unapply(x: Vdex)        = new Extractor(x.nthValue)
}
