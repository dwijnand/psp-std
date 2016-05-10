package psp
package std

/** The organization of the implicit builders is a black art.
  *  It might be better to generate StdBuilds[0-15] and put an implicit in
  *  each one so the prioritization is as unambiguous as scala allows.
  *
  *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
  *  around and it lets us build scala collections at the end of a view with no more code.
  *  Scala maps are built with Tuples even though Product2 should suffice; the types
  *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
  *  The rest of the time one should write it K -> V.
  */
import api._, all._

final class Conversions[A](val xs: View[A]) extends AnyVal with ConversionsMethods[A]

/** Conversions which require the elements to be pairs. Obtaining evidence of that
  *  up front simplifies everything else, because we don't have to mix and match
  *  between arity-1 and arity-2 type constructors.
  */
class PairConversions[R, A, B](val xs: View[R])(implicit sp: Splitter[R, A, B]) {
  def toExMap(implicit z: Eq[A]): ExMap[A, B]                         = toMap[ExMap]
  def toMap[CC[_, _]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z contraMap sp.split build xs
}

trait ConversionsMethods[A] extends Any {
  def xs: View[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs

  def iterator: scIterator[A]                  = toScalaStream.iterator
  def toArray(implicit z: CTag[A]): Array[A]   = to[Array]
  def toExSet(implicit z: Eq[A]): ExSet[A]     = to[ExSet]
  def toHashSet(implicit z: Hash[A]): ExSet[A] = to[ExSet]
  def toJavaSet: jSet[A]                       = to[jSet]
  def toPlist: Plist[A]                        = to[Plist]
  def toScalaStream: sciStream[A]              = to[sciStream]
  def toScalaSet: sciSet[A]                    = to[sciSet]
  def toScalaVector: sciVector[A]              = to[sciVector]
  def toVec: Vec[A]                            = to[Vec]
}

final class ViewsAs[A, R](val f: R => Foreach[A]) extends AnyVal {
  def viewAs(xs: R): IdView[A, R] = new IdView(f(xs))
}
final class HasViewsAs[A, R](repr: R)(implicit z: ViewsAs[A, R]) {
  def m: IdView[A, R] = z viewAs repr
}

final class Builds[-Elem, +To](val f: Foreach[Elem] => To) {
  def contraMap[A](g: A => Elem): Builds[A, To]    = builds(xs => f(Each(mf => xs foreach (g andThen mf))))
  def map[Next](g: To => Next): Builds[Elem, Next] = builds(f andThen g)
  def build(xs: Foreach[Elem]): To                 = f(xs)
  def apply(mf: Suspended[Elem]): To               = f(Each(mf))
  def scalaBuilder: scmBuilder[Elem, To]           = sciVector.newBuilder[Elem] mapResult (xs => build(xs.m))
}

trait JavaBuilders0 {
  protected def forJava[A, M[X] <: jCollection[X]](empty: M[A]): Builds[A, M[A]] =
    builds(xs => doto(empty)(r => xs foreach (r add _)))

  protected def forJavaMap[K, V, M[X, Y] <: jAbstractMap[X, Y]](empty: M[K, V]): Builds[K -> V, M[K, V]] =
    builds(xs => doto(empty)(r => xs foreach (x => r.put(fst(x), snd(x)))))

  implicit def buildJavaSet[A]: Builds[A, jSet[A]] = forJava(new jHashSet[A])
}
trait JavaBuilders extends JavaBuilders0 {
  implicit def buildJavaList[A]: Builds[A, jList[A]]          = forJava(new jArrayList[A])
  implicit def buildJavaMap[K, V]: Builds[K -> V, jMap[K, V]] = forJavaMap(new jHashMap[K, V])
}
trait ScalaBuilders0 extends JavaBuilders {
  implicit def forScala[A, That](implicit z: CanBuild[A, That]): Builds[A, That] =
    builds(xs => doto(z())(b => xs foreach (b += _)).result)
}
trait ScalaBuilders extends ScalaBuilders0 {
  implicit def forScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] =
    forScala contraMap (x => pair(fst(x), snd(x)))
}
trait JvmBuilders0 extends ScalaBuilders {
  implicit def buildJvmArray[A : CTag]: Builds[A, Array[A]] = builds(xs => doto(Array.newBuilder[A])(_ ++= xs.trav).result)
}
trait JvmBuilders extends JvmBuilders0 {
  implicit def buildJvmString: Builds[Char, String] = builds(xs => doto(new StringBuilder)(b => xs foreach (c => b append c)) toString)
}
trait PspBuilders0 extends JvmBuilders {
  implicit def buildPspSet[A : Eq]: Builds[A, ExSet[A]]            = Builds.scalaSet[A] map (xs => new Pset(xs))
  implicit def buildPspMap[K : Eq, V]: Builds[K -> V, ExMap[K, V]] = builds(_.view.zipped.force) // XXX
}
trait PspBuilders1 extends PspBuilders0 {
  implicit def buildPspList[A]: Builds[A, Plist[A]] = builds(xs => ll.foldRight[A, Plist[A]](xs, cast(Pnil), _ :: _))
}
trait PspBuilders extends PspBuilders1 {
  implicit def buildPspVec[A]: Builds[A, Vec[A]] = Builds.scalaVector[A] map (xs => new Vec(xs))
}
trait Builders extends PspBuilders
object Builders extends Builders

object Builds {
  def javaList[A]: Builds[A, jList[A]]               = ?
  def javaMap[K, V]: Builds[K -> V, jMap[K, V]]      = ?
  def javaSet[A]: Builds[A, jSet[A]]                 = ?
  def jvmArray[A : CTag]: Builds[A, Array[A]]        = ?
  def jvmString: Builds[Char, String]                = ?
  def pspList[A]: Builds[A, Plist[A]]                = ?
  def pspMap[K : Eq, V]: Builds[K -> V, ExMap[K, V]] = ?
  def pspSet[A : Eq]: Builds[A, ExSet[A]]            = ?
  def pspVec[A]: Builds[A, Vec[A]]                   = ?
  def scalaList[A]: Builds[A, sciList[A]]            = ?
  def scalaMap[K, V]: Builds[K -> V, sciMap[K, V]]   = ?
  def scalaSet[A]: Builds[A, sciSet[A]]              = ?
  def scalaVector[A]: Builds[A, sciVector[A]]        = ?
}
