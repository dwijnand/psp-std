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
import api._, all._, scala.Tuple2
import Builds._

trait JavaBuilders0 {
  implicit def buildJavaSet[A]: Builds[A, jSet[A]] = Builds.javaSet
}
trait JavaBuilders extends JavaBuilders0 {
  implicit def buildJavaList[A]: Builds[A, jList[A]]          = Builds.javaList
  implicit def buildJavaMap[K, V]: Builds[K -> V, jMap[K, V]] = Builds.javaMap
}
trait ScalaBuilders0 extends JavaBuilders {
  implicit def forScala[A, That](implicit z: CanBuild[A, That]): Builds[A, That] = Builds.forScala
}
trait ScalaBuilders extends ScalaBuilders0 {
  implicit def forScalaMap[K, V, That](implicit z: CanBuild[Tuple2[K, V], That]): Builds[K -> V, That] = Builds.forScalaMap
}
trait JvmBuilders0 extends ScalaBuilders {
  implicit def buildJvmArray[A: CTag]: Builds[A, Array[A]] = jvmArray[A]
}
trait JvmBuilders extends JvmBuilders0 {
  implicit def buildJvmString: Builds[Char, String] = jvmString
}
trait PspBuilders0 extends JvmBuilders {
  // XXX higher priority :Hash variants.
  implicit def buildPspSet[A: Eq]: Builds[A, Pset[A]]            = pspSet[A]
  implicit def buildPspMap[K: Eq, V]: Builds[K -> V, Pmap[K, V]] = pspMap[K, V]
}
trait PspBuilders1 extends PspBuilders0 {
  implicit def buildPspList[A]: Builds[A, Plist[A]] = pspList[A]
}
trait PspBuilders extends PspBuilders1 {
  implicit def buildPspVec[A]: Builds[A, Vec[A]] = pspVec[A]
}
trait StdBuilders extends PspBuilders
object StdBuilders extends StdBuilders

trait StdViewers0 {
  implicit def viewsAsPspSet[A, CC[X] <: Pset[X]]: ViewsAs[A, CC[A]]            = viewsAs(Each each _.basis)
  implicit def viewsAsJavaIterable[A, CC[X] <: jIterable[X]]: ViewsAs[A, CC[A]] = viewsAs(Each java _)
  implicit def viewsAsScala[A, CC[X] <: sCollection[X]]: ViewsAs[A, CC[A]]      = viewsAs(Each scala _)
}
trait StdViewers extends StdViewers0 with StdBuilders {
  implicit def viewsAsJavaMap[K, V, CC[X, Y] <: jMap[X, Y]]: ViewsAs[K -> V, CC[K, V]]   = viewsAs(Each javaMap _)
  implicit def viewsAsJvmArray[A]: ViewsAs[A, Array[A]]                                  = viewsAs(Each array _)
  implicit def viewsAsJvmString: ViewsAs[Char, String]                                   = viewsAs(Each jvmString _)
  implicit def viewsAsPspEach[A, CC[X] <: Each[X]]: ViewsAs[A, CC[A]]                    = viewsAs(identity)
  implicit def viewsAsPspView[A, CC[X] <: View[X]]: ViewsAs[A, CC[A]]                    = viewsAs(xs => Each apply (xs foreach _))
  implicit def viewsAsScalaMap[K, V, CC[X, Y] <: scMap[X, Y]]: ViewsAs[K -> V, CC[K, V]] = viewsAs(Each scala _)
}

trait Converters0 {
  implicit def convertJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): IdView[A, CC[A]]           = intoView(xs)
  implicit def convertJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): IdView[K -> V, CC[K, V]] = intoView(xs)
  implicit def convertMonoView[A, R](xs: R)(implicit z: ViewsAs[A, R]): IdView[A, R]                = intoView(xs)
}
trait Converters1 extends Converters0 {
  implicit def convertScala[A, CC[X] <: sCollection[X]](xs: CC[A]): IdView[A, CC[A]] = intoView(xs)
}
trait Converters2 extends Converters1 {
  implicit def convertJvmArray[A](xs: Array[A]): IdView[A, Array[A]]            = intoView(xs)
  implicit def convertPspEach[A, CC[X] <: Each[X]](xs: CC[A]): IdView[A, CC[A]] = intoView(xs)
}
trait StdConverters extends Converters2


trait StdConstructors {
  import StdBuilders._

  def builds[A, R](f: View[A] => R): Builds[A, R]      = new Builds(f)
  def elems[A, R](xs: A*)(implicit z: Builds[A, R]): R = z build Each.elems(xs: _*)

  def inView[A](mf: Suspended[A]): View[A]                           = new IdView(Each(mf))
  def intoView[A, R](xs: R)(implicit z: ViewsAs[A, R]): IdView[A, R] = z viewAs xs
  def lazyView[A](expr: => View[A]): View[A]                         = inView(expr foreach _)
  def rview[A, R](xs: A*): IdView[A, R]                              = new IdView(elems(xs: _*))
  def view[A](xs: A*): View[A]                                       = new IdView(Each.elems(xs: _*))
  def viewsAs[R, A](f: R => Each[A]): ViewsAs[A, R]                  = new ViewsAs(x => new IdView(f(x)))

  def arr[A: CTag](xs: A*): Array[A]      = xs.toArray[A]
  def vec[A](xs: A*): Vec[A]              = elems(xs: _*)
  def zip[A, B](xs: (A -> B)*): Zip[A, B] = zipPairs(view(xs: _*))

  def pmap[A: Hash, B](xs: (A -> B)*): Pmap[A, B] = elems(xs: _*)
  def pset[A: Hash](xs: A*): Pset[A]              = elems(xs: _*)
  def plist[A](xs: A*): Plist[A]                  = elems(xs: _*)
  def pnil[A](): Plist[A]                         = cast(Pnil)

  def scalaList[A](xs: A*): sciList[A]            = elems(xs: _*)
  def scalaMap[K, V](xs: (K -> V)*): sciMap[K, V] = elems(xs: _*)
  def scalaSet[A](xs: A*): sciSet[A]              = elems(xs: _*)

  def javaList[A](xs: A*): jList[A]            = elems(xs: _*)
  def javaMap[K, V](xs: (K -> V)*): jMap[K, V] = elems(xs: _*)
  def javaSet[A](xs: A*): jSet[A]              = elems(xs: _*)
}
