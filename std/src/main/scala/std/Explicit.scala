package psp
package std

import psp.api._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

abstract class AllExplicit extends ApiValues with StdEq with StdTypeClasses {
  final val ->        = Pair
  final val Array     = scala.Array
  final val Failure   = scala.util.Failure
  final val Nil       = scala.collection.immutable.Nil
  final val NoFile    = jFile("")
  final val NoIndex   = Index.invalid
  final val NoPath    = jPath("")
  final val NoUri     = jUri("")
  final val None      = scala.None
  final val Option    = scala.Option
  final val Some      = scala.Some
  final val Success   = scala.util.Success
  final val Try       = scala.util.Try
  final val sciList   = sci.List
  final val sciMap    = sci.Map
  final val sciSeq    = sci.Seq
  final val sciSet    = sci.Set
  final val sciVector = sci.Vector
  final val scmMap    = scm.Map

  final val ConstantFalse  = (x: scala.Any) => false
  final val ConstantTrue   = (x: scala.Any) => true
  final val ConstantFalse2 = (x: scala.Any, y: scala.Any) => false
  final val ConstantTrue2  = (x: scala.Any, y: scala.Any) => true

  // Type aliases I don't like enough to have in the API.
  type Bag[A]               = Pmap[A, Precise]
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type VdexRange            = ClosedRange[Vdex]
  type IntRange             = ClosedRange[Int]
  type LongRange            = ClosedRange[Long]
  type CharRange            = ClosedRange[Char]
  type OpenRange[+A]        = Consecutive.Open[A]
  type ClosedRange[+A]      = Consecutive.Closed[A]
  type Renderer             = Show[Doc]
  type View2D[+A]           = View[View[A]]
  type HashFun[+A]          = Fun[Long, View[A]]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def eqBy[A]    = new EqBy[A]
  def orderBy[A] = new OrderBy[A]
  def showBy[A]  = new ShowBy[A]
  def hashBy[A]  = new HashBy[A]

  def order[A](implicit z: Order[A]): Order[A] = z

  /** Unfortunately we need the overloads for function type
   *  inference.
   */
  def nth(n: Int): Nth      = Nth(n)
  def nth(n: Long): Nth     = Nth(n)
  def index(n: Int): Index  = Index(n)
  def index(n: Long): Index = Index(n)

  def byEquals[A]: Hash[A]    = Eq.Inherited
  def byReference[A]: Hash[A] = Eq.Reference
  def byToString[A]: Hash[A]  = Eq.ToString

  def classFilter[A : CTag]: Any ?=> A             = Fun.partial(isInstance[A], cast[A])
  def classNameOf(x: Any): String                  = JvmName asScala x.getClass short
  def inheritShow[A]: Show[A]                      = Show.Inherited
  def lformat[A](n: Int): FormatFun                = new FormatFun(cond(n <= 0, "%s", new Pstring("%%-%ds") format n))
  def println[A : Show](x: A): Unit                = scala.Console.out println render(x)
  def render[A](x: A)(implicit z: Show[A]): String = z show x

  def make[R] : MakeHelper[R]           = new MakeHelper[R]
  def remake[R](xs: R): RemakeHelper[R] = new RemakeHelper[R](xs)

  def bufferMap[A, B : Empty](): scmMap[A, B] = scmMap[A, B]() withDefaultValue emptyValue[B]

  def closedRange[A](start: Long, size: Precise)(f: Long => A): ClosedRange[A] = Interval.closed(start, size) map f
  def indexRange(start: Long, end: Long): VdexRange                            = Interval.until(start, end) map Index
  def nthInclusive(start: Long, end: Long): VdexRange                          = Interval.to(start, end) map (n => Nth(n))
  def longRange(start: Long, end: Long): LongRange                             = Interval.until(start, end) map identity
  def longsFrom(start: Long): OpenRange[Long]                                  = openRange(start)(identity)
  def openIndices: OpenRange[Index]                                            = openRange(0)(Index)
  def openRange[A](start: Long)(f: Long => A): OpenRange[A]                    = Interval open start map f

  def zipCross[A, B](l: View[A], r: View[B]): Zip[A, B]                        = new Zip.ZipCross(l, r)
  def zipSplit[R, A, B](xs: View[R])(implicit z: Splitter[R, A, B]): Zip[A, B] = new Zip.ZipSplit(xs)
  def zipPairs[A, B](xs: View[A -> B]): Zip[A, B]                              = new Zip.ZipPairs(xs)
  def zipViews[A, B](l: View[A], r: View[B]): Zip[A, B]                        = new Zip.ZipViews(l, r)
  def zipMap[A, B](l: View[A], f: A => B): Zip[A, B]                           = new Zip.ZipMap(l, f)

  def funGrid[A, B](xs: View[A])(columns: (A => B)*): View2D.FunGrid[A, B] = new View2D.FunGrid(xs, view(columns: _*))

  import Builders._

  def builds[A, R](f: View[A] => R): Builds[A, R]      = new Builds(f)
  def elems[A, R](xs: A*)(implicit z: Builds[A, R]): R = z build Each.elems(xs: _*)

  def inView[A](mf: Suspended[A]): View[A]                           = new IdView(Each(mf))
  def intoView[A, R](xs: R)(implicit z: ViewsAs[A, R]): IdView[A, R] = z viewAs xs
  def lazyView[A](expr: => View[A]): View[A]                         = inView(expr foreach _)
  def rview[A, R](xs: A*): IdView[A, R]                              = new IdView(elems(xs: _*))
  def view[A](xs: A*): View[A]                                       = new IdView(Each.elems(xs: _*))
  def viewsAs[R, A](f: R => Each[A]): ViewsAs[A, R]                  = new ViewsAs(x => new IdView(f(x)))

  def arr[A : CTag](xs: A*): Array[A]     = xs.toArray[A]
  def vec[A](xs: A*): Vec[A]              = elems(xs: _*)
  def zip[A, B](xs: (A -> B)*): Zip[A, B] = zipPairs(view(xs: _*))

  def pmap[A : Hash, B](xs: (A->B)*): Pmap[A, B] = elems(xs: _*)
  def pset[A : Hash](xs: A*): Pset[A]            = elems(xs: _*)
  def plist[A](xs: A*): Plist[A]                 = elems(xs: _*)
  def pnil[A](): Plist[A]                        = cast(Pnil)

  def scalaList[A](xs: A*): sciList[A]            = elems(xs: _*)
  def scalaMap[K, V](xs: (K -> V)*): sciMap[K, V] = elems(xs: _*)
  def scalaSet[A](xs: A*): sciSet[A]              = elems(xs: _*)

  def javaList[A](xs: A*): jList[A]               = elems(xs: _*)
  def javaMap[K, V](xs: (K -> V)*): jMap[K, V]    = elems(xs: _*)
  def javaSet[A](xs: A*): jSet[A]                 = elems(xs: _*)
}
