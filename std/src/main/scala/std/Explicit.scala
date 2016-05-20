package psp
package std

import psp.api._
import scala.collection.{ mutable => scm }

abstract class AllExplicit extends ApiValues with StdRelation with StdSplitZip with StdConstructors {
  final val ->      = Pair
  final val Array   = scala.Array
  final val Failure = scala.util.Failure
  final val None    = scala.None
  final val Some    = scala.Some
  final val Success = scala.util.Success
  final val Try     = scala.util.Try
  final val scmMap  = scm.Map

  final val ConstantFalse  = (x: scala.Any) => false
  final val ConstantTrue   = (x: scala.Any) => true
  final val ConstantFalse2 = (x: scala.Any, y: scala.Any) => false
  final val ConstantTrue2  = (x: scala.Any, y: scala.Any) => true

  // Type aliases I don't like enough to have in the API.
  type BoolAlgebra[A]       = spire.Bool[A]
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type CharRange            = ClosedRange[Char]
  type ClosedRange[+A]      = Consecutive.Closed[A]
  type ConstDoc[X]          = Doc
  type ConstSize[X]         = Size
  type HashFun[+A]          = Fun[Long, View[A]]
  type Heyting[A]           = spire.Heyting[A]
  type IntRange             = ClosedRange[Int]
  type LongRange            = ClosedRange[Long]
  type OpenRange[+A]        = Consecutive.Open[A]
  type Renderer             = Show[Doc]
  type VdexRange            = ClosedRange[Vdex]
  type View2D[+A]           = View[View[A]]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def orderBy[A] = new Relation.OrderBy[A]
  def showBy[A]  = new Relation.ShowBy[A]
  def hashBy[A]  = new Relation.HashBy[A]

  /** Unfortunately we need the overloads for function type
    *  inference.
    */
  def nth(n: Int): Nth      = Nth(n)
  def nth(n: Long): Nth     = Nth(n)
  def index(n: Int): Index  = Index(n)
  def index(n: Long): Index = Index(n)

  def classFilter[A: CTag]: Any ?=> A              = Fun.partial(isInstance[A], cast[A])
  def classNameOf(x: Any): String                  = JvmName asScala x.getClass short
  def lformat[A](n: Int): A => String              = stringFormat(cond(n <= 0, "%s", new Pstring("%%-%ds") format n), _)
  def println[A: Show](x: A): Unit                 = scala.Console.out println render(x)
  def render[A](x: A)(implicit z: Show[A]): String = z show x

  def make[R]: Builds.MakeHelper[R]            = new Builds.MakeHelper[R]
  def remake[R](xs: R): Builds.RemakeHelper[R] = new Builds.RemakeHelper[R](xs)

  def bufferMap[A, B: Empty](): scmMap[A, B] = scmMap[A, B]() withDefaultValue emptyValue[B]

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
}
