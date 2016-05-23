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
  type Vec[A]               = Direct[A]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def orderBy[A] = new Relation.OrderBy[A]
  def showBy[A]  = new Relation.ShowBy[A]
  def hashBy[A]  = new Relation.HashBy[A]

  /** Unfortunately we need the overloads for function type inference,
    * e.g. `view(1, 2) map index` doesn't work if only Long is here.
    */
  def nth(n: Int): Nth          = Nth(n)
  def nth(n: Long): Nth         = Nth(n)
  def index(n: Int): Index      = Index(n)
  def index(n: Long): Index     = Index(n)
  def precise(n: Int): Precise  = Size(n)
  def precise(n: Long): Precise = Size(n)

  def classFilter[A: CTag]: Any ?=> A              = Fun.partial(isInstance[A], cast[A])
  def classNameOf(x: Any): String                  = JvmName asScala x.getClass short
  def lformat[A](n: Int): A => String              = stringFormat(cond(n <= 0, "%s", new Pstring("%%-%ds") format n), _)
  def println[A: Show](x: A): Unit                 = scala.Console.out println render(x)
  def render[A](x: A)(implicit z: Show[A]): String = z show x
}
