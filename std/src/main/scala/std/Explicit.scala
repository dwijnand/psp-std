package psp
package std

import scala.collection.{ mutable => scm }

abstract class AllExplicit extends ApiValues with StdRelation with StdSplitZip with StdConstructors {
  final val ->      = Pair
  final val Array   = scala.Array
  final val Failure = scala.util.Failure
  final val None    = scala.None
  final val Some    = scala.Some
  final val Success = scala.util.Success
  final val Try     = scala.util.Try
  final val Tuple2  = scala.Tuple2
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
  type SizeRange            = ClosedRange[Precise]
  type VdexRange            = ClosedRange[Vdex]
  type View2D[+A]           = View[View[A]]
  type Vec[A]               = Direct[A]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def hashBy[A]  = new Relation.HashBy[A]

  def classFilter[A: CTag]: Any ?=> A              = Fun.partial(isInstance[A], cast[A])
  def classNameOf(x: Any): String                  = JvmName asScala x.getClass short
  def lformat[A](n: Int): A => String              = stringFormat(cond(n <= 0, "%s", new Pstring("%%-%ds") format n), _)
  def println[A: Show](x: A): Unit                 = scala.Console.out println render(x)
  def render[A](x: A)(implicit z: Show[A]): String = z show x
}
