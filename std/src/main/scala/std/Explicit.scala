package psp
package std

import scala.collection.{ mutable => scm }

abstract class AllExplicit extends ApiValues with StdRelation with StdConstructors {
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
  final val ConstantFalse2 = (x: Any, y: Any) => false
  final val ConstantTrue2  = (x: Any, y: Any) => true

  // Type aliases I don't like enough to have in the API.
  type BoolAlgebra[A]       = spire.Bool[A]
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type CharRange            = ClosedRange[Char]
  type ClosedRange[+A]      = Consecutive.Closed[A]
  type ConstDoc[X]          = Doc
  type ConstSize[X]         = Size
  type Coords               = PairOf[Index]
  type HashFun[+A]          = Fun[Long, View[A]]
  type Heyting[A]           = spire.Heyting[A]
  type IntRange             = ClosedRange[Int]
  type LongRange            = ClosedRange[Long]
  type OpenRange[+A]        = Consecutive.Open[A]
  type Renderer             = Show[Doc]
  type SizeRange            = ClosedRange[Precise]
  type SliceRange           = Consecutive[Index]
  type SliceRanges          = Vec[SliceRange]
  type Vec[A]               = Direct[A]

  type SplitView[A, R]   = RView[A, R]#Split
  type LiveView[A, B, R] = RView[A, R]#Live[B]
  type View2D[A]         = View[View[A]]
}
