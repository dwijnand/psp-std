package psp
package std

import api._, exp._ // no implicit conversions in this file

// 49 promoteSize
// 47 convertHasShowDoc
// 39 liftPair
// 24 apply
// 13 viewPspDirect
// 11 convertViewEach
// 11 const
// 10 applyNext
//  7 funToPartialFunction
//  7 convertViewBuilds
//  6 promoteIndex
//  5 viewPspString
//  4 viewScalaCollection
//  2 viewPspEach
//  2 constantPredicate
//  1 viewScalaIndexedSeq
//  1 viewPspArray
//  1 promoteApiExSet
//  1 long2bigInt
//  1 liftSeqPair
//  1 liftPositional
//  1 liftPartial
//  1 int2bigInt
trait AllImplicit extends scala.AnyRef
      with StdEmpty
      with StdImplicits
{
  self =>

  def id[A](value: Boolean)(implicit z: BooleanAlgebra[A]): A = cond(value, z.one, z.zero)

  // implicit def algebraConstant[A](value: Boolean)(implicit z: BooleanAlgebra[A]): A                  = cond(value, z.one, z.zero)

  implicit def conforms[A] : (A <:< A)                                                               = new conformance[A]
  // implicit def constantPredicate[A](value: Boolean): ToBool[A]                                       = cond(value, ConstantTrue, ConstantFalse)
  implicit def defaultRenderer: FullRenderer                                                         = new FullRenderer
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B                                     = f.toPartial
  implicit def opsDirect[A](xs: Direct[A]): DirectOps[A]                                             = new DirectOps(xs)
  implicit def opsForeach[A](xs: Foreach[A]): ForeachOps[A]                                          = new ForeachOps(xs)
  implicit def predicate1Algebra[A] : BooleanAlgebra[ToBool[A]]                                      = new Algebras.Predicate1Algebra[A]
  implicit def predicate2Algebra[A, B] : BooleanAlgebra[ToBool2[A, B]]                               = new Algebras.Predicate2Algebra[A, B]
  implicit def promoteSize(x: Long): Precise                                                         = Size(x)
  implicit def unbuildJavaIterable[A, CC[X] <: jIterable[X]] : UnbuildsAs[A, CC[A]]                  = Unbuilds[A, CC[A]](Each java _)
  implicit def viewJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): AtomicView[A, CC[A]]           = new LinearView(Each java xs)
  implicit def viewJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): AtomicView[K -> V, CC[K, V]] = new LinearView(Each javaMap xs)
}

/** This file needs to not import `object all` because that's cycle city,
 *  as we start relying on importing the implicits that we ourselves are
 *  supplying. We carved off some of that object for use here and import
 *  that specially.
 */
trait StdImplicits extends scala.AnyRef with StdBuilds with StdOps {
  self =>

  implicit def convertViewEach[A](xs: View[A]): Each[A]           = Each(xs foreach _)
  implicit def opsAny[A](x: A): AnyOps[A]                         = new AnyOps[A](x)
  implicit def promoteApiOrder[A](z: Order[A]): Order.Impl[A]     = Order impl z
  implicit def typeclassTupleCleave[A, B] : Cleaver[A -> B, A, B] = Cleaver[A -> B, A, B](((_, _)), fst, snd)

  implicit def promoteApiView[A](xs: View[A]): AtomicView[A, View[A]] = xs match {
    case xs: AtomicView[_, _] => cast(xs)
    case _                    => new LinearView(Each each xs)
  }
}

trait StdOps1 {
  implicit def opsPspUnbuilt[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Unbuilder[A, R]     = new Unbuilder[A, R](xs)
  implicit def convertViewBuilds[A, CC[A]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
}
trait StdOps2 extends StdOps1 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[A]                                       = new ViewOps(x)
  implicit def opsSize(x: Size): SizeOps                                                       = new SizeOps(x)
  implicit def opsTerminalView2[R, A](xs: R)(implicit z: UnbuildsAs[A, R]): TerminalViewOps[A] = new TerminalViewOps[A](xs.m)
  implicit def opsTerminalView[A](x: View[A]): TerminalViewOps[A]                              = new TerminalViewOps(x)
  implicit def opsUnbuildsView[R, A](xs: R)(implicit z: UnbuildsAs[A, R]): ViewOps[A]          = new ViewOps(xs.m)
  implicit def opsView2D[A](x: View2D[A]): View2DOps[A]                                        = new View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                                               = new Pstring(x)
}

trait StdOps3 extends StdOps2 {
  implicit def opsDirectView[R, A](xs: R)(implicit ev: R <:< Direct[A]): ViewOps[A] = new ViewOps(new DirectView(ev(xs)))

  // We're (sickly) using the context bound to reduce the applicability of the implicit,
  // but then discarding it. The only way these can be value classes is if the type class
  // arrives with the method call.

  implicit def opsChar(x: Char): CharOps                = new CharOps(x)
  implicit def opsFun[A, B](f: Fun[A, B]): FunOps[A, B] = new FunOps(f)
  implicit def opsLong(x: Long): LongOps                = new LongOps(x)
  implicit def opsPrecise(x: Precise): PreciseOps       = new PreciseOps(x)

  implicit def opsPairSplit[R, A, B](xs: Foreach[R])(implicit splitter: Splitter[R, A, B]): Paired[R, A, B] =
    new Paired[R, A, B](Each(xs foreach _))
}

trait StdOps extends StdOps3 {
  implicit def opsArrayNoTag[A](xs: Array[A]): ArrayOps[A]                                = new ArrayOps[A](xs)
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                      = new ShowInterpolator(sc)
  implicit def opsViewConversions[A](xs: View[A]): Conversions[A]                         = new Conversions(xs)
  implicit def unbuildableConv[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Conversions[A] = new Conversions[A](inView[A](z unbuild xs foreach _))
}

/*** The builder/unbuilder/view hierarchy.
 */

trait ScalaBuilds {
  implicit def unbuildScalaCollection[A, CC[X] <: GTOnce[X]] : UnbuildsAs[A, CC[A]]                            = Unbuilds[A, CC[A]](Each scala _)
  implicit def buildScalaCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                   = Builds.sCollection[A, That]
  implicit def viewScalaCollection[A, CC[X] <: sCollection[X]](xs: CC[A]): AtomicView[A, CC[A]]                = new LinearView(Each scala xs)
  implicit def unbuildScalaMap[K, V, CC[X, Y] <: scMap[X, Y]] : UnbuildsAs[K -> V, CC[K, V]]                   = Unbuilds[K -> V, CC[K, V]](Each scalaMap _)
  implicit def buildScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = Builds.sMap[K, V, That]
}
trait PspBuilds {
  implicit def unbuiltPspView0[A] : UnbuildsAs[A, View[A]]        = Unbuilds[A, View[A]](xs => xs)
  implicit def buildPspSet[A: Eq]: Builds[A, ExSet[A]]            = Builds.exSet[A]
  implicit def buildPspMap[K: Eq, V]: Builds[K -> V, ExMap[K, V]] = Builds.exMap[K, V]
}

trait StdBuilds0 extends PspBuilds with ScalaBuilds {
  implicit def buildPspArray[A: CTag]: Builds[A, Array[A]]                       = Builds.array[A]
  implicit def buildPspLinear[A] : Builds[A, Plist[A]]                           = Plist.newBuilder[A]
  implicit def unbuildPspEach[A, CC[X] <: Each[X]] : UnbuildsAs[A, CC[A]]        = Unbuilds[A, CC[A]](xs => xs)
  implicit def unbuiltPspArray[A] : UnbuildsAs[A, Array[A]]                      = Unbuilds[A, Array[A]](Direct array _)
  implicit def viewPspArray[A](xs: Array[A]): DirectView[A, Array[A]]            = new DirectView(Direct array xs)
  implicit def viewPspEach[A, CC[X] <: Each[X]](xs: CC[A]): AtomicView[A, CC[A]] = new LinearView(xs)
}
trait StdBuilds1 extends StdBuilds0 {
  implicit def buildPspDirect[A] : Builds[A, Vec[A]] = Vec.newBuilder[A]
}
trait StdBuilds extends StdBuilds1 {
  implicit def unbuildPspString: UnbuildsAs[Char, String]          = Unbuilds(Direct string _)
  implicit def buildPspString: Builds[Char, String]                = Builds.string
  implicit def viewPspString(xs: String): DirectView[Char, String] = new DirectView(Direct string xs)
}
