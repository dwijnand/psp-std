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
      with PrimitiveInstances
      with AlgebraInstances
      with StdImplicits
{
  self =>

  implicit def conforms[A] : (A <:< A)                           = new conformance[A]
  implicit def constantPredicate[A](value: Boolean): ToBool[A]   = cond(value, ConstantTrue, ConstantFalse)
  implicit def defaultRenderer: FullRenderer                     = new FullRenderer
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B = f.toPartial
  implicit def opsDirect[A](xs: Direct[A]): ops.DirectOps[A]     = new ops.DirectOps(xs)
  implicit def opsForeach[A](xs: Foreach[A]): ops.ForeachOps[A]  = new ops.ForeachOps(xs)
  implicit def promoteSize(x: Long): Precise                     = Size(x)

  implicit def opsJavaIterator[A](x: jIterator[A]): ops.JavaIteratorOps[A]                           = new ops.JavaIteratorOps(x)
  implicit def unbuildJavaIterable[A, CC[X] <: jIterable[X]] : UnbuildsAs[A, CC[A]]                  = Unbuilds[A, CC[A]](Each java _)
  implicit def viewJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): AtomicView[A, CC[A]]           = new LinearView(Each java xs)
  implicit def viewJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): AtomicView[K -> V, CC[K, V]] = new LinearView(Each javaMap xs)
}

/** This file needs to not import `object all` because that's cycle city,
 *  as we start relying on importing the implicits that we ourselves are
 *  supplying. We carved off some of that object for use here and import
 *  that specially.
 */
trait StdImplicits extends scala.AnyRef
      with StdBuilds
      with StdOps
      with StdUniversal {

  self =>

  implicit def typeclassTupleCleave[A, B] : Cleaver[A -> B, A, B]       = Cleaver[A -> B, A, B](((_, _)), fst, snd)
  // implicit def typeclassPlistSplit[A] : Splitter[Plist[A], A, Plist[A]] = Splitter(_.head, _.tail)
  // implicit def scalaListSplit[A] : Splitter[sciList[A], A, sciList[A]]  = Splitter(_.head, _.tail)

  implicit def convertViewEach[A](xs: View[A]): Each[A]    = Each(xs foreach _)
  // implicit def opsSplitView[A](xs: SplitView[A]): Split[A] = Split(xs.left, xs.right)
  implicit def opsBuildsTypeClass[Elem, To](z: Builds[Elem, To]): ops.BuildsTypeClassOps[Elem, To] = new ops.BuildsTypeClassOps(z)

  implicit def promoteApiOrder[A](z: Order[A]): Order.Impl[A]             = Order impl z
  implicit def promoteApiExSet[A](x: ExSet[A]): ExSet.Impl[A]             = ExSet impl x
  implicit def promoteApiExMap[K, V](x: ExMap[K, V]): ExMap.Impl[K, V]    = ExMap impl x
  implicit def promoteApiZipView[A, B](xs: ZipView[A, B]): Zip.Impl[A, B] = Zip impl xs

  implicit def promoteApiView[A](xs: View[A]): AtomicView[A, View[A]] = xs match {
    case xs: AtomicView[_, _] => cast(xs)
    case _                    => new LinearView(Each each xs)
  }
}

trait StdOps0 {
  implicit def opsPspUnbuilt[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Unbuilder[A, R] = new Unbuilder[A, R](xs)
}
trait StdOps1 extends StdOps0 {
  implicit def convertViewBuilds[A, CC[A]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
  // implicit def opsHasEq[A: Eq](x: View[A]): ops.HasEq[A]       = new ops.HasEq(x)
}
trait StdOps2 extends StdOps1 {
  implicit def opsAlreadyView[A](x: View[A]): ops.ViewOps[A]                                       = new ops.ViewOps(x)
  implicit def opsHasOrderInfix[A: Order](x: A): ops.OrderOps[A]                                   = new ops.OrderOps[A](x)
  implicit def opsSize(x: Size): ops.SizeOps                                                       = new ops.SizeOps(x)
  implicit def opsOp[A, B](x: Op[A, B]): OpOps[A, B]                                               = new OpOps(x)
  implicit def opsTerminalView2[R, A](xs: R)(implicit z: UnbuildsAs[A, R]): ops.TerminalViewOps[A] = new ops.TerminalViewOps[A](xs.m)
  implicit def opsTerminalView[A](x: View[A]): ops.TerminalViewOps[A]                              = new ops.TerminalViewOps(x)
  implicit def opsUnbuildsView[R, A](xs: R)(implicit z: UnbuildsAs[A, R]): ops.ViewOps[A]          = new ops.ViewOps(xs.m)
  implicit def opsView2D[A](x: View2D[A]): ops.View2DOps[A]                                        = new ops.View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                                                   = new Pstring(x)
}

trait StdOps3 extends StdOps2 {
  implicit def opsDirectView[R, A](xs: R)(implicit ev: R <:< Direct[A]): ops.ViewOps[A] = new ops.ViewOps(new DirectView(ev(xs)))

  // We're (sickly) using the context bound to reduce the applicability of the implicit,
  // but then discarding it. The only way these can be value classes is if the type class
  // arrives with the method call.

  implicit def opsChar(x: Char): ops.CharOps                                  = new ops.CharOps(x)
  implicit def opsCmpEnum(x: Cmp): ops.CmpEnumOps                             = new ops.CmpEnumOps(x)
  implicit def opsFun[A, B](f: Fun[A, B]): ops.FunOps[A, B]                   = new ops.FunOps(f)
  implicit def opsHasAlgebraInfix[A: BooleanAlgebra](x: A): ops.AlgebraOps[A] = new ops.AlgebraOps(x)
  implicit def opsHasEqInfix[A: Eq](x: A): ops.EqOps[A]                       = new ops.EqOps(x)
  implicit def opsHasEq[A: Eq](x: View[A]): ops.HasEq[A]                      = new ops.HasEq(x)
  implicit def opsInt(x: Int): ops.IntOps                                     = new ops.IntOps(x)
  implicit def opsLong(x: Long): ops.LongOps                                  = new ops.LongOps(x)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                   = new ops.OptionOps(x)
  implicit def opsPrecise(x: Precise): ops.PreciseOps                         = new ops.PreciseOps(x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                            = new ops.TryOps(x)

  implicit def opsPairSplit[R, A, B](xs: Foreach[R])(implicit splitter: Splitter[R, A, B]): Paired[R, A, B] =
    new Paired[R, A, B](Each(xs foreach _))
}

trait StdOps extends StdOps3 {
  implicit def opsArrayNoTag[A](xs: Array[A]): ArrayOps[A]                                = new ArrayOps[A](xs)
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                      = new ShowInterpolator(sc)
  implicit def opsViewConversions[A](xs: View[A]): Conversions[A]                         = new Conversions(xs)
  implicit def unbuildableConv[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Conversions[A] = new Conversions[A](inView[A](z unbuild xs foreach _))
}

trait StdUniversal {
  implicit def opsAny[A](x: A): ops.AnyOps[A]              = new ops.AnyOps[A](x)
  implicit def arrowAssocRef[A](x: A): ll.ArrowAssocRef[A] = new ll.ArrowAssocRef(x)
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
  implicit def buildPspLinear[A] : Builds[A, Plist[A]]                           = Plist.newBuilder[A]
  implicit def viewPspEach[A, CC[X] <: Each[X]](xs: CC[A]): AtomicView[A, CC[A]] = new LinearView(xs)
  implicit def unbuildPspEach[A, CC[X] <: Each[X]] : UnbuildsAs[A, CC[A]]        = Unbuilds[A, CC[A]](xs => xs)
}
trait StdBuilds1 extends StdBuilds0 {
  implicit def unbuiltPspArray[A] : UnbuildsAs[A, Array[A]]           = Unbuilds[A, Array[A]](Direct array _)
  implicit def buildPspArray[A: CTag]: Builds[A, Array[A]]            = Builds.array[A]
  implicit def viewPspArray[A](xs: Array[A]): DirectView[A, Array[A]] = new DirectView(Direct array xs)
}
trait StdBuilds2 extends StdBuilds1 {
  implicit def buildPspDirect[A] : Builds[A, Vec[A]] = Vec.newBuilder[A]
}
trait StdBuilds extends StdBuilds2 {
  implicit def unbuildPspString: UnbuildsAs[Char, String]              = Unbuilds(Direct string _)
  implicit def buildPspString: Builds[Char, String]                    = Builds.string
  implicit def viewPspString(xs: String): DirectView[Char, String]     = new DirectView(Direct string xs)
}

trait AlgebraInstances {
  implicit def predicateAlgebra[A] : BooleanAlgebra[ToBool[A]] = new Algebras.PredicateAlgebra[A]
}
