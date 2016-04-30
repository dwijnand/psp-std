package psp
package std

import api._
import psp.std.{ lowlevel => ll }
import exp._ // no implicit conversions in this file

trait AllImplicit extends scala.AnyRef
      with StdEmpty
      with StdJava
      with PrimitiveInstances
      with AlgebraInstances
      with StdImplicits
{
  self =>

  import StdShow._

  // Ugh. XXX
  implicit def promoteSize(x: Long): Precise                     = Size(x)
  implicit def promoteIndex(x: Long): Index                      = Index(x)
  implicit def wrapClass(x: jClass): JavaClass                   = new JavaClassImpl(x)
  implicit def conforms[A] : (A <:< A)                           = new conformance[A]
  implicit def defaultRenderer: FullRenderer                     = new FullRenderer
  implicit def constantPredicate[A](value: Boolean): ToBool[A]   = if (value) ConstantTrue else ConstantFalse
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B = f.toPartial
  implicit def opsDirect[A](xs: Direct[A]): ops.DirectOps[A]     = new ops.DirectOps(xs)
  implicit def opsForeach[A](xs: Foreach[A]): ops.ForeachOps[A]  = new ops.ForeachOps(xs)

  implicit final class DocSeqOps(xs: Direct[Doc]) {
    def joinLines: String = xs mapNow (x => render(x)) mk_s EOL
  }

  implicit def foreachDocShows[A: Show](xs: Foreach[A]): DocSeqOps =
    new DocSeqOps(inView[A](xs foreach _) map (x => Doc(x)))
}

/** This file needs to not import `object all` because that's cycle city,
 *  as we start relying on importing the implicits that we ourselves are
 *  supplying. We carved off some of that object for use here and import
 *  that specially.
 */
trait StdImplicits extends scala.AnyRef
      with StdBuilds
      with StdOps
      with SetAndMapOps
      with StdUniversal {

  self =>

  implicit def typeclassTupleCleave[A, B] : Cleaver[A -> B, A, B]       = Cleaver[A -> B, A, B](((_, _)), fst, snd)
  // implicit def typeclassPlistSplit[A] : Splitter[Plist[A], A, Plist[A]] = Splitter(_.head, _.tail)
  // implicit def scalaListSplit[A] : Splitter[sciList[A], A, sciList[A]]  = Splitter(_.head, _.tail)

  implicit def convertViewEach[A](xs: View[A]): Each[A]    = Each(xs foreach _)
  // implicit def opsSplitView[A](xs: SplitView[A]): Split[A] = Split(xs.left, xs.right)
  implicit def opsBuildsTypeClass[Elem, To](z: Builds[Elem, To]): ops.BuildsTypeClassOps[Elem, To] = new ops.BuildsTypeClassOps(z)

  // Promotion of the api type (which has as few methods as possible) to the
  // concrete type which has all the other ones.
  implicit def promoteApiVindex(x: Vindex): Index.Impl                            = Index impl Index(x.indexValue)
  implicit def promoteApiOrder[A](z: Order[A]): Order.Impl[A]                     = Order impl z
  implicit def promoteApiExSet[A](x: ExSet[A]): ExSet.Impl[A]                     = ExSet impl x
  implicit def promoteApiExMap[K, V](x: ExMap[K, V]): ExMap.Impl[K, V]            = ExMap impl x
  implicit def promoteApiView[A](xs: View[A]): AtomicView[A, View[A]]             = viewImpl[A](xs)
  implicit def promoteApiZipView[A, B](xs: ZipView[A, B]): Zip.Impl[A, B]         = Zip impl xs
  // implicit def promoteApiUnbuilds[A, R](x: UnbuildsAs[A, R]): Unbuilds.Impl[A, R] = Unbuilds impl x

  private def viewImpl[A](xs: api.View[A]): AtomicView[A, View[A]] = xs match {
    case xs: AtomicView[_, _] => xs.castTo
    case _                    => new LinearView(Each each xs)
  }
}

trait SetAndMapOps {
  implicit def opsExtensionalSet[A](x: ExSet[A]): ops.ExSetOps[A]          = new ops.ExSetOps(x)
  implicit def opsExtensionalMap[K, V](x: ExMap[K, V]): ops.ExMapOps[K, V] = new ops.ExMapOps(x)
}

trait StdOps0 {
  implicit def opsPspUnbuilt[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Unbuilder[A, R] = new Unbuilder[A, R](xs)
}
trait StdOps1 extends StdOps0 {
  implicit def convertViewBuilds[A, CC[A]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
  implicit def opsHasEq[A: Eq](x: View[A]): ops.HasEq[A]       = new ops.HasEq(x)
}
trait StdOps2 extends StdOps1 {
  // We buried Predef's {un,}augmentString in favor of these.
  implicit def opsWrapString(x: String): Pstring                                       = new Pstring(x)
  implicit def opsAlreadyView[A](x: View[A]): ops.IViewOps[A]                          = new ops.IViewOps(x)
  implicit def opsReprView[R, A](xs: R)(implicit ev: R <:< Direct[A]): ops.IViewOps[A] = new ops.IViewOps(new DirectView(ev(xs)))
  implicit def opsHasOrderInfix[A: Order](x: A): ops.OrderOps[A]                       = new ops.OrderOps[A](x)
  // implicit def opsHasHash[A: Hash](x: View[A]): ops.HasHash[A]                      = new ops.HasHash(x)
  implicit def opsView2D[A](x: View2D[A]): ops.View2DOps[A]                            = new ops.View2DOps(x)
  implicit def opsSize(x: Size): ops.SizeOps                                           = new ops.SizeOps(x)
}

trait StdOps3 extends StdOps2 {
  // We're (sickly) using the context bound to reduce the applicability of the implicit,
  // but then discarding it. The only way these can be value classes is if the type class
  // arrives with the method call.

  implicit def opsChar(x: Char): ops.CharOps                                  = new ops.CharOps(x)
  implicit def opsCmpEnum(x: Cmp): ops.CmpEnumOps                             = new ops.CmpEnumOps(x)
  implicit def opsFun[A, B](f: Fun[A, B]): ops.FunOps[A, B]                   = new ops.FunOps(f)
  implicit def opsHasAlgebraInfix[A: BooleanAlgebra](x: A): ops.AlgebraOps[A] = new ops.AlgebraOps[A](x)
  implicit def opsHasEmpty[A: Empty](x: View[A]): ops.HasEmpty[A]             = new ops.HasEmpty[A](x)
  implicit def opsHasEqInfix[A: Eq](x: A): ops.EqOps[A]                       = new ops.EqOps[A](x)
  implicit def opsHasOrder[A: Order](x: View[A]): ops.HasOrder[A]             = new ops.HasOrder(x)
  // implicit def opsInputStream(x: InputStream): ops.InputStreamOps             = new ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                     = new ops.IntOps(x)
  implicit def opsLong(x: Long): ops.LongOps                                  = new ops.LongOps(x)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                   = new ops.OptionOps[A](x)
  // implicit def opsPath(x: jPath): ops.PathOps                                 = new ops.PathOps(x)
  implicit def opsPrecise(x: Precise): ops.PreciseOps                         = new ops.PreciseOps(x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                            = new ops.TryOps[A](x)

  implicit def opsPairSplit[R, A, B](xs: Foreach[R])(implicit splitter: Splitter[R, A, B]): Paired[R, A, B] =
    new Paired[R, A, B](Each(xs foreach _))
}

trait StdOps extends StdOps3 {
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                      = new ShowInterpolator(sc)
  // implicit def convertPredicateStreamFilter[A](p: ToBool[A]): DirectoryStreamFilter[A]    = new DirectoryStreamFilter[A] { def accept(entry: A) = p(entry) }
  implicit def opsViewConversions[A](xs: View[A]): Conversions[A]                         = new Conversions(xs)
  implicit def unbuildableConv[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Conversions[A] = new Conversions[A](inView[A](z unbuild xs foreach _))

  implicit def opsArrayNoTag[A](xs: Array[A]): ops.ArraySpecificOps[A]         = new ops.ArraySpecificOps[A](xs)
  implicit def opsArrayWithTag[A: CTag](xs: Array[A]): ops.ArrayClassTagOps[A] = new ops.ArrayClassTagOps[A](xs)
}

trait StdUniversal0 {
  implicit def opsAny[A](x: A): ops.AnyOps[A] = new ops.AnyOps[A](x)
  // Lower priority than the hand-specialized variations.
  final implicit def arrowAssocRef[A](x: A): ll.ArrowAssocRef[A] = new ll.ArrowAssocRef(x)
}
trait StdUniversal extends StdUniversal0 {
  // final implicit def ac(x: Int): ll.ArrowAssocInt             = new ll.ArrowAssocInt(x)
  // final implicit def arrowAssocLong(x: Long): ll.ArrowAssocLong          = new ll.ArrowAssocLong(x)
  // final implicit def arrowAssocDouble(x: Double): ll.ArrowAssocDouble    = new ll.ArrowAssocDouble(x)
  // final implicit def arrowAssocChar(x: Char): ll.ArrowAssocChar          = new ll.ArrowAssocChar(x)
  // final implicit def arrowAssocBoolean(x: Boolean): ll.ArrowAssocBoolean = new ll.ArrowAssocBoolean(x)
}

/*** The builder/unbuilder/view hierarchy.
 */

trait ScalaBuilds {
  implicit def unbuildScalaCollection[A, CC[X] <: GTOnce[X]] : UnbuildsAs[A, CC[A]]             = Unbuilds[A, CC[A]](Each scala _)
  implicit def buildScalaCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]    = Builds.sCollection[A, That]
  implicit def viewScalaCollection[A, CC[X] <: sCollection[X]](xs: CC[A]): AtomicView[A, CC[A]] = new LinearView(Each scala xs)

  implicit def unbuildScalaMap[K, V, CC[X, Y] <: scMap[X, Y]] : UnbuildsAs[K -> V, CC[K, V]]                   = Unbuilds[K -> V, CC[K, V]](Each scalaMap _)
  implicit def buildScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = Builds.sMap[K, V, That]
  implicit def viewScalaIndexedSeq[A, CC[X] <: sciIndexedSeq[X]](xs: CC[A]): DirectView[A, CC[A]]              = new DirectView(Direct scala xs)
}
trait PspBuilds0 {
  implicit def unbuiltPspView0[A] : UnbuildsAs[A, View[A]]           = Unbuilds[A, View[A]](xs => xs)
}
trait PspBuilds extends PspBuilds0 {
  // implicit def unbuiltPspView1[A, R] : UnbuildsAs[A, BaseView[A, R]] = Unbuilds[A, BaseView[A, R]](xs => xs)
  implicit def buildPspSet[A: Eq]: Builds[A, ExSet[A]]               = Builds.exSet[A]
  implicit def buildPspMap[K: Eq, V]: Builds[K -> V, ExMap[K, V]]    = Builds.exMap[K, V]
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
  implicit def buildPspDirect[A] : Builds[A, Vec[A]]                                 = Vec.newBuilder[A]
  implicit def viewPspDirect[A, CC[X] <: Direct[X]](xs: CC[A]): DirectView[A, CC[A]] = new DirectView(xs)
}
trait StdBuilds extends StdBuilds2 {
  implicit def unbuildPspString: UnbuildsAs[Char, String]              = Unbuilds(Direct string _)
  implicit def buildPspString: Builds[Char, String]                    = Builds.string
  implicit def viewPspString(xs: String): DirectView[Char, String]     = new DirectView(Direct string xs)
}

trait AlgebraInstances {
  implicit def predicateAlgebra[A] : BooleanAlgebra[ToBool[A]] = new Algebras.PredicateAlgebra[A]
}
