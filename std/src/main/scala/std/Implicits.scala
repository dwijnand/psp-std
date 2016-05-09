package psp
package std

import api._, exp._ // no implicit conversions in this file
import java.util.AbstractMap.SimpleImmutableEntry

trait AllImplicit extends StdEmpty with StdImplicits { self =>

  implicit def conforms[A]: (A <:< A)                                 = new conformance[A]
  implicit def constantPredicate[A](value: Boolean): ToBool[A]        = cond(value, ConstantTrue, ConstantFalse)
  implicit def defaultRenderer: FullRenderer                          = new FullRenderer(minElements = 3, maxElements = 10)
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B      = f.toPartial
  implicit def predicate1Algebra[A]: BooleanAlgebra[ToBool[A]]        = new Algebras.Predicate1Algebra[A]
  implicit def predicate2Algebra[A, B]: BooleanAlgebra[ToBool2[A, B]] = new Algebras.Predicate2Algebra[A, B]
  implicit def promoteSize(x: Long): Precise                          = Size(x)
  implicit def showableToDoc[A](x: A)(implicit z: Show[A]): Doc       = Doc(x)

  implicit def opsDirect[A](xs: Direct[A]): DirectOps[A]                                             = new DirectOps(xs)
  implicit def opsForeach[A](xs: Foreach[A]): ForeachOps[A]                                          = new ForeachOps(xs)
  implicit def unbuildJavaIterable[A, CC[X] <: jIterable[X]]: UnbuildsAs[A, CC[A]]                   = Unbuilds[A, CC[A]](Each java _)
  implicit def viewJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): AtomicView[A, CC[A]]           = new IdView(Each java xs)
  implicit def viewJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): AtomicView[K -> V, CC[K, V]] = new IdView(Each javaMap xs)
}

/** This file needs to not import `object all` because that's cycle city,
  *  as we start relying on importing the implicits that we ourselves are
  *  supplying. We carved off some of that object for use here and import
  *  that specially.
  */
trait StdImplicits extends scala.AnyRef with StdBuilds with StdOps { self =>
  implicit def opsAny[A](x: A): AnyOps[A] = new AnyOps[A](x)

  implicit def cleaverProduct2[A, B]: Cleaver[A -> B, A, B]                 = cleaver[A -> B, A, B](((_, _)), fst, snd)
  implicit def cleaverProduct3[A, B, C]: Cleaver[`3->`[A, B, C], A, B -> C] = cleaver((x, y) => ((x, fst(y), snd(y))), _._1, x => pair(x._2, x._3))
  implicit def cleaversciList[A]: Cleaver[sciList[A], A, sciList[A]]        = cleaver(_ :: _, _.head, _.tail)
  implicit def cleaverJMapEntry[A, B]: Cleaver[jMapEntry[A, B], A, B]       = cleaver(new SimpleImmutableEntry(_, _), _.getKey, _.getValue)

  implicit def promoteApiOrder[A](z: Order[A]): Order.Impl[A] = Order impl z
  implicit def promoteApiView[A](xs: View[A]): AtomicView[A, View[A]] = new IdView(xs)
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

  implicit def opsChar(x: Char): CharOps                = new CharOps(x)
  implicit def opsFun[A, B](f: Fun[A, B]): FunOps[A, B] = new FunOps(f)
  implicit def opsLong(x: Long): LongOps                = new LongOps(x)
  implicit def opsPrecise(x: Precise): PreciseOps       = new PreciseOps(x)
}

trait StdOps extends StdOps3 {
  implicit def opsArrayNoTag[A](xs: Array[A]): ArrayOps[A]                                   = new ArrayOps[A](xs)
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                         = new ShowInterpolator(sc)
  implicit def convertConvertible[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Conversions[A] = new Conversions(xs.m)
}

trait Unbuilders {
  implicit def unbuildScalaCollection[A, CC[X] <: sCollection[X]]: UnbuildsAs[A, CC[A]]         = Unbuilds[A, CC[A]](Each scala _)
  implicit def viewScalaCollection[A, CC[X] <: sCollection[X]](xs: CC[A]): AtomicView[A, CC[A]] = new IdView(Each scala xs)
  implicit def unbuildScalaMap[K, V, CC[X, Y] <: scMap[X, Y]]: UnbuildsAs[K -> V, CC[K, V]]     = Unbuilds[K -> V, CC[K, V]](Each scalaMap _)
  implicit def unbuildJavaMap[K, V, CC[X, Y] <: jMap[X, Y]]: UnbuildsAs[K -> V, CC[K, V]]       = Unbuilds[K -> V, CC[K, V]](Each javaMap _)
}

trait StdBuilds0 extends Unbuilders with Builders {
  implicit def unbuildPspEach[A, CC[X] <: Foreach[X]]: UnbuildsAs[A, CC[A]] = Unbuilds[A, CC[A]](identity)
  implicit def unbuildJvmArray[A]: UnbuildsAs[A, Array[A]]                  = Unbuilds[A, Array[A]](Each array _)

  implicit def viewJvmArray[A](xs: Array[A]): DirectView[A, Array[A]]            = new DirectView(Each array xs)
  implicit def viewPspEach[A, CC[X] <: Each[X]](xs: CC[A]): AtomicView[A, CC[A]] = new IdView(xs)
}
trait StdBuilds extends StdBuilds0 {
  implicit def unbuildJvmString: UnbuildsAs[Char, String]          = Unbuilds(Each jvmString _)
  implicit def buildJvmString: Builds[Char, String]                = Builds.jvmString
  implicit def viewJvmString(xs: String): DirectView[Char, String] = new DirectView(Each jvmString xs)
}
