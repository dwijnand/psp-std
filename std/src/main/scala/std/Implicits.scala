package psp
package std

import api._, exp._ // no implicit conversions in this file
import java.util.AbstractMap.SimpleImmutableEntry

trait AllImplicit extends StdEmpty with StdImplicits { self =>

  implicit def conforms[A]: (A <:< A)                                 = new conformance[A]
  implicit def constantPredicate[A](value: Boolean): ToBool[A]        = cond(value, ConstantTrue, ConstantFalse)
  implicit def defaultRenderer: FullRenderer                          = new FullRenderer(minElements = 3, maxElements = 10)
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B      = f.toPartial
  implicit def opsDirect[A](xs: Direct[A]): DirectOps[A]              = new DirectOps(xs)
  implicit def opsForeach[A](xs: Foreach[A]): ForeachOps[A]           = new ForeachOps(xs)
  implicit def opsStringContext(sc: StringContext): ShowInterpolator  = new ShowInterpolator(sc)
  implicit def predicate1Algebra[A]: BooleanAlgebra[ToBool[A]]        = new Algebras.Predicate1Algebra[A]
  implicit def predicate2Algebra[A, B]: BooleanAlgebra[ToBool2[A, B]] = new Algebras.Predicate2Algebra[A, B]
  implicit def promoteSize(x: Long): Precise                          = Size(x)
  implicit def showableToDoc[A](x: A)(implicit z: Show[A]): Doc       = Doc(x)
}

/** This file needs to not import `object all` because that's cycle city,
  *  as we start relying on importing the implicits that we ourselves are
  *  supplying. We carved off some of that object for use here and import
  *  that specially.
  */
trait StdImplicits extends ViewersAs with ConvertersOf with StdOps { self =>
  implicit def opsAny[A](x: A): AnyOps[A] = new AnyOps[A](x)

  implicit def cleaverProduct2[A, B]: Cleaver[A -> B, A, B]                 = cleaver[A -> B, A, B](((_, _)), fst, snd)
  implicit def cleaverProduct3[A, B, C]: Cleaver[`3->`[A, B, C], A, B -> C] = cleaver((x, y) => ((x, fst(y), snd(y))), _._1, x => pair(x._2, x._3))
  implicit def cleaversciList[A]: Cleaver[sciList[A], A, sciList[A]]        = cleaver(_ :: _, _.head, _.tail)
  implicit def cleaverJMapEntry[A, B]: Cleaver[jMapEntry[A, B], A, B]       = cleaver(new SimpleImmutableEntry(_, _), _.getKey, _.getValue)

  implicit def promoteApiView[A](xs: View[A]): IdView[A, View[A]] = new IdView(xs)
}

trait StdOps0 {
  implicit def downConvertToCC[A, CC[X]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
}
trait StdOps1 extends StdOps0 {
  implicit def opsHasViewsAs[A, R](xs: R)(implicit z: ViewsAs[A, R]): HasViewsAs[A, R] = new HasViewsAs(xs)
  implicit def downConvertToR[A, R](xs: View[A])(implicit z: Builds[A, R]): R          = z build xs
}
trait StdOps2 extends StdOps1 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[A]                                                      = new ViewOps(x)
  implicit def opsSize(x: Size): SizeOps                                                                      = new SizeOps(x)
  implicit def opsTerminalView2[R, A](xs: R)(implicit z: ViewsAs[A, R]): TerminalViewOps[A]                   = new TerminalViewOps[A](xs.m)
  implicit def opsTerminalView[A](x: View[A]): TerminalViewOps[A]                                             = new TerminalViewOps(x)
  implicit def opsView[R, A](xs: R)(implicit z: ViewsAs[A, R]): ViewOps[A]                                    = new ViewOps(z viewAs xs)
  implicit def opsView2D[A](x: View2D[A]): View2DOps[A]                                                       = new View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                                                              = new Pstring(x)
  implicit def opsViewOfPairs[R, A, B](xs: View[R])(implicit sp: Splitter[R, A, B]): PairConversions[R, A, B] = new PairConversions(xs)
}
trait StdOps extends StdOps2 {
  implicit def opsIdView[R, A](xs: R)(implicit ev: R <:< Each[A]): ViewOps[A] = new ViewOps(new IdView(ev(xs)))

  implicit def opsChar(x: Char): CharOps                = new CharOps(x)
  implicit def opsFun[A, B](f: Fun[A, B]): FunOps[A, B] = new FunOps(f)
  implicit def opsLong(x: Long): LongOps                = new LongOps(x)
  implicit def opsPrecise(x: Precise): PreciseOps       = new PreciseOps(x)
}

trait ViewersAs0 {
  implicit def viewsAsJavaIterable[A, CC[X] <: jIterable[X]]: ViewsAs[A, CC[A]]          = viewsAs(Each java _)
  implicit def viewsAsScala[A, CC[X] <: sCollection[X]]: ViewsAs[A, CC[A]]               = viewsAs(Each scala _)
}
trait ViewersAs extends ViewersAs0 with Builders {
  implicit def viewsAsJavaMap[K, V, CC[X, Y] <: jMap[X, Y]]: ViewsAs[K -> V, CC[K, V]]   = viewsAs(Each javaMap _)
  implicit def viewsAsJvmArray[A]: ViewsAs[A, Array[A]]                                  = viewsAs(Each array _)
  implicit def viewsAsJvmString: ViewsAs[Char, String]                                   = viewsAs(Each jvmString _)
  implicit def viewsAsPspEach[A, CC[X] <: Foreach[X]]: ViewsAs[A, CC[A]]                 = viewsAs(identity)
  implicit def viewsAsScalaMap[K, V, CC[X, Y] <: scMap[X, Y]]: ViewsAs[K -> V, CC[K, V]] = viewsAs(Each scalaMap _)
}
