package psp
package std

import api._, exp._

trait AllImplicit extends StdEmpty with StdViewers with StdConverters with StdOps {
  import java.util.AbstractMap.SimpleImmutableEntry

  // Values.
  implicit def cleaverJMapEntry[A, B]: Cleaver[jMapEntry[A, B], A, B]       = cleaver(new SimpleImmutableEntry(_, _), _.getKey, _.getValue)
  implicit def cleaverProduct2[A, B]: Cleaver[A -> B, A, B]                 = cleaver[A -> B, A, B](((_, _)), fst, snd)
  implicit def cleaverProduct3[A, B, C]: Cleaver[`3->`[A, B, C], A, B -> C] = cleaver((x, y) => ((x, fst(y), snd(y))), _._1, x => pair(x._2, x._3))
  implicit def cleaversciList[A]: Cleaver[sciList[A], A, sciList[A]]        = cleaver(_ :: _, _.head, _.tail)
  implicit def conforms[A]: (A <:< A)                                       = new conformance[A]
  implicit def defaultRenderer: FullRenderer                                = new FullRenderer(minElements = Size(3), maxElements = Size(10))
  implicit def predicate1Algebra[A]: BooleanAlgebra[ToBool[A]]              = new Algebras.Predicate1Algebra[A]
  implicit def predicate2Algebra[A, B]: BooleanAlgebra[ToBool2[A, B]]       = new Algebras.Predicate2Algebra[A, B]
  implicit def promoteApiView[A](xs: View[A]): IdView[A, View[A]]           = new IdView(xs)
  implicit def showableToDoc[A](x: A)(implicit z: Show[A]): Doc             = Doc(x)

  // Conversions.
  implicit def longToPrecise(x: Long): Precise                    = Size(x)
  implicit def boolToConstPredicate[A](value: Boolean): ToBool[A] = cond(value, ConstantTrue, ConstantFalse)
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B  = f.toPartial
}

trait StdOps0 {
  implicit def unconvertViewToRepr[A, R](xs: View[A])(implicit z: Builds[A, R]): R = z build xs
}
trait StdOps1 extends StdOps0 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[View[A], A]             = new ViewOps(x)
  implicit def opsView[R, A](xs: R)(implicit z: ViewsAs[A, R]): ViewOps[R, A] = new ViewOps(z viewAs xs)
  implicit def opsView2D[A](x: View2D[A]): View2DOps[A]                       = new View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                              = new Pstring(x)
}
trait StdOps extends StdOps1 {
  implicit def opsIdView[R, A](xs: R)(implicit ev: R <:< Each[A]): ViewOps[R, A] = new ViewOps(new IdView(ev(xs)))
}
