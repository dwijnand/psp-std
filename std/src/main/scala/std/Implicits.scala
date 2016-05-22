package psp
package std

import api._, exp._

trait AllImplicit extends StdEmpty with MakesWalks with StdOps with StdAlgebra {
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def cleaverJMapEntry[A, B]: Cleaver[jMapEntry[A, B], A, B] = cleaver(new SimpleImmutableEntry(_, _), _.getKey, _.getValue)
  implicit def cleaverPair[A, B]: Cleaver[A -> B, A, B]               = cleaver[A -> B, A, B](((_, _)), fst, snd)
  implicit def conforms[A]: (A <:< A)                                 = new conformance[A]
  implicit def defaultRenderer: FullRenderer                          = new FullRenderer(minElements = Size(3), maxElements = Size(10))

  // Conversions.
  implicit def longToPrecise(x: Long): Precise                     = Size(x)
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B   = f.toPartial
  implicit def apiViewToIdView[A](xs: View[A]): IdView[A, View[A]] = new IdView(xs)
  implicit def hasShowToDoc[A](x: A)(implicit z: Show[A]): Doc     = Doc(x)
}

trait StdOps0 {
  implicit def unconvertViewToRepr[A, R](xs: View[A])(implicit z: Makes[A, R]): R = z make xs
}
trait StdOps extends StdOps0 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[A, View[A]]           = new ViewOps(x)
  implicit def opsView[A, R](xs: R)(implicit z: Walks[A, R]): ViewOps[A, R] = new ViewOps(z walk xs)
  implicit def opsView2D[A](x: View2D[A]): View2DOps[A]                     = new View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                            = new Pstring(x)
}
