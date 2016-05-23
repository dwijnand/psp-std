package psp
package std

import exp._

trait AllImplicit extends AllImplicit0 with StdEmpty with StdBuilders with StdAlgebra {
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def cleaveJavaMapEntry[A, B]: Cleaver[jMapEntry[A, B], A, B] = cleaver(new SimpleImmutableEntry(_, _), _.getKey, _.getValue)
  implicit def cleaveScalaProduct[A, B]: Cleaver[A -> B, A, B]          = cleaver[A -> B, A, B](((_, _)), fst, snd)
  implicit def conforms[A] : A <:< A                                    = x => x

  // Conversions.
  implicit def longToPrecise(x: Long): Precise                     = Size(x)
  implicit def hasShowToDoc[A](x: A)(implicit z: Show[A]): Doc     = Doc(x)
  implicit def apiViewToIdView[A](xs: View[A]): IdView[A, View[A]] = new IdView(xs)
}

trait AllImplicit0 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[A, View[A]]           = new ViewOps(x)
  implicit def opsView[A, R](xs: R)(implicit z: Walks[A, R]): ViewOps[A, R] = new ViewOps(z walk xs)
  implicit def opsView2D[A](x: View2D[A]): View2DOps[A]                     = new View2DOps(x)
  implicit def opsWrapString(x: String): Pstring                            = new Pstring(x)
}
