package psp
package std

import exp._

trait AllImplicit extends AllImplicit0 with StdEmpty with StdBuilders with StdAlgebra {
  implicit def productizeJavaMapEntry[A, B]: Productize[jMapEntry[A, B], A, B] = Productize(jPair, _.getKey, _.getValue)
  implicit def productizeScalaProduct[A, B]: Productize[A -> B, A, B]          = Productize(pair, fst, snd)

  // Conversions.
  implicit def longToPrecise(x: Long): Precise                   = Size(x)
  implicit def hasShowToDoc[A](x: A)(implicit z: Show[A]): Doc   = Doc(x)
  implicit def stringToPstring(x: String): Pstring               = new Pstring(x)
  implicit def apiViewToRView[A](xs: View[A]): RView[A, View[A]] = View(xs)
}

trait AllImplicit0 {
  implicit def opsAlreadyView[A](x: View[A]): ViewOps[A, View[A]]           = new ViewOps(x)
  implicit def opsView[A, R](xs: R)(implicit z: Walks[A, R]): ViewOps[A, R] = new ViewOps(z walk xs)
}
