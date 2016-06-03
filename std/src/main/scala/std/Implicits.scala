package psp
package std

import exp._

trait AllImplicit extends AllImplicit0 with StdEmpty with StdBuilders with StdAlgebra {
  implicit def productizeJavaMapEntry[A, B]: Productize[jMapEntry[A, B], A, B] = Productize(jPair, _.getKey, _.getValue)
  implicit def productizeScalaProduct[A, B]: Productize[A -> B, A, B]          = Productize(pair, fst, snd)

  // Conversions.
  implicit def longToPrecise(x: Long): Precise     = Size(x)
  implicit def stringToPstring(x: String): Pstring = new Pstring(x)
}

trait AllImplicit0 {
  implicit def walksToView[A, R](xs: R)(implicit z: Walks[A, R]): RView[A, R] = View(z walk xs)
  implicit def showsToDoc[A](x: A)(implicit z: Show[A]): Doc                  = Doc(x)
}
