package psp
package std

/** The organization of the implicit builders is a black art.
 *  It might be better to generate StdBuilds[0-15] and put an implicit in
 *  each one so the prioritization is as unambiguous as scala allows.
 *
 *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
 *  around and it lets us build scala collections at the end of a view with no more code.
 *  Scala maps are built with Tuples even though Product2 should suffice; the types
 *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
 *  The rest of the time one should write it K -> V.
 */

import api._, all._

object Built {
  def apply[A, R](xs: Foreach[A])(implicit z: Builds[A, R]): R = z build xs
}

object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To] = new Builds(f)

  def ?[Elem, To](implicit z: Builds[Elem, To]) = z

  import Java._

  def array[A: CTag]: Builds[A, Array[A]]                                                    = apply(xs => doto(Array.newBuilder[A])(_ ++= xs.trav).result)
  // def direct[A](): Builds[A, Vec[A]]                                                         = new Vec.Builder[A]
  def exMap[K : Eq, V] : Builds[K -> V, ExMap[K, V]]                                         = ?[K->V, jMap[K, V]] map (ExMap fromJava _)
  def exSet[A : Eq]: Builds[A, ExSet[A]]                                                     = ?[A, jSet[A]] map (ExSet fromJava _)
  // def list[A](): Builds[A, Plist[A]]                                                         = new Plist.Builder[A]
  def sCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                   = apply(xs => doto(z())(b => xs foreach (b += _)) result)
  def sMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = apply(xs => doto(z())(b => xs foreach (x => b += (fst(x) -> snd(x)))) result)
  def string(): Builds[Char, String]                                                         = apply(xs => doto(new StringBuilder)(b => xs foreach (c => b append c)) toString)

  // final class Impl[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
  //   def build(xs: Foreach[Elem]): To   = f(xs)
  //   def apply(mf: Suspended[Elem]): To = build(Each(mf))
  // }
}

/** These classes all put the expected result type up front,
 *  where it can either be inferred from an existing value or
 *  supplied directly.
 */
class RemakeHelper[R](xs: R) {
  def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
}
class MakeHelper[R] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
}
class MakeHelper1[CC[_]] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build expr
}
// class MakeHelper2[CC[_, _]] {
//   def apply[K, V](expr: => ZipView[K, V])(implicit z: Builds[K->V, CC[K, V]]): CC[K, V] = z build expr.pairs
// }
