package psp
package std

import api._, all._

final class Builds[-A, +R](val f: View[A] => R) {
  def contraMap[B](g: B => A): Builds[B, R] = Builds(f compose (_ map g))
  def map[S](g: R => S): Builds[A, S]       = Builds(g compose f)
  def build(xs: View[A]): R                 = f(xs)
  def build(xs: Each[A]): R                 = build(new IdView(xs))
}
final class ViewsAs[A, R](val f: R => View[A]) extends AnyVal {
  def viewAs(xs: R): IdView[A, R] = new IdView(f(xs))
}

object Builds {
  def apply[A, R](f: View[A] => R): Builds[A, R] =
    new Builds(f)

  private def create[B, A, R](buf: => B)(finish: B => R)(add: (B, A) => Unit): Builds[A, R] =
    apply(xs => finish(doto(buf)(b => xs foreach (x => add(b, x)))))

  def forJava[A, M[X] <: jCollection[X]](empty: M[A]): Builds[A, M[A]]                         = create(empty)(x => x)(_ add _)
  def forJavaMap[K, V, M[X, Y] <: jAbstractMap[X, Y]](empty: M[K, V]): Builds[K -> V, M[K, V]] = create(empty)(x => x)((r, kv) => kv app r.put)
  def forScala[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                        = create(z())(_.result)(_ += _)
  def forScalaMap[K, V, That](implicit z: CanBuild[(K, V), That]): Builds[K -> V, That]        = forScala contraMap (_ app pair)

  def javaList[A]: Builds[A, jList[A]]             = forJava(new jArrayList[A])
  def javaMap[K, V]: Builds[K -> V, jMap[K, V]]    = forJavaMap(new jHashMap[K, V])
  def javaSet[A]: Builds[A, jSet[A]]               = forJava(new jHashSet[A])
  def jvmArray[A: CTag]: Builds[A, Array[A]]       = create(Array.newBuilder[A])(_.result)(_ += _)
  def jvmString: Builds[Char, String]              = create(new StringBuilder)(_.toString)(_ append _)
  def pspList[A]: Builds[A, Plist[A]]              = apply(xs => ll.foldRight[A, Plist[A]](xs, pnil(), _ :: _))
  def pspMap[K: Eq, V]: Builds[K -> V, Pmap[K, V]] = ???
  def pspSet[A: Eq]: Builds[A, Pset[A]]            = apply(Pset apply _)
  def pspVec[A]: Builds[A, Vec[A]]                 = scalaVector[A] map (xs => new Vec(xs))
  def scalaList[A]: Builds[A, sciList[A]]          = forScala
  def scalaMap[K, V]: Builds[K -> V, sciMap[K, V]] = forScalaMap
  def scalaSet[A]: Builds[A, sciSet[A]]            = forScala
  def scalaVector[A]: Builds[A, sciVector[A]]      = forScala

  class RemakeHelper[R](xs: R) {
    def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
  }
  class MakeHelper[R] {
    def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
  }
}
