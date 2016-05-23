package psp
package std

import all._

trait Walks[A, R] {
  def walk(xs: R): IdView[A, R]
}
trait Makes[-A, +R] {
  def make(xs: View[A]): R
}

object Walks extends StdWalks {
  def apply[A, R](f: R => Each[A]): Walks[A, R] = new Walks[A, R] {
    def walk(xs: R): IdView[A, R] = new IdView(f(xs))
  }
}
object Makes extends StdMakes {
  def apply[A, R](f: View[A] => R): Makes[A, R] = new Makes[A, R] {
    def make(xs: View[A]): R = f(xs)
  }
  class Helper[R] {
    def apply[A](expr: View[A])(implicit z: Makes[A, R]): R = z make expr
  }

  def fromInts[A, R](f: Int => A, start: Int, end: Int): Direct[A] = new FromInts(f, start, end)
  class FromInts[A](f: Int => A, start: Int, end: Int) extends StdDirect[A](Size(end - start)) {
    def apply(idx: Vdex): A = f(start + idx.indexValue.toInt)
  }
}

trait StdWalks {
  def javaIterable[A, M[X] <: jIterable[X]]: Walks[A, M[A]]        = suspend(xs => xs.iterator foreach _)
  def javaMap[K, V, ⇶[X,Y] <: jMap[X, Y]]: Walks[K->V, K ⇶ V]      = suspend(_.entrySet.pairs.foreach _)
  def jvmArray[A]: Walks[A, Array[A]]                              = Walks(xs => Makes.fromInts(xs.apply, 0, xs.length))
  def jvmString: Walks[Char, String]                               = Walks(s => Makes.fromInts(s charAt _, 0, s.length))
  def pspEach[A, M[X] <: Each[X]]: Walks[A, M[A]]                  = Walks(identity)
  def pspMap[A: Hash, B](xs: (A->B)*): Walks[A->B, Pmap[A, B]]     = suspend(_.pairs.foreach _)
  def pspSet[A, M[X] <: Pset[X]]: Walks[A, M[A]]                   = suspend(_.basis.foreach _)
  def pspView[A, CC[X] <: View[X]]: Walks[A, CC[A]]                = suspend(_.foreach _)
  def scalaMap[K, V, M[X, Y] <: scMap[X, Y]]: Walks[K->V, M[K, V]] = suspend(_.foreach _)
  def scalaOnce[A, M[X] <: GTOnce[X]]: Walks[A, M[A]]              = suspend(_.foreach _)
  def scalaOption[A]: Walks[A, Opt[A]]                             = suspend(_.foreach _)

  private def suspend[A, R](f: R => Suspended[A]): Walks[A, R]             = Walks(f andThen Each.apply)
}
trait StdMakes {
  def javaCollection[A, M[X] <: jCollection[X]](implicit z: Empty[M[A]]): Makes[A, M[A]]            = create(z.empty)(x => x)(_ add _)
  def javaGenericMap[K, V, M[X, Y] <: jMap[X, Y]](implicit z: Empty[M[K, V]]): Makes[K->V, M[K, V]] = create(z.empty)(x => x)((r, kv) => kv app r.put)
  def scalaGenericMap[K, V, That](implicit z: CanBuild[(K, V), That]): Makes[K->V, That]            = scalaTraversable contraMap (_ app pair)
  def scalaTraversable[A, That](implicit z: CanBuild[A, That]): Makes[A, That]                      = create(z())(_.result)(_ += _)

  def javaList[A]: Makes[A, jList[A]]           = javaCollection
  def javaMap[K, V]: Makes[K->V, jMap[K, V]]    = javaGenericMap
  def javaSet[A]: Makes[A, jSet[A]]             = javaCollection
  def jvmArray[A: CTag]: Makes[A, Array[A]]     = create(Array.newBuilder[A])(_.result)(_ += _)
  def jvmString: Makes[Char, String]            = create(new StringBuilder)(_.toString)(_ append _)
  def pspList[A]: Makes[A, Plist[A]]            = Makes(xs => ll.foldRight[A, Plist[A]](xs, Pnil(), _ :: _))
  def pspMap[K: Eq, V]: Makes[K->V, Pmap[K, V]] = pspDirect[K->V] map (kvs => kvs.m.toMap[sciMap](scalaMap) |> (f => Pmap(kvs map fst toPset, Fun(f))))
  def pspSet[A: Eq]: Makes[A, Pset[A]]          = Makes(Pset apply _)
  def pspDirect[A]: Makes[A, Direct[A]]         = scalaVector[A] map (xs => Makes.fromInts(xs.apply, 0, xs.length))
  def pspView[A]: Makes[A, View[A]]             = Makes(identity)
  def scalaList[A]: Makes[A, sciList[A]]        = scalaTraversable
  def scalaMap[K, V]: Makes[K->V, sciMap[K, V]] = scalaGenericMap
  def scalaSet[A]: Makes[A, sciSet[A]]          = scalaTraversable
  def scalaVector[A]: Makes[A, sciVector[A]]    = scalaTraversable

  private def create[B, A, R](buf: => B)(finish: B => R)(add: (B, A) => Unit): Makes[A, R] =
    Makes(xs => finish(doto(buf)(b => xs foreach (x => add(b, x)))))
}

trait StdConstructors {
  def bufferMap[A, B: Empty](): scmMap[A, B]          = scmMap[A, B]() withDefaultValue emptyValue[B]
  def elems[A, R](xs: A*)(implicit z: Makes[A, R]): R = z make xs
  def inView[A](mf: Suspended[A]): IdView[A, Each[A]] = Each(mf).m2
  def make[R]: Makes.Helper[R]                        = new Makes.Helper[R]
  def openIndices: OpenRange[Index]                   = 0.andUp map Index
  def vec[A](xs: A*): Vec[A]                          = elems(xs: _*)
  def view[A](xs: A*): RepView[Vec[A], A]             = vec(xs: _*).m
}
