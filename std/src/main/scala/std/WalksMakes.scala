package psp
package std

import all._

trait Walks[A, R] {
  def walk(xs: R): RView[A, R]
  def sizeOf(xs: R): Size
}
trait Makes[-A, +R] {
  def make(xs: View[A]): R
}

object Walks extends StdWalks {
  def apply[A, R](f: R => Each[A]): Walks[A, R] = new Walks[A, R] {
    def walk(xs: R): RView[A, R] = View(f(xs))
    def sizeOf(xs: R): Size      = walk(xs).size
  }
}
object Makes extends StdMakes {
  class Const[A](elem: A) extends Generated[A] {
    def apply(idx: Index): A         = elem
    def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  class Continually[A](elem: => A) extends Generated[A] {
    def apply(idx: Index): A         = elem
    def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  class Iterable[A](it: () => scIterator[A]) extends Each[A] {
    def size                        = Size.Unknown
    def foreach(f: A => Unit): Unit = it() foreach f
  }
  class FromInts[A](f: Int => A, start: Int, end: Int) extends Direct[A] {
    def size = Size(end - start)
    def apply(idx: Index): A = f(start + idx.indexValue.toInt)
  }
  class Helper[R] {
    def apply[A](expr: View[A])(implicit z: Makes[A, R]): R = z make expr
  }

  def apply[A, R](f: View[A] => R): Makes[A, R] = new Makes[A, R] {
    def make(xs: View[A]): R = f(xs)
  }

  def fromPairs[R, A, B](xs: R)(implicit pz: IsPairs[R, A, B]): Vec[A->B]  = fromArgs(pz pairs xs seq: _*)
  def fromIntIndexed[R, A](xs: R)(implicit iz: IsIntIndexed[R, A]): Vec[A] = fromInts(iz.elem(xs, _), 0, iz length xs)
  def fromFolded[R, A](xs: R)(implicit ez: IsFolded[R, A]): Each[A]        = ez resume xs suspend
  def fromInts[A](f: Int => A, start: Int, end: Int): Vec[A]               = new FromInts(f, start, end)
  def fromArgs[A](xs: A*): Vec[A]                                          = fromInts(xs.apply, 0, xs.length)

  def reversed[A](xs: Vec[A]): Vec[A]              = fromInts(xs applyReverse _.index, 0, xs.size.getInt)
  def const[A](elem: A): Generated[A]              = new Const(elem)
  def continually[A](expr: => A): Generated[A]     = new Continually(expr)
  def iterable[A](expr: => scIterator[A]): Each[A] = new Iterable(() => expr)
}

trait StdWalks {
  def javaIterable[A, M[X] <: jIterable[X]]: Walks[A, M[A]]        = suspended(xs => xs.iterator foreach _)
  def javaMap[K, V, ⇶[X,Y] <: jMap[X, Y]]: Walks[K->V, K ⇶ V]      = suspended(xs => f => xs.entrySet foreach (kv => f(kv.toPair)))
  def pspEach[A, M[X] <: Each[X]]: Walks[A, M[A]]                  = Walks(identity)
  def pspMap[A: Hash, B](xs: (A->B)*): Walks[A->B, Pmap[A, B]]     = suspended(_.pairs.foreach _)
  def pspSet[A, M[X] <: Pset[X]]: Walks[A, M[A]]                   = suspended(_.basis.foreach _)
  def pspView[A, CC[X] <: View[X]]: Walks[A, CC[A]]                = suspended(_.foreach _)
  def scalaMap[K, V, M[X, Y] <: scMap[X, Y]]: Walks[K->V, M[K, V]] = suspended(_.foreach _)
  def scalaOnce[A, M[X] <: GTOnce[X]]: Walks[A, M[A]]              = Walks(fromScala)
  def scalaOption[A]: Walks[A, Opt[A]]                             = suspended(_.foreach _)

  private def fromScala[A](xs: GTOnce[A]): Each[A] = xs match {
    case xs: scIndexedSeq[A] => Makes.fromInts(xs.apply, 0, xs.length)
    case xs: sCollection[A]  => Folded[A](xs foreach _).suspend
    case _                   => fromScala(xs.toTraversable)
  }
  private def suspended[A, R](f: R => Suspended[A]): Walks[A, R] = Walks(xs => suspend(f(xs)))
}
trait StdMakes {
  def javaCollection[A, M[X] <: jCollection[X]](implicit z: Empty[M[A]]): Makes[A, M[A]]            = create[A](z.empty)(_ add _)(identity)
  def javaGenericMap[K, V, M[X, Y] <: jMap[X, Y]](implicit z: Empty[M[K, V]]): Makes[K->V, M[K, V]] = create[K->V](z.empty)((r, kv) => kv app r.put)(identity)
  def scalaGenericMap[K, V, That](implicit z: CanBuild[(K, V), That]): Makes[K->V, That]            = scalaTraversable contraMap (_ app pair)
  def scalaTraversable[A, That](implicit z: CanBuild[A, That]): Makes[A, That]                      = create[A](z())(_ += _)(_.result)

  def javaList[A]: Makes[A, jList[A]]                   = javaCollection
  def javaMap[K, V]: Makes[K->V, jMap[K, V]]            = javaGenericMap
  def javaSet[A]: Makes[A, jSet[A]]                     = javaCollection
  def jvmArray[A: CTag]: Makes[A, Array[A]]             = create[A](Array.newBuilder[A])(_ += _)(_.result)
  def jvmString: Makes[Char, String]                    = create[Char](new StringBuilder)(_ append _)(_.toString)
  def pspList[A]: Makes[A, Plist[A]]                    = Makes(xs => ll.foldRight[A, Plist[A]](xs, Pnil(), _ :: _))
  def pspMap[K : Hash : Eq, V]: Makes[K->V, Pmap[K, V]] = pspDirect[K->V] map (kvs => kvs.m.toMap[sciMap](scalaMap) |> (f => Pmap(kvs map fst toPset, Fun(f))))
  def pspSet[A : Hash : Eq]: Makes[A, Pset[A]]          = Makes(Pset apply _)
  def pspStream[A]: Makes[A, Pstream[A]]                = Makes(_ match { case HeadTailView(hd, tl) => new Pscons(hd, tl.toStream) ; case _ => Psnil() })
  def pspDirect[A]: Makes[A, Vec[A]]                    = scalaVector[A] map (xs => Makes.fromInts(xs.apply, 0, xs.length))
  def pspView[A]: Makes[A, View[A]]                     = Makes(identity)
  def scalaList[A]: Makes[A, sciList[A]]                = scalaTraversable
  def scalaMap[K, V]: Makes[K->V, sciMap[K, V]]         = scalaGenericMap
  def scalaSet[A]: Makes[A, sciSet[A]]                  = scalaTraversable
  def scalaVector[A]: Makes[A, sciVector[A]]            = scalaTraversable

  private def create[A] = new Helper[A]
  private class Helper[A] {
    def apply[B, R](buf: => B)(add: (B, A) => Unit)(finish: B => R): Makes[A, R] =
      Makes(xs => finish(doto(buf)(b => xs foreach (x => add(b, x)))))
  }
}

trait StdConstructors {
  def bufferMap[A, B: Empty](): scmMap[A, B]          = scmMap[A, B]() withDefaultValue emptyValue[B]
  def elems[A, R](xs: A*)(implicit z: Makes[A, R]): R = z make xs
  def suspend[A](mfs: Suspended[A]*): Each[A]         = Folded[A](f => mfs foreach (_ apply f)).suspend
  def make[R]: Makes.Helper[R]                        = new Makes.Helper[R]
  def openIndices: OpenRange[Index]                   = 0.andUp map Index
  def closedIndices: ClosedRange[Index]               = 0 indexUntil MaxLong
  def vec[A](xs: A*): Vec[A]                          = elems(xs: _*)
  def view[A](xs: A*): View[A]                        = xs.m

  def hashFun[A](xs: View[A])(implicit ez: Eq[A], hz: Hash[A]): HashFun[A] = {
    val buf = bufferMap[Long, View[A]]()
    zipMap(xs)(hz.hash) foreach ((x, h) => buf(h) :+= x)
    Fun(buf.result mapValues (xs => xs.zfoldl[View[A]]((res, x) => cond(res.m contains x, res, res :+ x)))) orElse (_ => view())
  }

  def zipCross[A, B](l: View[A], r: View[B]): Zip[A, B]                            = zipPairs( for (x <- l; y <- r) yield x -> y )
  def zipProducts[R, A, B](xs: View[R])(implicit z: IsProduct[R, A, B]): Zip[A, B] = zipPairs(xs map z.split)
  def zipMap[A, B](ls: => View[A])(f: A => B): Zip[A, B]                           = zipViews(ls, ls map f)
  def zipPairs[A, B](ps: => View[A -> B]): Zip[A, B]                               = new ZipPairs(ps)
  def zipViews[A, B](ls: => View[A], rs: => View[B]): Zip[A, B]                    = new ZipViews(ls, rs)
}
