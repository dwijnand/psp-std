package psp
package std

import api._, all._

abstract class StdDirect[A](val size: Precise) extends Direct[A] {
  final def foreach(f: A => Unit): Unit = size.indices foreach (i => f(apply(i)))
}

object Each {
  def apply[A](mf: Suspended[A]): Each[A]                          = construct(Size.Unknown, mf)
  def array[A](xs: Array[A]): WrapArray[A]                         = new WrapArray[A](xs)
  def const[A](elem: A): Each[A]                                   = apply(mf => while (true) mf(elem))
  def construct[A](size: Size, mf: Suspended[A]): Each[A]          = new WrapSuspended(size, mf)
  def continually[A](elem: => A): Each[A]                          = new WrapContinually(elem)
  def each[A](xs: Foreach[A]): Each[A]                             = construct(xs.size, xs foreach _)
  def javaMap[A, B](xs: jMap[A, B]): Each[A -> B]                  = construct(xs.size, mf => xs.entrySet foreach (k => mf(k.toPair)))
  def java[A](xs: jIterable[A]): Each[A]                           = apply(xs.iterator foreach _)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]                   = new WrapJoin(xs, ys)
  def jvmString(s: String): WrapString                             = new WrapString(s)
  def pair[R, A](x: R)(implicit z: Splitter[R, A, A]): WrapPair[A] = new WrapPair(z split x)
  def reversed[A](xs: Direct[A]): WrapReverse[A]                   = new WrapReverse(xs)
  def scalaMap[A, B](xs: scMap[A, B]): Each[A -> B]                = construct(xs.size, xs foreach _)
  def scala[A](xs: sCollection[A]): Each[A]                        = construct(xs.size, xs foreach _)
  def unapplySeq[A](xs: Foreach[A]): Some[scSeq[A]]                = Some(xs.seq)

  abstract class WrapEach[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    def foreach(f: A => Unit): Unit = mf(f)
  }
  abstract class WrapDirect[A, R](size: Precise, f: Long => A) extends StdDirect[A](size) {
    def apply(i: Vdex): A = f(i.indexValue)
  }
  final class WrapString(xs: String)        extends WrapDirect[Char, String](xs.length, xs charAt _.toInt)
  final class WrapArray[A](xs: Array[_])    extends WrapDirect[A, Array[A]](xs.length, i => cast[A](xs(i.toInt)))
  final class WrapReverse[A](xs: Direct[A]) extends WrapDirect[A, Direct[A]](xs.size, i => xs(xs.size.lastIndex - i))
  final class WrapPair[A](xs: PairOf[A])    extends WrapDirect[A, PairOf[A]](2, i => cond(i == 0, fst(xs), snd(xs)))

  final class WrapSuspended[A](size: Size, mf: Suspended[A]) extends WrapEach(size, mf)
  final class WrapJoin[A](xs: Each[A], ys: Each[A])          extends WrapEach[A](xs.size + ys.size, (xs foreach _) &&& (ys foreach _))
  final class WrapContinually[A](expr: => A)                 extends WrapEach[A](Infinite, f => while (true) f(expr))
}

object View2D {
  type Coords = PairOf[Vdex]

  def mpartition[A](xs: View[A])(p: View[A] => ToBool[A]): View2D[A] =
    xs partition p(xs) app ((ls, rs) => lazyView(ls +: mpartition(rs)(p)))

  class Ops[A](val xss: View2D[A]) extends AnyVal {
    import StdShow.showString

    def column(vdex: Vdex): View[A]   = xss flatMap (_ sliceIndex vdex)
    def transpose: View2D[A]          = openIndices map column
    def flatten: View[A]              = xss flatMap identity
    def mmap[B](f: A => B): View2D[B] = xss map (_ map f)

    def grid_s(implicit z: Show[A]): String = {
      val width = xss.mmap(_.show.length).flatten.max
      val fmt   = lformat(width)
      val yss   = xss mmap (x => fmt(z show x))

      (yss map (_.joinWords)).joinLines.trimLines
    }
  }

  class FunGrid[-A, +B](basis: View[A], functions: View[A => B]) extends (Coords => B) {
    def isEmpty: Bool        = basis.isEmpty || functions.isEmpty
    def apply(xy: Coords): B = xy app (rows applyIndex _ applyIndex _)
    def rows: View2D[B]      = basis map (r => functions map (_ apply r))
    def columns: View2D[B]   = rows.transpose

    def widths(implicit z: Show[B]): View[Int]   = columns map (_ map (_.show.length) max)
    def lines(implicit z: Show[B]): View[String] = cond(isEmpty, view(), widths zip rows map (lformat(_)(_)))
  }
}
