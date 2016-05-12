package psp
package std

import api._, all._

trait TerminalViewOps[A] extends Any {
  def xs: View[A]

  def foldl[B](zero: B)(f: (B, A) => B): B = ll.foldLeft(xs, zero, f)

  def head: A = xs match {
    case IdView(xs: Direct[A]) => xs(Index(0))
    case _                     => (xs take 1).force.head
  }

  def count(p: ToBool[A]): Int             = foldl[Int](0)((res, x) => cond(p(x), res + 1, res))
  def exists(p: ToBool[A]): Boolean        = foldl(false)((res, x) => cond(p(x), return true, res))
  def find(p: ToBool[A]): Option[A]        = foldl(none[A])((res, x) => cond(p(x), return Some(x), res))
  def findOr(p: ToBool[A], alt: => A): A   = find(p) | alt
  def foldr[B](zero: B)(f: (A, B) => B): B = ll.foldRight(xs, zero, f)
  def forall(p: ToBool[A]): Boolean        = foldl(true)((_, x) => p(x) || { return false })
  def indexWhere(p: ToBool[A]): Index      = xs.zipIndex findLeft p map snd or NoIndex
  def isEmpty: Boolean                     = xs.size.isZero || !exists(true)
  def last: A                              = xs takeRight 1 head
  def nonEmpty: Boolean                    = xs.size.isNonZero || exists(true)
  def reducel(f: BinOp[A]): A              = xs.tail.foldl(head)(f)
  def reducer(f: BinOp[A]): A              = xs.init.foldr(last)(f)

  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B = find(pf.isDefinedAt) map pf or z.empty
  def zfoldl[B : Empty](f: (B, A) => B): B            = ll.foldLeft(xs, emptyValue[B], f)
  def zfoldr[B : Empty](f: (A, B) => B): B            = ll.foldRight(xs, emptyValue[B], f)
  def zhead(implicit z: Empty[A]): A                  = zcond(nonEmpty, head)
  def zlast(implicit z: Empty[A]): A                  = zcond(nonEmpty, last)
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A  = zcond(nonEmpty, reducel(f))
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A  = zcond(nonEmpty, reducer(f))

  // Show[A]
  def joinLines(implicit z: Show[A]): String         = mk_s(EOL)(z)
  def joinWords(implicit z: Show[A]): String         = mk_s(" ")(z)
  def join_s(implicit z: Show[A]): String            = mk_s("")(z)
  def mk_s(sep: Char)(implicit z: Show[A]): String   = this mk_s sep.to_s
  def mk_s(sep: String)(implicit z: Show[A]): String = xs map z.show zreducel ((l, r) => cond(l == "", "", l ~ sep) ~ r)
  def to_s(implicit z: Show[A]): String              = "[ " + (xs mk_s ", ") + " ]"

  // Order[A]
  def max(implicit z: Order[A]): A   = reducel(all.max)
  def min(implicit z: Order[A]): A   = reducel(all.min)
  def maxOf[B : Order](f: A => B): B = xs map f max

  def byEquals         = new EqViewOps[A](xs)(Eq.Inherited)
  def byRef            = new EqViewOps[Ref[A]](xs.toRefs)(Eq.Reference)
  def byString         = new EqViewOps[A](xs)(Eq.ToString)
  def by(eqv: Hash[A]) = new EqViewOps[A](xs)(eqv)
}

final case class AView[R, A, B](xs: View[A], op: Op[A, B]) extends View[B] {
  def size: Size                  = op[ConstSize](xs.size)
  def foreach(f: B => Unit): Unit = op(xs) foreach f
}
object AView {
  def apply[R, A, B](xs: R)(implicit z: ViewsAs[A, R]): AView[R, A, A] =
    new AView[R, A, A](z viewAs xs, Op(classNameOf(xs)))
}

final class ViewOps[R, A](val xs: View[A]) extends AnyVal with TerminalViewOps[A] {
  private type Next[B] = AView[R, A, B]
  private implicit def applyNext[B](op: Op[A, B]): Next[B] = xs match {
    case AView(xs, prev) => AView[R, A, B](cast(xs), prev ~ op)
    case _               => AView[R, A, B](xs, op)
  }

  // def gatherClass[B: CTag] : View[B]             = xs collect classFilter[B]
  // def gather[B](p: Partial[A, View[B]]): View[B] = xs flatMap p.zapply
  // def tee(f: A => String): View[A]               = xs map (x => sideEffect(x, println(f(x))))

  def tee(f: ToUnit[A]): View[A]                       = xs map (x => doto(x)(f))
  def filter(p: ToBool[A]): View[A]                    = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                 = xs withFilter !p
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs withFilter (regex isMatch _)
  def mapIf(pf: A ?=> A): View[A]                      = xs map (x => pf.applyOr(x, x))
  def slice(start: Vdex, len: Precise): View[A]        = xs drop Size(start.indexValue) take len
  def slice(r: VdexRange): View[A]                     = if (r.isEmpty) view() else slice(r.head, r.size)

  def sort(implicit z: Order[A]): View[A]      = xs.toRefArray.inPlace.sort
  def sortBy[B: Order](f: A => B): View[A]     = sort(orderBy[A](f))
  def sortWith(cmp: OrderRelation[A]): View[A] = sort(Order(cmp))

  def takeToFirst(p: ToBool[A]): View[A]              = xs span !p mapRight (_ take 1) join
  def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A] = xs dropWhile p takeWhile q
  def sliceIndex(idx: Vdex): View[A]                  = xs drop idx.excluding take 1
  def applyIndex(idx: Vdex): A                        = sliceIndex(idx).head

  def init: View[A]    = xs dropRight 1
  def tail: View[A]    = xs drop 1
  def tails: View2D[A] = cond(isEmpty, view(), view(xs) ++ tail.tails)
  def inits: View2D[A] = cond(isEmpty, view(), view(xs) ++ init.inits)

  def collect[B](pf: A ?=> B): View[B]        = Op.Collect(pf)
  def drop(n: Precise): View[A]               = Op.Drop[A](n)
  def dropRight(n: Precise): View[A]          = Op.DropRight[A](n)
  def dropWhile(p: ToBool[A]): View[A]        = Op.DropWhile(p)
  def flatMap[B](f: A => Foreach[B]): View[B] = Op.FlatMap(f)
  def map[B](f: A => B): View[B]              = Op.Maps(f)
  def take(n: Precise): View[A]               = Op.Take[A](n)
  def takeRight(n: Precise): View[A]          = Op.TakeRight[A](n)
  def takeWhile(p: ToBool[A]): View[A]        = Op.TakeWhile(p)
  def withFilter(p: ToBool[A]): View[A]       = Op.Filter(p)

  def ++(ys: View[A]): View[A] = inView { mf => xs foreach mf ; ys foreach mf }
  def +:(head: A): View[A]     = view(head) ++ xs
  def :+(last: A): View[A]     = xs ++ view(last)

  def cross[B](ys: View[B]): Zip[A, B]     = crossViews(xs, ys)
  def mapAndCross[B](f: A => B): Zip[A, B] = crossViews(xs, xs map f)
  def mapAndZip[B](f: A => B): Zip[A, B]   = zipViews(xs, xs map f)
  def zipIndex: Zip[A, Index]              = zipViews(xs, openIndices)
  def zip[B](ys: View[B]): Zip[A, B]       = zipViews[A, B](xs, ys)

  def zipped[L, R](implicit z: Splitter[A, L, R]): Zip[L, R] = zipSplit(xs)

  def partition(p: ToBool[A]): Split[A]  = Split(xs withFilter p, xs withFilter !p)
  def span(p: ToBool[A]): Split[A]       = Split(xs takeWhile p, xs dropWhile p)
  def splitAt(idx: Vdex): Split[A]       = Split(xs take idx.excluding, xs drop idx.excluding)
  def splitAround(idx: Vdex): Split[A]   = splitAt(idx) mapRight (_ drop 1)
  def dropIndex(idx: Index): View[A]     = splitAround(idx).join
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
      val width = (xss mmap (_.show)).flatten maxOf (_.length)
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
