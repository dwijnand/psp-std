package psp
package std

import api._, all._

final class TerminalViewOps[A](val xs: View[A]) extends AnyVal {
  def count(p: ToBool[A]): Int                       = foldl[Int](0)((res, x) => cond(p(x), res + 1, res))
  def exists(p: ToBool[A]): Boolean                  = foldl(false)((res, x) => cond(p(x), return true, res))
  def find(p: ToBool[A]): Option[A]                  = foldl(none[A])((res, x) => cond(p(x), return Some(x), res))
  def findOr(p: ToBool[A], alt: => A): A             = find(p) | alt
  def foldl[B](zero: B)(f: (B, A) => B): B           = ll.foldLeft(xs, zero, f)
  def foldr[B](zero: B)(f: (A, B) => B): B           = ll.foldRight(xs, zero, f)
  def forall(p: ToBool[A]): Boolean                  = foldl(true)((_, x) => p(x) || { return false })
  def head: A                                        = (xs take 1).force.head
  def indexWhere(p: ToBool[A]): Index                = xs.zipIndex findLeft p map snd or NoIndex
  def isEmpty: Boolean                               = xs.size.isZero || !exists(true)
  def last: A                                        = xs takeRight 1 head
  def nonEmpty: Boolean                              = xs.size.isNonZero || exists(true)
  def reducel(f: BinOp[A]): A                        = xs.tail.foldl(head)(f)
  def reducer(f: BinOp[A]): A                        = xs.init.foldr(last)(f)
  def zfoldl[B : Empty](f: (B, A) => B): B           = ll.foldLeft(xs, emptyValue[B], f)
  def zfoldr[B : Empty](f: (A, B) => B): B           = ll.foldRight(xs, emptyValue[B], f)
  def zhead(implicit z: Empty[A]): A                 = zcond(nonEmpty, head)
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A = zcond(nonEmpty, reducel(f))
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A = zcond(nonEmpty, reducer(f))

  // Show[A]
  def joinLines(implicit z: Show[A]): String         = this mk_s EOL
  def join_s(implicit z: Show[A]): String            = this mk_s ""
  def mk_s(sep: Char)(implicit z: Show[A]): String   = this mk_s sep.to_s
  def mk_s(sep: String)(implicit z: Show[A]): String = xs map z.show zreducel ((l, r) => cond(l == "", "", l ~ sep) ~ r)
  def to_s(implicit z: Show[A]): String              = "[ " + (xs mk_s ", ") + " ]"

  // Order[A]
  def max(implicit z: Order[A]): A   = reducel(all.max)
  def maxOf[B : Order](f: A => B): B = xs map f max

  def byEquals         = new EqViewOps[A](xs)(Eq.Inherited)
  def byRef            = new EqViewOps[Ref[A]](xs.toRefs)(Eq.Reference)
  def byString         = new EqViewOps[A](xs)(Eq.ToString)
  def by(eqv: Hash[A]) = new EqViewOps[A](xs)(eqv)
}

final case class AView[A, B](xs: View[A], op: Op[A, B]) extends View[B] {
  def size: Size                  = op[ConstSize](xs.size)
  def foreach(f: B => Unit): Unit = op(xs) foreach f
}

final class ViewOps[A](val xs: View[A]) extends AnyVal {
  private implicit def applyNext[B](op: Op[A, B]): View[B] = xs match {
    case AView(xs, prev) => AView(xs, prev ~ op)
    case _               => new AView(xs, op)
  }

  // def apply(index: Index): A                                         = xs drop index.sizeExcluding head
  // def dropIndex(index: Index): View[A]                               = xs splitAt index mapRight (_ drop 1) rejoin
  // def first[B](pf: A ?=> B): Option[B]                               = find(pf.isDefinedAt) map pf
  // def fold[B](implicit z: Empty[B]): HasInitialValue[A, B]           = foldFrom(z.empty)
  // def forallTrue(implicit ev: A <:< Boolean): Boolean                = forall(ev)
  // def gatherClass[B: CTag] : View[B]                                 = xs collect classFilter[B]
  // def gather[B](p: Partial[A, View[B]]): View[B]                     = xs flatMap p.zapply
  // def indicesWhere(p: ToBool[A]): View[Index]                        = zipIndex filterLeft p rights
  // def intersperse(ys: View[A]): View[A]                              = Split(xs, ys).intersperse
  // def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C]     = xs map (f => ev(f)(x))
  // def mapBy[B: Eq, C](f: A => B, g: View[A] => C): ExMap[B, C]       = groupBy[B](f) map g // Probably this should be groupBy
  // def memo: Indexed.Memo[A]                                          = new Indexed.Memo(xs)
  // def minOf[B: Order](f: A => B): B                                  = xs map f min
  // def quotientSet(implicit z: Eq[A]): View[ExSet[A]]                 = groupBy[A](identity).values map (_.toExSet)
  // def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A]                = xs dropWhile p takeWhile q
  // def sortBy[B](f: A => B)(implicit z: Order[B]): View[A]            = orderOps(orderBy[A](f)).sorted
  // def sortWith(cmp: OrderRelation[A]): View[A]                       = orderOps(Order(cmp)).sorted
  // def splitAround(index: Index): A -> Split[A]                       = splitAt(index) |> (lr => (lr onRight (_.head)) -> (lr mapRight (_ tail)))
  // def takeToFirst(p: ToBool[A]): View[A]                             = xs span !p mapRight (_ take 1) rejoin
  // def tee(f: A => String): View[A]                                   = xs map (x => sideEffect(x, println(f(x))))
  // def unzip[L, R](implicit z: Splitter[A, L, R]): View[L] -> View[R] = zipped[L, R] |> (x => x.lefts -> x.rights)
  // def withSize(size: Size): View[A]                                  = new Each.Impl[A](size, xs foreach _)
  // def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B                = find(pf.isDefinedAt).fold(z.empty)(pf)

  def filter(p: ToBool[A]): View[A]                    = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                 = xs withFilter !p
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs filter (regex isMatch _)
  def init: View[A]                                    = xs dropRight 1
  def mapIf(pf: Partial[A, A]): View[A]                = xs map (x => pf.applyOr(x, x))
  def slice(range: VdexRange): View[A]                 = xs drop range.startLong take range.size
  def sorted(implicit z: Order[A]): View[A]            = xs.toRefArray.inPlace.sort
  def tail: View[A]                                    = xs drop 1

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

  def cross[B](ys: View[B]): Zip[A, B]     = crossViews(xs, ys)
  def mapAndCross[B](f: A => B): Zip[A, B] = crossViews(xs, xs map f)
  def mapAndZip[B](f: A => B): Zip[A, B]   = zipViews(xs, xs map f)
  def zipIndex: Zip[A, Index]              = zipViews(xs, openIndices)
  def zip[B](ys: View[B]): Zip[A, B]       = zipViews[A, B](xs, ys)

  def zipped[L, R](implicit z: Splitter[A, L, R]): Zip[L, R] = zipSplit[A, L, R](xs)(z)

  def partition(p: ToBool[A]): Split[A] = Split(withFilter(p), withFilter(!p))
  def span(p: ToBool[A]): Split[A]      = Split(takeWhile(p), dropWhile(p))
  def splitAt(index: Index): Split[A]   = Split(xs take index.sizeExcluding, xs drop index.sizeExcluding)

  def mpartition(p: View[A] => ToBool[A]): View2D[A] = (
    inView[View[A]](
      mf =>
        xs partition p(xs) match {
      case Split(xs, ys) =>
        mf(xs)
        ys mpartition p foreach mf
  })
  )
}

final class View2DOps[A](val xss: View2D[A]) extends AnyVal {
  def column(vdex: Vdex): View[A]   = xss flatMap (_ drop vdex.sizeExcluding take 1)
  def transpose: View2D[A]          = openIndices map column
  def flatten: View[A]              = xss flatMap identity
  def mmap[B](f: A => B): View2D[B] = xss map (_ map f)
}
