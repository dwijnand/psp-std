package psp
package std
package ops

import api._, all._, StdEq._

final class TerminalViewOps[A](val xs: View[A]) extends AnyVal {
  def count(p: ToBool[A]): Int                        = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def exists(p: ToBool[A]): Boolean                   = foldl(false)((res, x) => if (p(x)) return true else res)
  def find(p: ToBool[A]): Option[A]                   = foldl(none[A])((res, x) => if (p(x)) return Some(x) else res)
  def findOr(p: ToBool[A], alt: => A): A              = find(p) | alt
  def foldl[B](zero: B)(f: (B, A) => B): B            = ll.foldLeft(xs, zero, f)
  def foldr[B](zero: B)(f: (A, B) => B): B            = ll.foldRight(xs, zero, f)
  def forall(p: ToBool[A]): Boolean                   = foldl(true)((_, x) => p(x) || { return false })
  def head: A                                         = (xs take 1).force.head
  def indexWhere(p: ToBool[A]): Index                 = xs.zipIndex findLeft p map snd or NoIndex
  def isEmpty: Boolean                                = xs.size.isZero || !exists(true)
  def last: A                                         = xs takeRight 1 head
  def max(implicit z: Order[A]): A                    = xs reducel (_ max _)
  def maxOf[B: Order](f: A => B): B                   = xs map f max
  def nonEmpty: Boolean                               = xs.size.isNonZero || exists(true)
  def product(implicit z: MultiplicativeMonoid[A]): A = z prod xs.trav
  def reducel(f: BinOp[A]): A                         = xs.tail.foldl(head)(f)
  def reducer(f: BinOp[A]): A                         = xs.init.foldr(last)(f)
  def sum(implicit z: AdditiveMonoid[A]): A           = z sum xs.trav
  def zfoldl[B: Empty](f: (B, A) => B): B             = ll.foldLeft(xs, emptyValue[B], f)
  def zfoldr[B: Empty](f: (A, B) => B): B             = ll.foldRight(xs, emptyValue[B], f)
  def zhead(implicit z: Empty[A]): A                  = xs match { case Each(head, _*) => head ; case _ => z.empty }
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A  = if (xs.isEmpty) z.empty else reducel(f)
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A  = if (xs.isEmpty) z.empty else reducer(f)

  def joinLines(implicit z: Show[A]): String         = this mk_s EOL
  def join_s(implicit z: Show[A]): String            = this mk_s ""
  def mk_s(sep: Char)(implicit z: Show[A]): String   = this mk_s sep.to_s
  def mk_s(sep: String)(implicit z: Show[A]): String = xs map z.show zreducel ((l, r) => cond(l == "", "", l ~ sep) ~ r)
  def to_s(implicit z: Show[A]): String              = "[ " + ( xs mk_s ", " ) + " ]"
}

final case class AView[A, B](xs: View[A], op: Op[A, B]) extends View[B] {
  def size: Size                  = op[Op.ConstSize](xs.size)
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

  def cross[B](ys: View[B]): View[A -> B]              = for (x <- xs ; y <- ys) yield x -> y
  def filter(p: ToBool[A]): View[A]                    = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                 = xs withFilter !p
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs filter (regex isMatch _)
  def init: View[A]                                    = xs dropRight 1
  def mapIf(pf: Partial[A, A]): View[A]                = xs map (x => pf.applyOr(x, x))
  def slice(range: VdexRange): View[A]                 = xs drop range.startInt take range.size
  def sorted(implicit z: Order[A]): View[A]            = xs.toRefArray.inPlace.sort
  def tail: View[A]                                    = xs drop 1

  def collect[B](pf: A ?=> B): View[B]        = Op.Collect(pf)
  def drop(n: Precise): View[A]               = Op.Pos[A](Op.Drop(n))
  def dropRight(n: Precise): View[A]          = Op.Pos[A](Op.DropRight(n))
  def dropWhile(p: ToBool[A]): View[A]        = Op.DropWhile(p)
  def flatMap[B](f: A => Foreach[B]): View[B] = Op.FlatMap(f)
  def map[B](f: A => B): View[B]              = Op.Maps(f)
  def take(n: Precise): View[A]               = Op.Pos[A](Op.Take(n))
  def takeRight(n: Precise): View[A]          = Op.Pos[A](Op.TakeRight(n))
  def takeWhile(p: ToBool[A]): View[A]        = Op.TakeWhile(p)
  def withFilter(p: ToBool[A]): View[A]       = Op.Filter(p)

  def mapZip[B](f: A => B): ZipView[A, B]                        = Zip.zip2(xs, xs map f)
  def zipIndex: ZipView[A, Index]                                = xs zip indices.all
  def zip[B](ys: View[B]): ZipView[A, B]                         = Zip.zip2[A, B](xs, ys)
  def zipped[L, R](implicit z: Splitter[A, L, R]): ZipView[L, R] = Zip.zip0[A, L, R](xs)(z)

  def partition(p: ToBool[A]): Split[A] = Split(withFilter(p), withFilter(!p))
  def span(p: ToBool[A]): Split[A]      = Split(takeWhile(p), dropWhile(p))
  def splitAt(index: Index): Split[A]   = Split(xs take index.sizeExcluding, xs drop index.sizeExcluding)

  def mpartition(p: View[A] => ToBool[A]): View2D[A] = (
    inView[View[A]](mf => xs partition p(xs) match {
      case Split(xs, ys) =>
        mf(xs)
        ys mpartition p foreach mf
    })
  )
}

final class View2DOps[A](val xss: View2D[A]) {
  def column(vdex: Vdex): View[A]   = xss flatMap (_ drop vdex.sizeExcluding take 1)
  def transpose: View2D[A]          = indices.all map column
  def flatten: View[A]              = xss flatMap identity
  def mmap[B](f: A => B): View2D[B] = xss map (_ map f)
}

/** Methods requiring us to have additional knowledge, by parameter or type class.
 *  We keep the interface simple and foolproof by establishing thet instance
 *  first and only offering the methods after that.
 *
 *  But.
 *
 *  This approach, so nice in principle, stretches scala's willingness to connect
 *  implicits past its abilities. It works on psp collections, but when we depend on
 *  an implicit to entire Viewville in the first place, then these methods become
 *  out of reach without an implicit call to .m to become a view.
 *
 *  The search continues.
 */
class HasEq[A](xs: View[A])(implicit z: Eq[A]) {
  def contains(x: A): Boolean = xs exists (_ === x)
  def distinct: View[A]       = xs.zfoldl[Vec[A]]((res, x) => if (new HasEq(res) contains x) res else res :+ x)
  def indexOf(x: A): Index    = xs indexWhere (_ === x)
  def toSet: ExSet[A]         = xs.toExSet

  // def indicesOf(x: A): View[Index]                = xs indicesWhere (_ === x)
  // def mapOnto[B](f: A => B): ExMap[A, B]          = toSet mapOnto f
  // def toBag: Bag[A]                               = xs groupBy identity map (_.size.getInt)
  // def without(x: A): View[A]                      = xs filterNot (_ === x)
  // def withoutEmpty(implicit z: Empty[A]): View[A] = this without z.empty
}
class HasHash[A](xs: View[A])(implicit z: Hash[A]) extends HasEq[A](xs)(z) {
  override def toSet: ExSet[A] = xs.toHashSet
}
