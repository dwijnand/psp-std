package psp
package std

import api._, all._

trait RepView[R, A] extends ViewMethods[R, A] with View[A] with RepView.Derived[R, A] {
  type MapTo[B] = RepView[R, B]

  def view[B](xs: B*): MapTo[B]
  def apply[B](next: Op[A, B]): MapTo[B]

  def init: This            = this dropRight 1
  def tail: This            = this drop 1
  def inits: Map2D[A]       = this +: zcond(!isEmpty, init.inits)
  def tails: Map2D[A]       = this +: zcond(!isEmpty, tail.tails)
  def asRefs: MapTo[Ref[A]] = this map castRef

  def asDocs(implicit z: Show[A]): MapTo[Doc]     = this map (x => Doc(x))
  def mkDoc(sep: Doc)(implicit z: Show[A]): Doc   = asDocs zreducel (_ ~ sep ~ _)

  def collect[B](pf: A ?=> B): MapTo[B]     = apply(Op.Collect(pf))
  def drop(n: Precise): This                = apply(Op.Drop[A](n))
  def dropRight(n: Precise): This           = apply(Op.DropRight[A](n))
  def dropWhile(p: ToBool[A]): This         = apply(Op.DropWhile(p))
  def filter(p: ToBool[A]): This            = apply(Op.Filter(p))
  def flatMap[B](f: A => View[B]): MapTo[B] = apply(Op.FlatMap(f))
  def map[B](f: A => B): MapTo[B]           = apply(Op.Maps(f))
  def slice(r: VdexRange): This             = apply(Op.Slice[A](r))
  def take(n: Precise): This                = apply(Op.Take[A](n))
  def takeRight(n: Precise): This           = apply(Op.TakeRight[A](n))
  def takeWhile(p: ToBool[A]): This         = apply(Op.TakeWhile(p))
  def withFilter(p: ToBool[A]): This        = apply(Op.Filter(p))
  def filterNot(p: ToBool[A]): This         = apply(Op.Filter(!p))
  def append(that: View[A]): This           = apply(Op.Append(that))
  def prepend(that: View[A]): This          = apply(Op.Prepend(that))
  def reverseView: This                     = apply(Op.Reverse())
}

object RepView {
  type View2D[R, A] = RepView[R, RepView[R, A]]

  trait Derived[R, A] {
    self: RepView[R, A] =>

    type M[X] = RepView[R, X]
    type V    = M[A]
    type Pred = ToBool[A]

    def zipCross[B, C](l: M[B], r: M[C]): Zip[B, C]                           = new ZipFromCross(l, r)
    def zipSplit[A, B, C](xs: M[A])(implicit z: Splitter[A, B, C]): Zip[B, C] = new ZipFromSplit(xs)
    def zipViews[B, C](l: M[B], r: M[C]): Zip[B, C]                           = new ZipFromViews(l, r)
    def zipPairs[B, C](xs: M[B->C]): Zip[B, C]                                = new ZipFromPairs(xs)
    def zipMap[B](f: A => B): Zip[A, B]                                       = new ZipFromFun(self, f)

    class ZipFromSplit[A, B, C](xs: M[A])(implicit z: Splitter[A, B, C]) extends Zip[B, C] {
      def pairs: M[B->C] = xs map z.split
      def lefts: M[B]    = pairs map fst
      def rights: M[C]   = pairs map snd
    }
    case class ZipFromViews[B, C](lefts: M[B], rights: M[C]) extends Zip[B, C] {
      def pairs: M[B->C] = insist("ZipFromViews")( Each scalaOnce new ZipIterator(lefts.iterator, rights.iterator) )
    }
    case class ZipFromPairs[B, C](pairs: M[B->C]) extends Zip[B, C] {
      def lefts: M[B]    = pairs map fst
      def rights: M[C]   = pairs map snd
    }
    case class ZipFromFun[B, C](lefts: M[B], f: B => C) extends Zip[B, C] {
      def pairs  = lefts map (x => x -> f(x))
      def rights = lefts map f
    }
    case class ZipFromCross[B, C](lv: M[B], rv: M[C]) extends Zip[B, C] {
      def pairs: M[B->C] = for (x <- lv; y <- rv) yield x -> y
      def lefts: M[B]    = pairs map fst
      def rights: M[C]   = pairs map snd
    }

    trait Zip[B, C] extends psp.std.Zip[B, C] {
      type This  = Zip[B, C]
      type BPred = (B, C) => Bool

      def lefts: M[B]    // the left element of each pair. Moral equivalent of pairs map fst.
      def rights: M[C]   // the right element of each pair. Moral equivalent of pairs map snd.
      def pairs: M[B->C] // the pairs. Moral equivalent of lefts zip rights.

      def foldl[D](zero: D)(f: (D, B, C) => D): D =
        ll.foldLeft[B->C, D](pairs, zero, (res, x) => f(res, fst(x), snd(x)))

      def find(p: BPred): Opt[B->C] =
        foldl(none())((res, x, y) => cond(p(x, y), return some(x -> y), res))

      def foreach(f: (B, C) => Unit): Unit = (lefts, rights) match {
        case (xs: Direct[B], ys) => cast[Precise](xs.size).indices zip ys mapLeft xs.apply
        case (xs, ys: Direct[C]) => xs zip cast[Precise](ys.size).indices mapRight ys.apply
        case _                   => lefts.iterator |> (it => rights foreach (y => cond(it.hasNext, f(it.next, y), return )))
      }

      def corresponds(p: BPred): Bool          = iterator |> (it => it.forall(_ app p) && !it.hasMore)
      def drop(n: Precise): This               = zipSplit(pairs drop n)
      def exists(p: BPred): Bool               = !forall(!p)
      def filter(p: BPred): This               = withFilter(p)
      def filterLeft(p: ToBool[B]): This       = withFilter((x, _) => p(x))
      def filterRight(p: ToBool[C]): This      = withFilter((_, y) => p(y))
      def first[D: Empty](pf: (B->C) ?=> D): D = pairs zfirst pf
      def forall(p: BPred): Bool               = iterator forall (_ app p)
      def iterator: ZipIterator[B, C]          = new ZipIterator(lefts.iterator, rights.iterator)
      def mapLeft[B1](f: B => B1): Zip[B1, C]  = zipViews(lefts map f, rights)
      def mapRight[C1](f: C => C1): Zip[B, C1] = zipViews(lefts, rights map f)
      def map[D](f: (B, C) => D): M[D]         = pairs map (_ app f)
      def take(n: Precise): This               = zipSplit(pairs take n)
      def unzip: M[B] -> M[C]                  = lefts -> rights
      def withFilter(p: BPred): This           = zipPairs(pairs filter (_ app p))

      def force[R](implicit z: Makes[B->C, R]): R = z make pairs
    }

    /** When a View is split into two disjoint views.
      * Notably, that's span, partition, and splitAt.
      */
    case class Split(leftView: V, rightView: V) extends SplitView[V] {
      type This = Split
      def remake(l: V, r: V): This = Split(l, r)

      def collate: V       = rightView.iterator |> (it => leftView.zfoldl[V]((res, x) => if (it.isEmpty) return res else res :+ x :+ it.next))
      def cross: Zip[A, A] = app(zipCross)
      def join: V          = leftView ++ rightView
      def pairs: M[A->A]   = zip.pairs
      def zip: Zip[A, A]   = app(zipViews)
    }

    def partition(p: Pred): Split        = Split(this filter p, this filter !p)
    def span(p: Pred): Split             = Split(this takeWhile p, this dropWhile p)
    def splitAround(idx: Vdex): Split    = splitAt(idx) mapRight (_ tail)
    def splitAt(idx: Vdex): Split        = splitAtTake(idx.excluding)
    def splitAtTake(len: Precise): Split = Split(this take len, this drop len)
    def dropIndex(idx: Vdex):V           = splitAround(idx).join
    def takeToFirst(p: Pred): V          = this span !p app ((x, y) => x ++ (y take 1))

    def zipTail: Zip[A, A]      = zipViews(self, tail)
    def zipIndex: Zip[A, Index] = zipViews(self, insist("zipIndex")(openIndices))
    def zip[B](ys: View[B]): Zip[A, B] = zipViews(self, insist("zip")(ys))
  }

  def empty[R, A]: RepView[R, A]                                 = insist("empty")(view())
  def apply[R, A](xs: R)(implicit z: Walks[A, R]): RepView[R, A] = insist("apply")(z walk xs)
  def insist[R](str: String): ImplHelper[R]                      = new ImplHelper[R](str)

  class ImplHelper[R](str: String) {
    def apply[A](body: View[A]): RepView[R, A] = new Impl[R, A, A](Op[A](str), body)
  }

  class Impl[R, Z, A](val op: Op[Z, A], val basis: View[Z]) extends RepView[R, A] {
    def apply[B](next: Op[A, B]): MapTo[B] = new Impl(op ~ next, basis)
    def view[B](xs: B*): RepView[R, B]     = insist(any"view(<${xs.length} elems>)")(exp.view(xs: _*))

    def xs: View[A] = op[View](basis)
    def opDoc: Doc  = op[ConstDoc](Doc.empty)

    // log"$opDoc: ${ Doc(this map (_.any_s) joinWords) }"
    def foreach(f: A => Unit): Unit = xs foreach f
  }
  implicit class RepView2DOps[R, Z, A](xss: View2D[R, A]) {
    import all._

    type Sliver   = RepView[R, A]
    type MapTo[B] = RepView.View2D[R, B]

    def column(vdex: Vdex): Sliver   = xss flatMap (_ sliceIndex vdex)
    def transpose: MapTo[A]          = ??? // RepView(openIndices)(???) map column // ??? // RepWithOp(openIndices)(???) map column
    def flatten: Sliver              = xss flatMap (x => x)
    def mmap[B](f: A => B): MapTo[B] = xss map (_ map f)

    def grid_s(implicit z: Show[A]): String = {
      val width = xss.mmap(z show _ length).flatten.max
      val fmt   = lformat(width)
      val yss   = xss mmap (x => fmt(z show x))
      val lines = yss map (_ joinWith " ")

      lines.joinLines mapLines (_.trim)
    }
  }
}
