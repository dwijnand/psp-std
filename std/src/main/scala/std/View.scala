package psp
package std

import all._

class ViewOps[A, R](val xs: View[A]) extends ViewMethods[R, A] {
  type MapTo[B] = View[B]

  def view[A](xs: A*): MapTo[A]       = exp.view(xs: _*)
  def apply[B](op: Op[A, B]): View[B] = Operable.OperableView(xs)(op)

  protected def concat(xs: This, ys: This): This  = suspend[A](xs.foreach, ys.foreach)

  def inits: Map2D[A]                         = view(xs) ++ zcond(!isEmpty, init.inits)
  def tails: Map2D[A]                         = view(xs) ++ zcond(!isEmpty, tail.tails)
  def init: This                              = this dropRight 1
  def tail: This                              = this drop 1
  def asRefs: MapTo[Ref[A]]                   = xs map castRef
  def asDocs(implicit z: Show[A]): MapTo[Doc] = xs map (x => Doc(x))

  def collect[B](pf: A ?=> B): MapTo[B]     = xs collect pf
  def drop(n: Precise): This                = xs drop n
  def dropRight(n: Precise): This           = xs dropRight n
  def dropWhile(p: ToBool[A]): This         = xs dropWhile p
  def filter(p: ToBool[A]): View[A]         = xs withFilter p
  def flatMap[B](f: A => View[B]): MapTo[B] = xs flatMap f
  def map[B](f: A => B): MapTo[B]           = xs map f
  def reverseView: This                     = xs.reverseView
  def slice(r: VdexRange): View[A]          = zcond(!r.isEmpty, slice(r.head, r.size))
  def take(n: Precise): This                = xs take n
  def takeRight(n: Precise): This           = xs takeRight n
  def takeWhile(p: ToBool[A]): This         = xs takeWhile p
  def withFilter(p: ToBool[A]): This        = xs withFilter p
  def filterNot(p: ToBool[A]): This         = xs filter !p
  def append(that: View[A]): This           = concat(xs, that)
  def prepend(that: View[A]): This          = concat(that, xs)

  def partition(p: ToBool[A]): Split[A]  = Split(xs filter p, xs filter !p)
  def span(p: ToBool[A]): Split[A]       = Split(xs takeWhile p, xs dropWhile p)
  def splitAround(idx: Vdex): Split[A]   = splitAt(idx) mapRight (_ tail)
  def splitAt(idx: Vdex): Split[A]       = splitAfter(idx.excluding)
  def splitAfter(len: Precise): Split[A] = Split(xs take len, xs drop len)
  def dropIndex(idx: Vdex): View[A]      = splitAround(idx).join
  def takeToFirst(p: ToBool[A]): View[A] = this span !p app ((x, y) => x ++ (y take 1))

  def zipTail: Zip[A, A]             = zipViews(xs, xs.tail) // like "xs sliding 2" but better
  def zipIndex: Zip[A, Index]        = zipViews(xs, openIndices)
  def zip[B](ys: View[B]): Zip[A, B] = zipViews[A, B](xs, ys)
}

trait ViewMethods[R, A] {
  def xs: View[A]

  type MapTo[B] <: View[B]
  type This = MapTo[A]
  type Map2D[B] = MapTo[MapTo[B]]

  def view[A](xs: A*): MapTo[A]
  protected def concat(xs: This, ys: This): This

  class EqOps(implicit eqv: Eq[A]) {
    def contains(x: A): Boolean = xs exists (_ === x)
    def distinct: View[A]       = xs.zfoldl[View[A]]((res, x) => cond(res.m contains x, res, res :+ x))
    def indexOf(x: A): Vdex     = xs indexWhere (_ === x)
  }
  def zipTail: Zip[A, A]
  def zipIndex: Zip[A, Index]
  def zip[B](ys: View[B]): Zip[A, B]

  def partition(p: ToBool[A]): SplitView[This]
  def span(p: ToBool[A]): SplitView[This]
  def splitAround(idx: Vdex): SplitView[This]
  def splitAt(idx: Vdex): SplitView[This]
  def splitAfter(len: Precise): SplitView[This]
  def dropIndex(idx: Vdex): MapTo[A]
  def takeToFirst(p: ToBool[A]): MapTo[A]


  def collect[B](pf: A ?=> B): MapTo[B]
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def dropWhile(p: ToBool[A]): This
  def filter(p: ToBool[A]): This
  def flatMap[B](f: A => View[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def reverseView: This
  def slice(r: VdexRange): This
  def take(n: Precise): This
  def takeRight(n: Precise): This
  def takeWhile(p: ToBool[A]): This
  def withFilter(p: ToBool[A]): This
  def filterNot(p: ToBool[A]): This
  def append(that: View[A]): This
  def prepend(that: View[A]): This

  def init: This
  def tail: This
  def inits: Map2D[A]
  def tails: Map2D[A]

  def asRefs: MapTo[Ref[A]]
  def asDocs(implicit z: Show[A]): MapTo[Doc]
  def asShown(implicit z: Show[A]): MapTo[String] = this map z.show

  def by(eqv: Eq[A]): EqOps              = new EqOps()(eqv)
  def byEquals: EqOps                    = by(Relation.Inherited)
  def byRef                              = asRefs by Relation.Reference
  def byShow(implicit z: Show[A]): EqOps = by(Eq.by[A](_.pp))

  def ++(ys: View[A]): This = append(ys)
  def +:(head: A): This     = prepend(view(head))
  def :+(last: A): This     = append(view(last))

  def mapLive[B](columns: (A => B)*): View.Live[A, B] = new View.Live(xs, view(columns: _*))

  def grep(re: Regex)(implicit z: Show[A]): This      = this filter (re isMatch _)
  def mapIf(pf: A ?=> A): View[A]                     = this map (x => pf.applyOr(x, x))
  def sliceIndex(start: Vdex): View[A]                = slice(start, _1)
  def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A] = this dropWhile p takeWhile q
  def sort(implicit z: Order[A]): View[A]             = xs.toRefArray.inPlace.sort.m
  def sortBy[B: Order](f: A => B): View[A]            = sort(Order by f)
  def sortWith(isLess: Relation[A]): View[A]          = sort(Order(isLess))
  def tee(f: ToUnit[A]): View[A]                      = this map (x => doto(x)(f))

  def max(implicit z: Order[A]): A = reducel(all.max)
  def min(implicit z: Order[A]): A = reducel(all.min)

  def slice(start: Vdex, len: Precise): View[A] = this drop start.excluding take len

  def headOr(alt: => A): A = this take 1 match {
    case View(x) => x
    case _       => alt
  }
  def zhead(implicit z: Empty[A]): A = headOr(z.empty)
  def head: A = this headOr abort("empty")
  def last: A = xs takeRight 1 head

  def applyIndex(idx: Vdex): A                        = sliceIndex(idx).head
  def count(p: ToBool[A]): Int                        = foldl[Int](0)((res, x) => cond(p(x), res + 1, res))
  def exists(p: ToBool[A]): Boolean                   = foldl(false)((res, x) => cond(p(x), return true, res))
  def find(p: ToBool[A]): Option[A]                   = foldl(none[A])((res, x) => cond(p(x), return some(x), res))
  def forall(p: ToBool[A]): Boolean                   = !exists(!p)
  def indexWhere(p: ToBool[A]): Vdex                  = xs.zipIndex first { case (x, i) if p(x) => i }
  def isEmpty: Boolean                                = xs.size.isZero || !exists(ConstantTrue)
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B = find(pf.isDefinedAt) map pf or z.empty
  def zlast(implicit z: Empty[A]): A                  = zcond(!isEmpty, last)

  def foldl[B](zero: B)(f: (B, A) => B): B           = ll.foldLeft(xs, zero, f)
  def foldr[B](zero: B)(f: (A, B) => B): B           = ll.foldRight(xs, zero, f)
  def reducel(f: BinOp[A]): A                        = xs.tail.foldl(xs.head)(f)
  def reducer(f: BinOp[A]): A                        = xs.init.foldr(last)(f)
  def zfoldl[B: Empty](f: (B, A) => B): B            = ll.foldLeft(xs, emptyValue[B], f)
  def zfoldr[B: Empty](f: (A, B) => B): B            = ll.foldRight(xs, emptyValue[B], f)
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A = zcond(!isEmpty, reducel(f))
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A = zcond(!isEmpty, reducer(f))

  def to[CC[X]](implicit z: Makes[A, CC[A]]): CC[A] = z make xs
  def force[R](implicit z: Makes[A, R]): R          = z make xs
  def build(implicit z: Makes[A, R]): R             = force[R]

  def pairs[B, C](implicit z: IsProduct[A, B, C]): MapTo[B->C] = map(z.split)

  def iterator: scIterator[A]                          = toScalaStream.iterator
  def toArray(implicit z: CTag[A]): Array[A]           = to[Array]
  def toPset(implicit hz: Hash[A], ez: Eq[A]): Pset[A] = to[Pset]
  def toRefArray(): Array[Ref[A]]                      = asRefs.force
  def toScalaStream: sciStream[A]                      = to[sciStream]
  def toVec: Vec[A]                                    = to[Vec]

  def seq: scSeq[A]          = to[scSeq] // varargs or unapplySeq, usually
  def trav: scTraversable[A] = to[scTraversable] // scala flatMap, usually
}
