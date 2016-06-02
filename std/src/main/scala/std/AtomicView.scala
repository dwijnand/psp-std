package psp
package std

import all._, View._

trait View[+A] extends Any with Foreach[A] {
  def size: Size = SizeOfView(this)
}

final case class IdView[A, R](underlying: Foreach[A]) extends RView[A, R]

sealed trait RView[A, R] extends View[A] with ViewClasses[A, R] {
  self =>

  def ++(that: View[A]): This                         = Joined(this, that)
  def +:(head: A): This                               = self prepend view(head)
  def :+(last: A): This                               = self append view(last)
  def append(that: View[A]): This                     = self ++ that
  def applyIndex(idx: Vdex): A                        = sliceIndex(idx).head
  def asDocs(implicit z: Show[A]): MapTo[Doc]         = self map (x => Doc(x))
  def asRefs: MapTo[Ref[A]]                           = self map castRef
  def asShown(implicit z: Show[A]): MapTo[String]     = self map z.show
  def by(eqv: Eq[A]): EqOps                           = new EqOps()(eqv)
  def byEquals: EqOps                                 = by(Relation.Inherited)
  def byRef: MapTo[Ref[A]]#EqOps                      = asRefs by Relation.Reference
  def byShow(implicit z: Show[A]): EqOps              = by(Eq.by[A](_.pp))
  def count(p: Pred): Int                             = foldl[Int](0)((res, x) => cond(p(x), res + 1, res))
  def dropIndex(idx: Vdex): This                      = splitAround(idx).join
  def exists(p: Pred): Bool                           = foldl(false)((res, x) => cond(p(x), return true, res))
  def filter(p: Pred): This                           = self withFilter p
  def filterNot(p: Pred): This                        = self filter !p
  def find(p: Pred): Option[A]                        = foldl(none[A])((res, x) => cond(p(x), return some(x), res))
  def foldl[B](zero: B)(f: (B, A) => B): B            = ll.foldLeft(self, zero, f)
  def foldr[B](zero: B)(f: (A, B) => B): B            = ll.foldRight(self, zero, f)
  def forall(p: Pred): Bool                           = !exists(!p)
  def grep(re: Regex)(implicit z: Show[A]): This      = self filter (re isMatch _)
  def head: A                                         = self headOr abort("empty")
  def headOr(alt: => A): A                            = self match { case HeadTailView(hd, _) => hd ; case _ => alt }
  def indexWhere(p: Pred): Vdex                       = zipIndex first { case (x, i) if p(x) => i }
  def init: This                                      = self dropRight 1
  def inits: Map2D[A]                                 = self +: zcond(!isEmpty, init.inits)
  def isEmpty: Bool                                   = size.isZero || !exists(ConstantTrue)
  def last: A                                         = self takeRight 1 head
  def mapIf(pf: A ?=> A): This                        = self map (x => pf.applyOr(x, x))
  def mapLive[B](columns: (A => B)*): Live[B]         = new Live(columns.m)
  def max(implicit z: Order[A]): A                    = reducel(all.max)
  def min(implicit z: Order[A]): A                    = reducel(all.min)
  def partition(p: Pred): Split                       = Split(self filter p, self filter !p)
  def prepend(that: View[A]): This                    = Joined(that, this)
  def reducel(f: BinOp[A]): A                         = tail.foldl(head)(f)
  def reducer(f: BinOp[A]): A                         = init.foldr(last)(f)
  def slice(r: VdexRange): This                       = zcond(!r.isEmpty, slice(r.head, r.size))
  def slice(start: Vdex, len: Precise): This          = self drop start.excluding take len
  def sliceIndex(start: Vdex): This                   = slice(start, _1)
  def sliceWhile(p: Pred, q: Pred): This              = self dropWhile p takeWhile q
  def sort(implicit z: Order[A]): This                = cast(toRefArray.inPlace.sort.m)
  def sortBy[B: Order](f: A => B): This               = sort(Order by f)
  def sortWith(isLess: Relation[A]): This             = sort(Order(isLess))
  def span(p: Pred): Split                            = Split(self takeWhile p, self dropWhile p)
  def splitAfter(len: Precise): Split                 = Split(self take len, self drop len)
  def splitAround(idx: Vdex): Split                   = splitAt(idx) mapRight (_ tail)
  def splitAt(idx: Vdex): Split                       = splitAfter(idx.excluding)
  def tail: This                                      = self drop 1
  def tails: Map2D[A]                                 = self +: zcond(!isEmpty, tail.tails)
  def takeToFirst(p: Pred): This                      = self span !p app ((x, y) => x ++ (y take 1))
  def tee(f: ToUnit[A]): This                         = self map (x => doto(x)(f))
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B = find(pf.isDefinedAt) map pf or z.empty
  def zfoldl[B: Empty](f: (B, A) => B): B             = ll.foldLeft(self, emptyValue[B], f)
  def zfoldr[B: Empty](f: (A, B) => B): B             = ll.foldRight(self, emptyValue[B], f)
  def zhead(implicit z: Empty[A]): A                  = headOr(z.empty)
  def zipIndex: Zip[A, Index]                         = zipViews(self, openIndices)
  def zipTail: Zip[A, A]                              = zipViews(self, tail)
  def zip[B](ys: View[B]): Zip[A, B]                  = zipViews(self, ys)
  def zlast(implicit z: Empty[A]): A                  = zcond(!isEmpty, last)
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A  = zcond(!isEmpty, reducel(f))
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A  = zcond(!isEmpty, reducer(f))

  /** Building, converting, forcing.
   */

  def build(implicit z: Makes[A, R]): R                        = force[R]
  def force[R](implicit z: Makes[A, R]): R                     = z make self
  def foreach(f: ToUnit[A]): Unit                              = foreachView(this)(f)
  def iterator: scIterator[A]                                  = toScalaStream.iterator
  def pairs[B, C](implicit z: IsProduct[A, B, C]): MapTo[B->C] = map(z.split)
  def toArray(implicit z: CTag[A]): Array[A]                   = to[Array]
  def toPset(implicit hz: Hash[A], ez: Eq[A]): Pset[A]         = to[Pset]
  def toRefArray(): Array[Ref[A]]                              = asRefs.force
  def toStream: Pstream[A]                                     = to[Pstream]
  def toScalaStream: sciStream[A]                              = to[sciStream]
  def toVec: Vec[A]                                            = to[Vec]
  def to[CC[X]](implicit z: Makes[A, CC[A]]): CC[A]            = z make self

  def seq: scSeq[A]          = to[scSeq]         // varargs or unapplySeq, usually
  def trav: scTraversable[A] = to[scTraversable] // scala flatMap, usually

  /** Reified view ops.
   */
  def collect[B](pf: A ?=> B): MapTo[B]     = View2(this, Collect(pf))
  def drop(n: Precise): This                = View0(this, Drop(n))
  def dropRight(n: Precise): This           = View0(this, DropRight(n))
  def dropWhile(p: Pred): This              = View1(this, DropWhile(p))
  def flatMap[B](f: A => View[B]): MapTo[B] = View2(this, FlatMap(f))
  def map[B](f: A => B): MapTo[B]           = View2(this, Mapped(f))
  def reverseView: This                     = View0(this, Reverse)
  def take(n: Precise): This                = View0(this, Take(n))
  def takeRight(n: Precise): This           = View0(this, TakeRight(n))
  def takeWhile(p: Pred): This              = View1(this, TakeWhile(p))
  def withFilter(p: Pred): This             = View1(this, Filter(p))
}

object View {
  def apply[A, R](xs: Foreach[A]): RView[A, R]                 = IdView(xs)
  def walks[A, R](xs: R)(implicit z: Walks[A, R]): RView[A, R] = apply(z walk xs)

  sealed trait Op0
  sealed trait Op1[A]
  sealed trait Op2[A, B]

  final case class Joined[A, R](ls: View[A], rs: View[A])           extends RView[A, R]
  final case class View0[A, R](prev: RView[A, R], op: Op0)          extends RView[A, R]
  final case class View1[A, R](prev: RView[A, R], op: Op1[A])       extends RView[A, R]
  final case class View2[A, B, R](prev: RView[A, R], op: Op2[A, B]) extends RView[B, R]

  final case class Drop(n: Precise)      extends Op0
  final case class DropRight(n: Precise) extends Op0
  final case class Take(n: Precise)      extends Op0
  final case class TakeRight(n: Precise) extends Op0
  final case object Reverse              extends Op0

  final case class Filter[A](p: ToBool[A])        extends Op1[A]
  final case class TakeWhile[A](p: ToBool[A])     extends Op1[A]
  final case class DropWhile[A](p: ToBool[A])     extends Op1[A]

  final case class Mapped[A, B](f: A => B)        extends Op2[A, B]
  final case class FlatMap[A, B](f: A => View[B]) extends Op2[A, B]
  final case class Collect[A, B](pf: A ?=> B)     extends Op2[A, B]

  def eachifyView[A](xs: View[A]): Each[A]            = suspend(runView(xs) _)
  def foreachView[A](xs: View[A])(f: ToUnit[A]): Unit = runView(xs)(x => f(x))

  def foldView[A, B](xs: View[A])(zero: B)(f: (B, A) => B): B = {
    var res = zero
    runView(xs)(x => res = f(res, x))
    res
  }

  def runView[C](xs: View[C])(f: ToUnit[C]): Unit = xs match {
    case OptimizeView(xs)        => runView(xs)(f)
    case IdView(xs)              => xs foreach f
    case Joined(xs, ys)          => runView(xs)(f); runView(ys)(f)
    case View0(xs, Reverse)      => runView(xs.toVec.reverse)(f)
    case View2(xs, Mapped(g))    => runView(xs)(g andThen f)
    case View2(xs, FlatMap(g))   => runView(xs)(x => g(x) foreach f)
    case View1(xs, Filter(p))    => runView(xs)(x => if (p(x)) f(x))
    case View2(xs, Collect(pf))  => runView(xs)(x => if (pf isDefinedAt x) f(pf(x)))
    case SlicedView(xs, range)   => ll.foreachSlice(xs, range, f)
    case View1(xs, TakeWhile(p)) => ll.foreachTakeWhile(xs, f, p)
    case View1(xs, DropWhile(p)) => ll.foreachDropWhile(xs, f, p)
    case View0(xs, DropRight(n)) => ll.foreachDropRight(xs, f, n)
    case View0(xs, TakeRight(n)) => ll.foreachTakeRight(xs, f, min(n, xs.size.preciseOrMaxLong))
    case View0(xs, Drop(n))      => ll.foreachSlice(xs, n.getLong.andUp map Index, f)
    case View0(xs, Take(n))      => ll.foreachSlice(xs, n.indices, f)
  }

  // XXX Figure out how to maintain laziness here.
  def unapplySeq[A](xs: View[A]): Some[scSeq[A]] = Some(xs.seq)
}
