package psp
package std

import api._, all._

class ViewOps[R, A](private val xs: View[A]) extends AnyVal {
  def by(eqv: Hash[A]): EqViewOps[A] = new EqViewOps[A](xs)(eqv)
  def byEquals: EqViewOps[A]         = by(Relation.Inherited)
  def byRef: EqViewOps[Ref[A]]       = new EqViewOps[Ref[A]](asRefs)(Relation.Reference)
  def byToString: EqViewOps[A]       = by(Relation.ToString)

  def partition(p: ToBool[A]): Split[A]   = Split(xs withFilter p, xs withFilter !p)
  def span(p: ToBool[A]): Split[A]        = Split(xs takeWhile p, xs dropWhile p)
  def splitAround(idx: Vdex): Split[A]    = splitAt(idx) mapRight (_ tail)
  def splitAt(idx: Vdex): Split[A]        = splitAtTake(idx.excluding)
  def splitAtTake(len: Precise): Split[A] = Split(xs take len, xs drop len)
  def zipTail: Zip[A, A]                  = zipViews(xs, xs.tail)
  def zipIndex: Zip[A, Index]             = zipViews(xs, openIndices)
  def zip[B](ys: View[B]): Zip[A, B]      = zipViews[A, B](xs, ys)

  def +:(head: A): View[A]                             = view(head) ++ xs
  def :+(last: A): View[A]                             = xs ++ view(last)
  def dropIndex(idx: Vdex): View[A]                    = splitAround(idx).join
  def filter(p: ToBool[A]): View[A]                    = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                 = xs withFilter !p
  def grep(regex: Regex)(implicit z: Show[A]): View[A] = xs withFilter (regex isMatch _)
  def mapIf(pf: A ?=> A): View[A]                      = xs map (x => pf.applyOr(x, x))
  def slice(r: VdexRange): View[A]                     = zcond(!r.isEmpty, slice(r.head, r.size))
  def slice(start: Vdex, len: Precise): View[A]        = xs drop Size(start.indexValue) take len
  def sliceIndex(idx: Vdex): View[A]                   = xs drop idx.excluding take 1
  def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A]  = xs dropWhile p takeWhile q
  def sort(implicit z: Order[A]): View[A]              = xs.toRefArray.inPlace.sort
  def sortBy[B: Order](f: A => B): View[A]             = sort(orderBy[A](f))
  def sortWith(cmp: OrderRelation[A]): View[A]         = sort(Relation order cmp)
  def takeToFirst(p: ToBool[A]): View[A]               = xs span !p app ((x, y) => x ++ (y take 1))
  def tee(f: ToUnit[A]): View[A]                       = xs map (x => doto(x)(f))

  def init: View[A]    = xs dropRight 1
  def inits: View2D[A] = view(xs) ++ zcond(!isEmpty, init.inits)
  def tail: View[A]    = xs drop 1
  def tails: View2D[A] = view(xs) ++ zcond(!isEmpty, tail.tails)

  def joinLines(implicit z: Show[A]): String         = mk_s(EOL)(z)
  def joinWords(implicit z: Show[A]): String         = mk_s(" ")(z)
  def join_s(implicit z: Show[A]): String            = mk_s("")(z)
  def mk_s(sep: Char)(implicit z: Show[A]): String   = this mk_s sep.to_s
  def mk_s(sep: String)(implicit z: Show[A]): String = xs map z.show zreducel ((l, r) => zcond(l =!= "", l ~ sep) ~ r)
  def to_s(implicit z: Show[A]): String              = "[ " + (xs mk_s ", ") + " ]"

  def max(implicit z: Order[A]): A = reducel(all.max)
  def min(implicit z: Order[A]): A = reducel(all.min)

  def applyIndex(idx: Vdex): A                        = sliceIndex(idx).head
  def count(p: ToBool[A]): Int                        = foldl[Int](0)((res, x) => cond(p(x), res + 1, res))
  def exists(p: ToBool[A]): Boolean                   = foldl(false)((res, x) => cond(p(x), return true, res))
  def find(p: ToBool[A]): Option[A]                   = foldl(none[A])((res, x) => cond(p(x), return some(x), res))
  def forall(p: ToBool[A]): Boolean                   = !exists(!p)
  def indexWhere(p: ToBool[A]): Index                 = xs.zipIndex first { case (x, i) if p(x) => i }
  def isEmpty: Boolean                                = xs.size.isZero || !exists(true)
  def last: A                                         = xs takeRight 1 head
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B = find(pf.isDefinedAt) map pf or z.empty
  def zhead(implicit z: Empty[A]): A                  = zcond(!isEmpty, xs.head)
  def zlast(implicit z: Empty[A]): A                  = zcond(!isEmpty, last)

  def foldl[B](zero: B)(f: (B, A) => B): B           = ll.foldLeft(xs, zero, f)
  def foldr[B](zero: B)(f: (A, B) => B): B           = ll.foldRight(xs, zero, f)
  def reducel(f: BinOp[A]): A                        = xs.tail.foldl(xs.head)(f)
  def reducer(f: BinOp[A]): A                        = xs.init.foldr(last)(f)
  def zfoldl[B: Empty](f: (B, A) => B): B            = ll.foldLeft(xs, emptyValue[B], f)
  def zfoldr[B: Empty](f: (A, B) => B): B            = ll.foldRight(xs, emptyValue[B], f)
  def zreducel(f: BinOp[A])(implicit z: Empty[A]): A = zcond(!isEmpty, reducel(f))
  def zreducer(f: BinOp[A])(implicit z: Empty[A]): A = zcond(!isEmpty, reducer(f))

  def ++(ys: View[A]): View[A] = Each.join(xs, ys).m

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs
  def force[R](implicit z: Builds[A, R]): R          = z build xs

  def iterator: scIterator[A]                = toScalaStream.iterator
  def toArray(implicit z: CTag[A]): Array[A] = to[Array]
  def toPset(implicit z: Eq[A]): Pset[A]     = to[Pset]
  def toPlist: Plist[A]                      = to[Plist]
  def toRefArray(): Array[Ref[A]]            = asRefs.force
  def toScalaStream: sciStream[A]            = to[sciStream]
  def toScalaVector: sciVector[A]            = to[sciVector]
  def toVec: Vec[A]                          = to[Vec]

  def asRefs: View[Ref[A]]   = xs map castRef
  def seq: scSeq[A]          = to[scSeq] // varargs or unapplySeq, usually
  def trav: scTraversable[A] = to[scTraversable] // scala flatMap, usually
}

class View2DOps[A](private val xss: View2D[A]) extends AnyVal {
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
