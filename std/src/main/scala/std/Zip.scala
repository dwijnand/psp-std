package psp
package std

import api._, all._

/** When a View is split into two disjoint views.
  * Notably, that's span, partition, and splitAt.
  */
final case class Split[A](leftView: View[A], rightView: View[A]) extends TwoHeteroViews[A, A] {
  type V = View[A]

  def collate: V                           = pairs flatMap (_.each)
  def mapBoth[B](f: V => B): PairOf[B]     = views map2 f
  def mapEach(f: ToSelf[V]): Split[A]      = Split(f(leftView), f(rightView))
  def mapLeft(f: ToSelf[V]): Split[A]      = Split(f(leftView), rightView)
  def mapRight(f: ToSelf[V]): Split[A]     = Split(leftView, f(rightView))
  def appLeft[B](f: V => B): B             = f(leftView)
  def appRight[B](f: V => B): B            = f(rightView)
  def join: V                              = app(_ ++ _)
  def sort(implicit z: Order[A]): Split[A] = mapEach(_.sort)
}

trait TwoHeteroViews[+L, +R] extends Any {
  private[this] type LV = View[L]
  private[this] type RV = View[R]

  def leftView: LV
  def rightView: RV

  def app[B](f: (LV, RV) => B): B = views app f

  /** M[A -> B] vs. M[A] -> M[B] in views vs. pairs.
   */
  def views: LV -> RV     = leftView -> rightView
  def pairs: View[L -> R] = zip.pairs
  def zip: Zip[L, R]      = this app zipViews
  def cross: Zip[L, R]    = this app zipCross
}

/** When a View presents as a sequence of pairs.
  *  There may be two underlying views being zipped, or one view holding pairs.
  */
trait Zip[+A1, +A2] extends Any {
  def lefts: View[A1]       // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]      // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2] // the pairs. Moral equivalent of lefts zip rights.
  def size: Size
}
trait ZipFromViews[+A1, +A2] extends Any with Zip[A1, A2] {
  def pairs: View[A1 -> A2] = inView(mf => this.foreach((x, y) => mf(x -> y)))
  def size                  = pairs.size
}
trait ZipFromPairs[+A1, +A2] extends Any with Zip[A1, A2] {
  def lefts: View[A1]  = pairs map fst
  def rights: View[A2] = pairs map snd
  def size             = lefts.size min rights.size
}

object Zip {

  /** A Zip has similar operations to a View, but with the benefit of
    *  being aware each element has a left and a right.
    */
  implicit class ZipOps[A1, A2](private val x: Zip[A1, A2]) extends AnyVal {
    import x.{ lefts, rights, pairs }

    type MapTo[R] = (A1, A2) => R
    type Both     = A1 -> A2
    type This     = Zip[A1, A2]
    type LPred    = ToBool[A1]
    type RPred    = ToBool[A2]
    type PredBoth = MapTo[Bool]
    type OptBoth  = Option[Both]

    def foldl[B](zero: B)(f: (B, A1, A2) => B): B =
      ll.foldLeft[A1 -> A2, B](pairs, zero, (res, x) => f(res, fst(x), snd(x)))

    def find(p: PredBoth): OptBoth =
      foldl(none())((res, x, y) => cond(p(x, y), return some(x -> y), res))

    def foreach(f: (A1, A2) => Unit): Unit = (lefts, rights) match {
      case (xs: Direct[A1], ys) => xs.size.indices zip ys mapLeft xs.apply
      case (xs, ys: Direct[A2]) => xs zip ys.size.indices mapRight ys.apply
      case _                    => lefts.iterator |> (it => rights foreach (y => cond(it.hasNext, f(it.next, y), return)))
    }

    private def checkForall(p: PredBoth, ignoreStubs: Boolean): Bool = {
      val it = new ZipIterator(lefts.iterator, rights.iterator)
      if (it exists (x => !(x app p))) return false
      ignoreStubs || !it.hasMore
    }

    def drop(n: Precise): This        = zipSplit(pairs drop n)
    def dropWhileFst(p: LPred): This  = zipSplit(pairs dropWhile (_ appLeft p))
    def dropWhileSnd(p: RPred): This  = zipSplit(pairs dropWhile (_ appRight p))
    def filter(p: PredBoth): This     = withFilter(p)
    def filterLeft(p: LPred): This    = withFilter((x, _) => p(x))
    def filterRight(p: RPred): This   = withFilter((_, y) => p(y))
    def take(n: Precise): This        = zipSplit(pairs take n)
    def takeWhileFst(p: LPred): This  = zipSplit(pairs takeWhile (_ appLeft p))
    def takeWhileSnd(p: RPred): This  = zipSplit(pairs takeWhile (_ appRight p))
    def withFilter(p: PredBoth): This = zipSplit(inView[Both](mf => foreach((x, y) => if (p(x, y)) mf(x -> y))))

    def corresponds(p: PredBoth): Bool            = checkForall(p, ignoreStubs = false)
    def exists(p: PredBoth): Bool                 = !forall(!p)
    def flatMap[B](f: MapTo[Foreach[B]]): View[B] = inView(mf => foreach((x, y) => f(x, y) foreach mf))
    def forall(p: PredBoth): Bool                 = checkForall(p, ignoreStubs = true)
    def mapLeft[B1](f: A1 => B1): Zip[B1, A2]     = zipViews(lefts map f, rights)
    def mapRight[B2](f: A2 => B2): Zip[A1, B2]    = zipViews(lefts, rights map f)
    def map[B](f: MapTo[B]): View[B]              = inView(mf => foreach((x, y) => mf(f(x, y))))
    def unzip: View[A1] -> View[A2]               = lefts -> rights

    def force[R](implicit z: Builds[Both, R]): R = z build pairs
  }

  final case class ZipIterator[A1, A2](ls: scIterator[A1], rs: scIterator[A2]) extends scIterator[A1 -> A2] {
    def hasNext: Bool    = ls.hasNext && rs.hasNext
    def hasMore: Bool    = ls.hasNext || rs.hasNext
    def next(): A1 -> A2 = ls.next -> rs.next
  }

  final case class ZipSplit[A, A1, A2](xs: View[A])(implicit z: Splitter[A, A1, A2]) extends ZipFromPairs[A1, A2] {
    def pairs = xs map z.split
  }
  final case class ZipPairs[A1, A2](pairs: View[A1 -> A2]) extends ZipFromPairs[A1, A2]

  final case class ZipCross[A1, A2](lv: View[A1], rv: View[A2]) extends ZipFromPairs[A1, A2] {
    def pairs = for (x <- lv; y <- rv) yield x -> y
  }
  final case class ZipViews[A1, A2](lefts: View[A1], rights: View[A2]) extends ZipFromViews[A1, A2]
  final case class ZipMap[A1, A2](lefts: View[A1], f: A1 => A2) extends ZipFromViews[A1, A2] {
    def rights = lefts map f
  }
}
