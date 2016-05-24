package psp
package std

import all._

/** When a View is split into two disjoint views.
  * Notably, that's span, partition, and splitAt.
  */
final case class Split[A](leftView: View[A], rightView: View[A]) extends SplitView[View[A]] {
  type V = View[A]

  type This = Split[A]
  def remake(l: V, r: V): This = Split(l, r)

  def pairs: View[PairOf[A]] = zip.pairs
  def collate: V             = pairs flatMap (_.each)
  def join: V                = app(_ ++ _)
  def cross: Zip[A, A]       = app(zipCross)
  def zip: Zip[A, A]         = app(zipViews)
}

trait SplitView[V] {
  type This <: SplitView[V]

  def leftView: V
  def rightView: V
  def remake(l: V, r: V): This

  def views: PairOf[V]                 = leftView -> rightView
  def appLeft[B](f: V => B): B         = f(leftView)
  def appRight[B](f: V => B): B        = f(rightView)
  def app[B](f: (V, V) => B): B        = views app f
  def mapBoth[B](f: V => B): PairOf[B] = views map2 f
  def mapEach(f: ToSelf[V]): This      = remake(f(leftView), f(rightView))
  def mapLeft(f: ToSelf[V]): This      = remake(f(leftView), rightView)
  def mapRight(f: ToSelf[V]): This     = remake(leftView, f(rightView))
}
object SplitView {
  def unapply[R](x: SplitView[R]) = some(x.leftView -> x.rightView)
}

/** When a View presents as a sequence of pairs.
  *  There may be two underlying views being zipped, or one view holding pairs.
  */
trait Zip[+A1, +A2] extends Any {
  def lefts: View[A1]       // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]      // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2] // the pairs. Moral equivalent of lefts zip rights.
}
trait ZipFromViews[+A1, +A2] extends Any with Zip[A1, A2] {
  def pairs: View[A1 -> A2] = this map (_ -> _)
}
trait ZipFromPairs[+A1, +A2] extends Any with Zip[A1, A2] {
  def lefts: View[A1]  = pairs map fst
  def rights: View[A2] = pairs map snd
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
      ll.foldLeft[Both, B](pairs, zero, (res, x) => f(res, fst(x), snd(x)))

    def find(p: PredBoth): OptBoth =
      foldl(none())((res, x, y) => cond(p(x, y), return some(x -> y), res))

    def foreach(f: MapTo[Unit]): Unit =
      lefts.iterator |> (it => rights foreach (y => cond(it.hasNext, f(it.next, y), return )))

    def eqBy[B: Eq](f: A1 => B, g: A2 => B): Zip[A1, A2] =
      x filter ((a, b) => f(a) === g(b))

    def corresponds(p: PredBoth): Bool         = iterator |> (it => it.forall(_ app p) && !it.hasMore)
    def drop(n: Precise): This                 = zipSplit(pairs drop n)
    def exists(p: PredBoth): Bool              = !forall(!p)
    def filter(p: PredBoth): This              = withFilter(p)
    def filterLeft(p: LPred): This             = withFilter((x, _) => p(x))
    def filterRight(p: RPred): This            = withFilter((_, y) => p(y))
    def first[B: Empty](pf: Both ?=> B): B     = pairs zfirst pf
    def forall(p: PredBoth): Bool              = iterator forall (_ app p)
    def iterator: ZipIterator[A1, A2]          = new ZipIterator(lefts.iterator, rights.iterator)
    def mapLeft[B1](f: A1 => B1): Zip[B1, A2]  = zipViews(lefts map f, rights)
    def mapRight[B2](f: A2 => B2): Zip[A1, B2] = zipViews(lefts, rights map f)
    def map[B](f: MapTo[B]): View[B]           = suspend[B](mf => foreach((x, y) => mf(f(x, y)))).m
    def take(n: Precise): This                 = zipSplit(pairs take n)
    def unzip: View[A1] -> View[A2]            = lefts -> rights
    def withFilter(p: PredBoth): This          = zipSplit(suspend[Both](mf => foreach((x, y) => if (p(x, y)) mf(x -> y))))

    def force[R](implicit z: Makes[Both, R]): R = z make pairs
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

trait StdSplitZip {
  def splitter[R, A, B](f: R => (A -> B)): Splitter[R, A, B] = new Splitter[R, A, B] { def split(x: R): A -> B = f(x) }
  def joiner[R, A, B](f: (A, B) => R): Joiner[R, A, B]       = new Joiner[R, A, B] { def join(x: A -> B): R    = f(x._1, x._2) }

  def cleaver[R, A, B](f: (A, B) => R, l: R => A, r: R => B): Cleaver[R, A, B] = new Cleaver[R, A, B] {
    def split(x: R): A -> B = l(x) -> r(x)
    def join(x: A -> B): R  = x app f
  }

  def zipCross[A, B](l: View[A], r: View[B]): Zip[A, B]                        = new Zip.ZipCross(l, r)
  def zipSplit[R, A, B](xs: View[R])(implicit z: Splitter[R, A, B]): Zip[A, B] = new Zip.ZipSplit(xs)
  def zipPairs[A, B](xs: View[A -> B]): Zip[A, B]                              = new Zip.ZipPairs(xs)
  def zipViews[A, B](l: View[A], r: View[B]): Zip[A, B]                        = new Zip.ZipViews(l, r)
  def zipMap[A, B](l: View[A], f: A => B): Zip[A, B]                           = new Zip.ZipMap(l, f)
}
