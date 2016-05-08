package psp
package std

import api._, all._

/** When a View is split into two disjoint views.
 *  Notably, that's span, partition, and splitAt.
 */
final case class Split[A](left: View[A], right: View[A]) {
  type V = View[A]

  def intersperse: V                   = zipped flatMap (vec(_, _))
  def mapLeft(f: ToSelf[V]): Split[A]  = Split(f(left), right)
  def mapRight(f: ToSelf[V]): Split[A] = Split(left, f(right))
  def onLeft[B](f: V => B): B          = f(left)
  def onRight[B](f: V => B): B         = f(right)
  def rejoin: V                        = left ++ right
  def zipped: Zip[A, A]                = zipViews(left, right)
}

/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait Zip[+A1, +A2] extends Any {
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}
object Zip {
  /** A Zip has similar operations to a View, but with the benefit of
   *  being aware each element has a left and a right.
   */
  implicit class ZipOps[A1, A2](val x: Zip[A1, A2]) extends AnyVal {
    import x.{ lefts, rights, pairs }

    type MapTo[R] = (A1, A2) => R
    type Both     = A1 -> A2
    type This     = Zip[A1, A2]
    type LPred    = ToBool[A1]
    type RPred    = ToBool[A2]
    type BothPred = MapTo[Bool]
    type OptPair  = Option[Both]

    def zfoldl[B](f: (B, A1, A2) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
    def foldl[B](zero: B)(f: (B, A1, A2) => B): B = {
      var res = zero
      foreach ((x, y) => res = f(res, x, y))
      res
    }
    def find(p: BothPred): OptPair = {
      foreach((x, y) => if (p(x, y)) return Some(x -> y))
      None
    }
    def foreach(f: (A1, A2) => Unit): Unit = (lefts, rights) match {
      case (xs: Direct[A1], ys: Direct[A2]) => (xs.size min ys.size).indices foreach (i => f(xs(i), ys(i)))
      case (xs: Direct[A1], ys)             => (ys take xs.size).zipIndex map ((y, i) => f(xs(i), y))
      case (xs, ys: Direct[A2])             => (xs take ys.size).zipIndex map ((x, i) => f(x, ys(i)))
      case _                                => lefts.iterator |> (it => rights foreach (y => if (it.hasNext) f(it.next, y) else return))
    }
    def drop(n: Precise): This                    = zipViews(lefts drop n, rights drop n)
    def dropWhileFst(p: LPred): This              = pairs dropWhile (xy => p(fst(xy))) zipped
    def dropWhileSnd(p: RPred): This              = pairs dropWhile (xy => p(snd(xy))) zipped
    def filter(p: BothPred): This                 = withFilter(p)
    def filterLeft(p: LPred): This                = withFilter((x, _) => p(x))
    def filterRight(p: RPred): This               = withFilter((_, y) => p(y))
    def findLeft(p: LPred): OptPair               = find((x, _) => p(x))
    def findRight(p: RPred): OptPair              = find((_, y) => p(y))
    def flatMap[B](f: MapTo[Foreach[B]]): View[B] = inView(mf => foreach((x, y) => f(x, y) foreach mf))
    def mapLeft[B1](f: A1 => B1): Zip[B1, A2]     = zipViews(lefts map f, rights)
    def mapRight[B2](f: A2 => B2): Zip[A1, B2]    = zipViews(lefts, rights map f)
    def map[B](f: MapTo[B]): View[B]              = inView(mf => foreach((x, y) => mf(f(x, y))))
    def take(n: Precise): This                    = zipViews(lefts take n, rights take n)
    def takeWhileFst(p: LPred): This              = pairs takeWhile (xy => p(fst(xy))) zipped
    def takeWhileSnd(p: RPred): This              = pairs takeWhile (xy => p(snd(xy))) zipped
    def withFilter(p: BothPred): This             = inView[Both](mf => foreach((x, y) => if (p(x, y)) mf(x -> y))).zipped

    def to[R](implicit z: Builds[Both, R]): R    = force[R]
    def force[R](implicit z: Builds[Both, R]): R = z build pairs
  }

  /** Zip0 means we're using a Splitter to interpret a collection holding the joined type.
   *  Zip1 means there's a single view containing pairs, a View[A1->A2].
   *  Zip2 means there are two separate views, a View[A1] and a View[A2].
   *  This is plus or minus only a performance-related implementation detail.
   */
  final case class ZipSplit[A, A1, A2](xs: View[A])(implicit z: Splitter[A, A1, A2]) extends Zip[A1, A2] {
    def lefts  = xs map (x => fst(z split x))
    def rights = xs map (x => snd(z split x))
    def pairs  = xs map z.split
  }
  final case class ZipPairs[A1, A2](pairs: View[A1 -> A2]) extends Zip[A1, A2] {
    def lefts  = pairs map fst
    def rights = pairs map snd
  }
  final case class ZipViews[A1, A2](lefts: View[A1], rights: View[A2]) extends Zip[A1, A2] {
    def pairs: View[A1 -> A2] = inView(mf => this.foreach((x, y) => mf(x -> y)))
  }
  final case class ZipWith[A1, A2](xs: View[A1], f: A1 => A2) extends Zip[A1, A2] {
    def lefts  = xs
    def rights = xs map f
    def pairs  = lefts zip rights pairs
  }
  final case class CrossViews[A1, A2](xs: View[A1], ys: View[A2]) extends Zip[A1, A2] {
    def lefts  = pairs map fst
    def rights = pairs map snd
    def pairs  = for (x <- xs ; y <- ys) yield x -> y
  }
}
