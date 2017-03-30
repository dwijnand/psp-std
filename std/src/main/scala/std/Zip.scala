package psp
package std

import all._

/** When a View presents as a sequence of pairs.
  * There may be two underlying views being zipped, or one view holding pairs.
  */
trait Zip[+A1, +A2] extends Any {
  def lefts: View[A1]       // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]      // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2] // the pairs. Moral equivalent of lefts zip rights.
}
final class ZipPairs[+A, +B](ps: => View[A -> B]) extends Zip[A, B] {
  def pairs: View[A -> B] = ps
  def lefts: View[A]      = pairs map fst
  def rights: View[B]     = pairs map snd
}
final class ZipViews[+A, +B](ls: => View[A], rs: => View[B]) extends Zip[A, B] {
  def pairs: View[A -> B] = this map (_ -> _)
  def lefts: View[A]      = ls
  def rights: View[B]     = rs
}

object Zip {
  implicit class ZipOps[A1, A2](private val zip: Zip[A1, A2]) extends AnyVal {
    import zip._

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
      filter((a, b) => f(a) === g(b))

    def corresponds(p: PredBoth): Bool         = iterator |> (it => it.forall(_ app p) && !it.hasMore)
    def drop(n: Precise): This                 = zipProducts(pairs drop n)
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
    def take(n: Precise): This                 = zipProducts(pairs take n)
    def unzip: View[A1] -> View[A2]            = lefts -> rights
    def withFilter(p: PredBoth): This          = zipProducts(suspend[Both](mf => foreach((x, y) => if (p(x, y)) mf(x -> y))))

    def force[R](implicit z: Makes[Both, R]): R = z make pairs
  }
}
