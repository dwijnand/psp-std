package psp
package std

import api._, all._ // , StdEq._

final case class Split[A](left: View[A], right: View[A]) extends api.SplitView[A] {
  def onRight[B](f: View[A] => B): B            = f(right)
  // def onLeft[B](f: View[A] => B): B             = f(left)
  // def mapLeft(f: View[A] => View[A]): Split[A]  = copy(left = f(left))
  // def mapRight(f: View[A] => View[A]): Split[A] = copy(right = f(right))
  def rejoin: View[A]                           = left ++ right
  def zipped: Zip.Impl[A, A]                    = Zip.zip2(left, right)
  def intersperse: View[A]                      = zipped flatMap (vec(_, _))
  // def pair: (View[A], View[A])                  = left -> right
}

object Zip {
  def impl[A, B](xs: ZipView[A, B]): Impl[A, B]                                  = new Impl[A, B](xs)
  def zip0[AB, A, B](xs: View[AB])(implicit z: Splitter[AB, A, B]): Impl[A, B] = impl(new ZipView0(xs))
  def zip1[A, B](xs: View[A -> B]): Impl[A, B]                                   = impl(new ZipView1(xs))
  def zip2[A, B](l: View[A], r: View[B]): Impl[A, B]                             = impl(new ZipView2(l, r))
  // def zip2[A, B](lr: (View[A], View[B])): Impl[A, B]                             = zip2(lr._1, lr._2)
  // def zipf[A, B](l: View[A])(f: Index => A => B): Impl[A, B]                     = impl(new ZipViewF(l, f))
  // def zipc[A, B](l: View[A], r: View[B]): Impl[A, B]                             = impl(new CrossView(l, r))

  /** A ZipView has similar operations to a View, but with the benefit of
   *  being aware each element has a left and a right.
   */
  final case class Impl[A1, A2](x: ZipView[A1, A2]) extends AnyVal with ZipView[A1, A2] {
    type Both       = A1 -> A2
    type This       = ZipView[A1, A2]
    type Predicate2 = (A1, A2) => Bool

    // def relativeSize      = x.relativeSize
    def lefts: View[A1]   = x.lefts
    def rights: View[A2]  = x.rights
    def pairs: View[Both] = x.pairs

    def zfoldl[B](f: (B, A1, A2) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
    def foldl[B](zero: B)(f: (B, A1, A2) => B): B = {
      var res = zero
      foreach ((x, y) => res = f(res, x, y))
      res
    }
    def find(p: (A1, A2) => Boolean): Option[Both] = {
      foreach((x, y) => if (p(x, y)) return Some(x -> y))
      None
    }
    def foreach(f: (A1, A2) => Unit): Unit = (lefts, rights) match {
      case (xs: Direct[A1], ys: Direct[A2]) => (xs.size min ys.size).indices foreach (i => f(xs(i), ys(i)))
      case (xs: Direct[A1], ys)             => (ys take xs.size).indexed map (i => y => f(xs(i), y))
      case (xs, ys: Direct[A2])             => (xs take ys.size).indexed map (i => x => f(x, ys(i)))
      case _                                => lefts.iterator |> (it => rights foreach (y => if (it.hasNext) f(it.next, y) else return))
    }

    // def drop(n: Precise): This                                      = zip2(lefts drop n, rights drop n)
    // def dropWhileFst(p: ToBool[A1]): This                           = pairs dropWhile (xy => p(fst(xy))) zipped
    def dropWhileSnd(p: ToBool[A2]): This                           = pairs dropWhile (xy => p(snd(xy))) zipped
    def filter(q: Predicate2): This                                 = withFilter(q)
    def filterLeft(q: ToBool[A1]): This                             = withFilter((x, _) => q(x))
    // def filterRight(q: ToBool[A2]): This                            = withFilter((_, y) => q(y))
    def findLeft(p: ToBool[A1]): Option[Both]                       = find((x, _) => p(x))
    // def findRight(p: ToBool[A2]): Option[Both]                      = find((_, y) => p(y))
    def flatMap[B](f: (A1, A2) => Foreach[B]): View[B]              = inView(mf => foreach((x, y) => f(x, y) foreach mf))
    // def mapBoth[B1, B2](g: (A1, A2) => (B1 -> B2)): ZipView[B1, B2] = zip1(this map g)
    def mapLeft[B1](g: A1 => B1): ZipView[B1, A2]                   = zip2(lefts map g, rights)
    // def mapRight[B2](g: A2 => B2): ZipView[A1, B2]                  = zip2(lefts, rights map g)
    def map[B](f: (A1, A2) => B): View[B]                           = inView(mf => foreach((x, y) => mf(f(x, y))))
    // def take(n: Precise): This                                      = zip2(lefts take n, rights take n)
    def takeWhileFst(p: ToBool[A1]): This                           = pairs takeWhile (xy => p(fst(xy))) zipped
    // def takeWhileSnd(p: ToBool[A2]): This                           = pairs takeWhile (xy => p(snd(xy))) zipped
    def withFilter(p: Predicate2): This                             = inView[Both](mf => foreach((x, y) => if (p(x, y)) mf(x -> y))).zipped

    // def toJavaMap: jMap[A1, A2]                           = Java.Map[A1, A2](pairs.seq: _*)
    // def toMap(implicit z: Eq[A1]): ExMap[A1, A2]          = to[ExMap[A1, A2]]
    // def toScalaMap: sciMap[A1, A2]                        = to[sciMap[A1, A2]]
    // def toSortedMap(implicit z: Order[A1]): ExMap[A1, A2] = toMap // TODO
    // def to[R](implicit z: Builds[Both, R]): R             = force[R]

    def force[R](implicit z: Builds[Both, R]): R = z build pairs
  }

  /** ZipView0 means we're using a Splitter to interpret a collection holding the joined type.
   *  ZipView1 means there's a single view containing pairs, a View[A1->A2].
   *  ZipView2 means there are two separate views, a View[A1] and a View[A2].
   *  This is plus or minus only a performance-related implementation detail.
   */
  final case class ZipView0[A, A1, A2](xs: View[A])(implicit z: Splitter[A, A1, A2]) extends ZipView[A1, A2] {
    // def relativeSize = Some(0)
    def lefts        = xs map (x => fst(z split x))
    def rights       = xs map (x => snd(z split x))
    def pairs        = xs map z.split
  }
  final case class ZipView1[A1, A2](pairs: View[A1 -> A2]) extends ZipView[A1, A2] {
    // def relativeSize = Some(0)
    def lefts  = pairs map fst
    def rights = pairs map snd
  }
  final case class ZipView2[A1, A2](lefts: View[A1], rights: View[A2]) extends ZipView[A1, A2] {
    // def relativeSize          = (lefts.size, rights.size) match {
    //   case (Finite(l), Finite(r)) => Some(r - l)
    //   case _                      => None
    // }
    def pairs: View[A1 -> A2] = inView(mf => this.foreach((x, y) => mf(x -> y)))
  }
  // final case class ZipViewF[A1, A2](xs: View[A1], f: Index => A1 => A2) extends ZipView[A1, A2] {
  //   def relativeSize = Some(0)
  //   def lefts        = xs
  //   def rights       = xs.indexed map f
  //   def pairs        = lefts zip rights pairs
  // }
  // final class CrossView[A1, A2](xs: View[A1], ys: View[A2]) extends ZipView[A1, A2] {
  //   def relativeSize = Some(0)
  //   def lefts        = pairs map fst
  //   def rights       = pairs map snd
  //   def pairs        = for (x <- xs ; y <- ys) yield x -> y
  // }
}
