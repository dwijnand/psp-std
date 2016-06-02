package psp
package std

import all._

trait ViewClasses[A, R] {
  self: RView[A, R] =>

  type Map2D[B] = MapTo[MapTo[B]]
  type MapTo[X] = RView[X, R]
  type Pred     = ToBool[A]
  type This     = MapTo[A]
  type V        = MapTo[A]
  type Zipped   = Zip[A, A]
  type S2D      = Pstream[Pstream[A]]

  def mpartition(mf: View[A] => Pred): Map2D[A] = {
    def next(self: View[A]): Pstream[MapTo[A]] = mf(self) |> (p => Pstream(self filter p as, next(self filter !p)))
    next(self).as
  }

  /** When a View is split into two disjoint views.
    * Notably, that's span, partition, and splitAt.
    */
  case class Split(leftView: V, rightView: V) {
    def appLeft[B](f: V => B): B      = f(leftView)
    def appRight[B](f: V => B): B     = f(rightView)
    def app[B](f: (V, V) => B): B     = views app f
    def collate: V                    = View(pairs flatMap (_.each))
    def cross: Zipped                 = app(zipCross)
    def join: V                       = app(_ ++ _)
    def mapBoth[B](f: V => B): B->B   = views map2 f
    def mapEach(f: ToSelf[V]): Split  = Split(f(leftView), f(rightView))
    def mapLeft(f: ToSelf[V]): Split  = Split(f(leftView), rightView)
    def mapRight(f: ToSelf[V]): Split = Split(leftView, f(rightView))
    def pairs: RView[A->A, R]         = View(zip.pairs)
    def views: V->V                   = leftView -> rightView
    def zip: Zipped                   = app(zipViews(_, _))
  }

  /** A derived view based on a sequence of functions.
   *  Each element in `self` gives rise to a row of elements,
   *  after applying each function `functions` to the element.
   */
  class Live[B](functions: View[A => B]) extends (Coords => B) {
    def isEmpty: Bool             = self.isEmpty || functions.isEmpty
    def apply(xy: Coords): B      = xy app (rows applyIndex _ applyIndex _)
    def rows: MapTo[View[B]]      = self map (r => functions map (_ apply r))
    def column(n: Vdex): MapTo[B] = rows map (_ applyIndex n)
    def columns: Map2D[B]         = View(openIndices map column)

    def widths(implicit z: Show[B]): MapTo[Int]  = columns map (_ map (_.pp.length) max)
    def lines(implicit z: Show[B]): View[String] = zcond(!isEmpty, widths zip rows map (lformat(_)(_)))
  }

  class EqOps(implicit z: Eq[A]) {
    def contains(x: A): Boolean = self exists (_ === x)
    def distinct: RView[A, R]   = self.zfoldl[V]((xs, x) => if (xs contains x) xs else xs :+ x)
    def indexOf(x: A): Vdex     = self indexWhere (_ === x)
  }
}

/** Extractors.
 */

import View._
import Size.Zero

object SplitView {
  def unapply[A, R](x: SplitView[A, R]) = Some(x.leftView -> x.rightView)
}
object EmptyView {
  def unapply[A](xs: View[A]): Bool = xs.isEmpty
}
object HeadTailView {
  def unapply[A, R](xs: RView[A, R]) = xs take 1 match {
    case View(x) => some(x -> xs.tail)
    case _       => none()
  }
}
object SlicedView {
  def unapply[A, R](xs: RView[A, R]): Option[RView[A, R] -> VdexRange] = xs match {
    case View0(prev, Reverse)      => none()
    case View0(prev, DropRight(n)) => unapply(prev) map (_ mapRight (_ dropRight n))
    case View0(prev, TakeRight(n)) => unapply(prev) map (_ mapRight (_ takeRight n))
    case View0(prev, Drop(n))      => unapply(prev) map (_ mapRight (_ drop n))
    case View0(prev, Take(n))      => unapply(prev) map (_ mapRight (_ take n))
    case _                         => xs.size matchIf { case x: Precise => some(xs -> x.indices) }
  }
}
object OptimizeView {
  private def finish[A, R](xs: RView[A, R]): Some[RView[A, R]] = xs match {
    case OptimizeView(ys) => Some(ys)
    case _                => Some(xs)
  }
  def unapply[A, R](xs: RView[A, R]): Opt[RView[A, R]] = xs match {
    case View0(View0(xs, Reverse), Reverse)             => finish(xs)
    case View2(View2(xs, Mapped(f)), Mapped(g))         => finish(xs map (f andThen g))
    case View2(View2(xs, Mapped(f)), FlatMap(g))        => finish(xs flatMap (f andThen g))
    case View2(View2(xs, FlatMap(f)), Mapped(g))        => finish(xs flatMap (x => f(x) map g))
    case View2(View2(xs, FlatMap(f)), FlatMap(g))       => finish(xs flatMap (f andThen g))
    case View1(View1(xs, Filter(p)), Filter(q))         => finish(xs withFilter p && q)
    case View2(View2(xs, Collect(pf)), Mapped(f))       => finish(xs collect (pf andThen f))
    case View0(View0(xs, Drop(n1)), Drop(n2))           => finish(xs drop n1 + n2)
    case View0(View0(xs, DropRight(n1)), DropRight(n2)) => finish(xs dropRight n1 + n2)
    case View0(View0(xs, Take(n1)), Take(n2))           => finish(xs take min(n1, n2))
    case View0(View0(xs, TakeRight(n1)), TakeRight(n2)) => finish(xs takeRight min(n1, n2))
    case View1(View1(xs, TakeWhile(p)), TakeWhile(q))   => finish(xs takeWhile p && q)
    case View1(View1(xs, DropWhile(p)), DropWhile(q))   => finish(xs dropWhile p || q)
    case View0(xs, Drop(Zero))                          => finish(xs)
    case View0(xs, DropRight(Zero))                     => finish(xs)
    case View0(_, Take(Zero))                           => finish(emptyValue)
    case View0(_, TakeRight(Zero))                      => finish(emptyValue)
    case _                                              => none()
  }
}
object SizeOfView {
  def unapply[A](xs: View[A]): Some[Size] = Some(apply(xs))
  def apply[A](xs: View[A]): Size = xs match {
    case IdView(xs: View[_])                    => apply(xs)
    case IdView(xs: Indexed[_])                 => xs.size
    case Joined(SizeOfView(v1), SizeOfView(v2)) => v1 + v2
    case View0(SizeOfView(n), Reverse)          => n
    case View2(SizeOfView(n), Mapped(_))        => n
    case View2(SizeOfView(n), FlatMap(_))       => cond(n.isZero, n, Size.Unknown)
    case View1(SizeOfView(n), Filter(_))        => n.atMost
    case View2(SizeOfView(n), Collect(_))       => n.atMost
    case View1(SizeOfView(n), TakeWhile(_))     => n.atMost
    case View1(SizeOfView(n), DropWhile(_))     => n.atMost
    case View0(SizeOfView(m), DropRight(n))     => Size.min(m, n)
    case View0(SizeOfView(m), TakeRight(n))     => Size.min(m, n)
    case View0(SizeOfView(m), Drop(n))          => Size.min(m, n)
    case View0(SizeOfView(m), Take(n))          => Size.min(m, n)
    case _                                      => Size.Unknown
  }
}
