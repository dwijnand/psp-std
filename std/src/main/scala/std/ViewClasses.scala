package psp
package std

import views._, all._

trait ViewClasses[A, R] {
  self: RView[A, R] =>

  type Map2D[B] = MapTo[MapTo[B]]
  type MapTo[X] = RView[X, R]
  type Pred     = ToBool[A]
  type This     = MapTo[A]
  type V        = MapTo[A]
  type Zipped   = Zip[A, A]

  def mpartition(mf: View[A] => Pred): View[View[A]] = {
    def next(self: View[A]): Pstream[View[A]] =
      mf(self) |> (p => Pstream[View[A]](self filter p, next(self filter !p)))

    next(self)
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
    def pairs: MapTo[A->A]            = View(zip.pairs)
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
    def column(n: Index): MapTo[B] = rows map (_ applyIndex n)
    def columns: Map2D[B]         = View(openIndices map column)

    def widths(implicit z: Show[B]): MapTo[Int]  = columns map (_ map (_.pp.length) max)
    def lines(implicit z: Show[B]): View[String] = zcond(!isEmpty, widths zip rows map (lformat(_)(_)))
  }

  class EqOps(implicit z: Eq[A]) {
    def contains(x: A): Boolean = self exists (_ === x)
    def distinct: RView[A, R]   = self.zfoldl[V]((xs, x) => if (xs contains x) xs else xs :+ x)
    def indexOf(x: A): Index    = self indexWhere (_ === x)
  }
}

/** Extractors.
 */

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
  def unapply[A, R](xs: RView[A, R]): Option[RView[A, R] -> SliceRange] = xs match {

    case View0(SlicedView(xs, r), DropRight(n)) => some(pair(xs, r dropRight n))
    case View0(SlicedView(xs, r), TakeRight(n)) => some(pair(xs, r takeRight n))
    case View0(SlicedView(xs, r), Drop(n))      => some(pair(xs, r drop n))
    case View0(SlicedView(xs, r), Take(n))      => some(pair(xs, r take n))
    case IdView(u: Each[_])                     => u.size matchIf { case x: Precise => some(xs -> x.indices) }
    case _                                      => none()
  }
}
object SizeOfView {
  def unapply[A](xs: View[A]): Some[Size] = Some(apply(xs))
  def apply[A](xs: View[A]): Size = xs match {
    case Op(EmptyView(), _)                                                      => _0
    case Op(SizeOfView(n), FlatMap(_))                                           => Size.Unknown
    case Op(SizeOfView(n), Reverse | Mapped(_))                                  => n
    case Op(SizeOfView(n), Filter(_) | Collect(_) | TakeWhile(_) | DropWhile(_)) => n.atMost
    case Op(SizeOfView(m), Drop(n))                                              => m - n
    case Op(SizeOfView(m), DropRight(n))                                         => m - n
    case Op(SizeOfView(m), Take(n))                                              => Size.min(m, n)
    case Op(SizeOfView(m), TakeRight(n))                                         => Size.min(m, n)
    case Joined(SizeOfView(v1), SizeOfView(v2))                                  => v1 + v2
    case IdView(SizeOfView(n))                                                   => n
    case IdView(xs: Each[_])                                                     => xs.size
    case IdView(_)                                                               => Size.Unknown
  }
}
object Op {
  def unapply[A, R, S](xs: RView[A, R]) = xs match {
    case View0(prev, op) => some(prev -> op)
    case View1(prev, op) => some(prev -> op)
    case View2(prev, op) => some(prev -> op)
    case _               => none()
  }
}
