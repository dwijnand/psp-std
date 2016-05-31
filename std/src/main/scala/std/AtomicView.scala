package psp
package std

import all._, View._

trait View[+A] extends Any with Foreach[A] {
  def size: Size = sizeOf(this)
}

final case class IdView[A, R](underlying: Foreach[A]) extends RView[A, R]

sealed trait RView[A, R] extends View[A] {
  type MapTo[X] = RView[X, R]
  type This     = MapTo[A]

  def xs: RView[A, R] = this

  def collect[B](pf: A ?=> B): MapTo[B]     = View2(this, Collect(pf))
  def drop(n: Precise): This                = View0(this, Drop(n))
  def dropRight(n: Precise): This           = View0(this, DropRight(n))
  def dropWhile(p: ToBool[A]): This         = View1(this, DropWhile(p))
  def flatMap[B](f: A => View[B]): MapTo[B] = View2(this, FlatMap(f))
  def foldl[B](zero: B)(f: (B, A) => B): B  = foldView(this)(zero)(f)
  def foreach(f: ToUnit[A]): Unit           = runView(this)(f)
  def join(that: View[A]): View[A]          = Joined(this, that)
  def map[B](f: A => B): MapTo[B]           = View2(this, Mapped(f))
  def reverseView: This                     = View0(this, Reverse)
  def take(n: Precise): This                = View0(this, Take(n))
  def takeRight(n: Precise): This           = View0(this, TakeRight(n))
  def takeWhile(p: ToBool[A]): This         = View1(this, TakeWhile(p))
  def withFilter(p: ToBool[A]): This        = View1(this, Filter(p))

  def build(implicit z: Makes[A, R]): R = xs.force[R]
}

object View {
  type RView2D[A, R] = RView[A, RView[A, R]]

  import Size.Zero

  def apply[A, R](xs: Foreach[A]): RView[A, R] = IdView(xs)

  object Optimize {
    private def finish[A, R](xs: RView[A, R]): Some[RView[A, R]] = xs match {
      case Optimize(ys) => Some(ys)
      case _            => Some(xs)
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

  def foldView[A, B](xs: View[A])(zero: B)(f: (B, A) => B): B = {
    var res = zero
    runView(xs)(x => res = f(res, x))
    res
  }

  def sizeOf[A](xs: View[A]): Size = xs match {
    case IdView(xs: Direct[_])   => xs.size
    case IdView(_: Indexed[_])   => Infinite
    case Joined(xs, ys)          => sizeOf(xs) + sizeOf(ys)
    case View0(xs, Reverse)      => sizeOf(xs)
    case View2(xs, Mapped(_))    => sizeOf(xs)
    case View2(xs, FlatMap(_))   => cond(sizeOf(xs).isZero, Zero, Size.Unknown)
    case View1(xs, Filter(_))    => sizeOf(xs).atMost
    case View2(xs, Collect(_))   => sizeOf(xs).atMost
    case View1(xs, TakeWhile(_)) => sizeOf(xs).atMost
    case View1(xs, DropWhile(_)) => sizeOf(xs).atMost
    case View0(xs, DropRight(n)) => Size.min(n, sizeOf(xs))
    case View0(xs, TakeRight(n)) => Size.min(n, sizeOf(xs))
    case View0(xs, Drop(n))      => Size.min(n, sizeOf(xs))
    case View0(xs, Take(n))      => Size.min(n, sizeOf(xs))
    case _                       => Size.Unknown
  }
  def runView[C](xs: View[C])(f: ToUnit[C]): Unit = xs match {
    case Optimize(xs)                       => runView(xs)(f)
    case IdView(xs)                         => xs foreach f
    case Joined(xs, ys)                     => runView(xs)(f); runView(ys)(f)
    case View0(View0(xs, Reverse), Reverse) => runView(xs)(f)
    case View0(xs, Reverse)                 => runView(xs.toVec.reverse)(f)
    case View2(xs, Mapped(g))               => runView(xs)(g andThen f)
    case View2(xs, FlatMap(g))              => runView(xs)(x => g(x) foreach f)
    case View1(xs, Filter(p))               => runView(xs)(x => if (p(x)) f(x))
    case View2(xs, Collect(pf))             => runView(xs)(x => if (pf isDefinedAt x) f(pf(x)))
    case FlatSlice(xs, range)               => ll.foreachSlice(xs, range, f)
    case View1(xs, TakeWhile(p))            => ll.foreachTakeWhile(xs, f, p)
    case View1(xs, DropWhile(p))            => ll.foreachDropWhile(xs, f, p)
    case View0(xs, DropRight(n))            => ll.foreachDropRight(xs, f, n)
    case View0(xs, TakeRight(n))            => ll.foreachTakeRight(xs, f, n)
    case View0(xs, Drop(n))                 => ll.foreachSlice(xs, n.getLong.andUp map Index, f)
    case View0(xs, Take(n))                 => ll.foreachSlice(xs, n.indices, f)
  }

  object FlatSlice {
    def unapply[A, R](xs: RView[A, R]): Option[RView[A, R] -> VdexRange] = xs match {

      case View0(prev, Reverse)      => none()
      case View0(prev, DropRight(n)) => unapply(prev) map (_ mapRight (_ dropRight n))
      case View0(prev, TakeRight(n)) => unapply(prev) map (_ mapRight (_ takeRight n))
      case View0(prev, Drop(n))      => unapply(prev) map (_ mapRight (_ drop n))
      case View0(prev, Take(n))      => unapply(prev) map (_ mapRight (_ take n))
      case _                         => xs.size matchIf { case x: Precise => some(xs -> x.indices) }
    }
  }

  class Live[A, B](basis: View[A], functions: View[A => B]) extends (Coords => B) {
    def isEmpty: Bool            = basis.isEmpty || functions.isEmpty
    def apply(xy: Coords): B     = xy app (rows applyIndex _ applyIndex _)
    def rows: View[View[B]]      = basis map (r => functions map (_ apply r))
    def column(n: Vdex): View[B] = rows map (_ applyIndex n)
    def columns: View[View[B]]   = View(openIndices map column)

    def widths(implicit z: Show[B]): View[Int]   = columns map (_ map (_.pp.length) max)
    def lines(implicit z: Show[B]): View[String] = cond(isEmpty, view(), widths zip rows map (lformat(_)(_)))
  }

  // XXX Figure out how to maintain laziness here.
  def unapplySeq[A](xs: View[A]): Some[scSeq[A]] = Some(xs.seq)
}
object EmptyView {
  def unapply[A](xs: View[A]): Bool = xs.isEmpty
}
