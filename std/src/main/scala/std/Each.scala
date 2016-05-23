package psp
package std

import api._, all._

sealed trait Cont[A] extends Any

trait Each[+A] extends Any with Foreach[A]

trait Indexed[+A] extends Any with Each[A] {
  def apply(idx: Vdex): A
}
trait Direct[+A] extends Any with Indexed[A] {
  def size: Precise
  def head: A            = apply(_0) // depend on this
  def reverse: Direct[A] = size.getInt |> (e => Makes.fromInts(n => apply(Index(e - 1 - n)), 0, e))
}

object Cont {
  def apply[A](mf: Suspended[A]): Cont[A] = Opaque(mf)

  final case class Opaque[A](mf: Suspended[A])                extends Cont[A]
  final case class Join[A](c1: Cont[A], c2: Cont[A])          extends Cont[A]
  final case class Filter[A](c: Cont[A], p: ToBool[A])        extends Cont[A]
  final case class Mapped[A, B](c: Cont[A], g: A => B)        extends Cont[B]
  final case class FlatMap[A, B](c: Cont[A], g: A => Cont[B]) extends Cont[B]

  final class Stream[A](head: => A, tail: => Stream[A])

  implicit class ContOps[A](c: Cont[A]) {
    def join(that: Cont[A]): Cont[A]         = Join(c, that)
    def filter(p: ToBool[A]): Cont[A]        = Filter(c, p)
    def map[B](f: A => B): Cont[B]           = Mapped(c, f)
    def flatMap[B](f: A => Cont[B]): Cont[B] = FlatMap(c, f)

    def resume(f: ToUnit[A]): Unit = c match {
      case Opaque(mf)     => mf(f)
      case Join(c1, c2)   => c1 resume f; c2 resume f
      case Filter(c, p)   => c resume (x => if (p(x)) f(x))
      case Mapped(c, g)   => c resume (x => g andThen f)
      case FlatMap(c, g)  => c resume (x => g(x) resume f)
    }
  }
}

abstract class StdDirect[A](val size: Precise) extends Direct[A] {
  final def foreach(f: A => Unit): Unit = size.indices foreach (i => f(apply(i)))
}

object Each {
  def suspend[A](c: Cont[A]): Each[A] = new Suspend(c)

  class Suspend[A](c: Cont[A]) extends Each[A] {
    def foreach(f: A => Unit): Unit = c resume f
  }
  class IntIndexed[A](f: Int => A, start: Int, end: Int) extends StdDirect[A](Size(end - start)) {
    def apply(idx: Vdex): A    = f(start + idx.indexValue.toInt)
  }
  class Const[A](elem: A) extends Indexed[A] {
    def apply(idx: Vdex): A         = elem
    def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  class Continual[A](elem: => A) extends Indexed[A] {
    def apply(idx: Vdex): A         = elem
    def foreach(f: A => Unit): Unit = while (true) f(elem)
  }

  def array[A](xs: Array[A]): Each[A]                                 = intIndexed(xs.apply, 0, xs.length)
  def elems[A](xs: A*): Each[A]                                       = intIndexed[A](xs.apply, 0, xs.length)
  def jvmString(s: String): Each[Char]                                = intIndexed(s charAt _, 0, s.length)
  def pair[A, R](x: R)(implicit z: Splitter[R, A, A]): Each[A]        = intIndexed(i => cond(i == 0, x._1, x._2), 0, 2)
  def const[A](elem: A): Each[A]                                      = new Const(elem)
  def continually[A](expr: => A): Each[A]                             = new Continual(expr)
  def intIndexed[A](f: Int => A, start: Int, end: Int): IntIndexed[A] = new IntIndexed(f, start, end)

  def apply[A](mf: Suspended[A]): Each[A]              = new Suspend(Cont(mf))
  def each[A](xs: Foreach[A]): Each[A]                 = new Suspend(Cont[A](xs foreach _))
  def join[A](xs: Foreach[A], ys: Foreach[A]): Each[A] = new Suspend(Cont[A](xs foreach _) join Cont[A](ys foreach _))
  def javaMap[A, B](xs: jMap[A, B]): Each[A -> B]      = new Suspend(Cont[A -> B](xs.entrySet map (_.toPair) foreach _))
  def java[A](xs: jIterable[A]): Each[A]               = new Suspend(Cont[A](xs.iterator foreach _))

  def scalaOnce[A](xs: GTOnce[A]): Each[A] = scala(xs.toTraversable)
  def scala[A](xs: sCollection[A]): Each[A] = xs match {
    case xs: sciIndexedSeq[_] => intIndexed(xs.apply, 0, xs.length)
    case _                    => apply(xs foreach _)
  }
}

object View2D {
  type Coords = PairOf[Vdex]

  def mpartition[A](xs: View[A])(p: View[A] => ToBool[A]): View2D[A] =
    xs partition p(xs) app ((ls, rs) => inView(ls +: mpartition(rs)(p) foreach _))

  class Live[-A, +B](basis: View[A], functions: View[A => B]) extends (Coords => B) {
    def isEmpty: Bool        = basis.isEmpty || functions.isEmpty
    def apply(xy: Coords): B = xy app (rows applyIndex _ applyIndex _)
    def rows: View2D[B]      = basis map (r => functions map (_ apply r))
    def columns: View2D[B]   = rows.transpose

    def widths(implicit z: Show[B]): View[Int]   = columns map (_ map (_.pp.length) max)
    def lines(implicit z: Show[B]): View[String] = cond(isEmpty, view(), widths zip rows map (lformat(_)(_)))
  }
}
