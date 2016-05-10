package psp
package std

import api._, all._

object Each {
  def apply[A](mf: Suspended[A]): Each[A]                          = construct(Size.Unknown, mf)
  def array[A](xs: Array[A]): WrapArray[A]                         = new WrapArray[A](xs)
  def const[A](elem: A): Each[A]                                   = apply(mf => while (true) mf(elem))
  def construct[A](size: Size, mf: Suspended[A]): Each[A]          = new WrapSuspended(size, mf)
  def continually[A](elem: => A): Each[A]                          = new WrapContinually(elem)
  def each[A](xs: Foreach[A]): Each[A]                             = construct(xs.size, xs foreach _)
  def javaMap[A, B](xs: jMap[A, B]): Each[A -> B]                  = construct(xs.size, mf => xs.entrySet foreach (k => mf(k.toPair)))
  def java[A](xs: jIterable[A]): Each[A]                           = apply(xs.iterator foreach _)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]                   = new Joined(xs, ys)
  def jvmString(s: String): WrapString                             = new WrapString(s)
  def pair[R, A](x: R)(implicit z: Splitter[R, A, A]): WrapPair[A] = new WrapPair(z split x)
  def reversed[A](xs: Direct[A]): WrapReverse[A]                   = new WrapReverse(xs)
  def scalaMap[A, B](xs: scMap[A, B]): Each[A -> B]                = construct(xs.size, xs foreach _)
  def scala[A](xs: sCollection[A]): Each[A]                        = construct(xs.size, xs foreach _)
  def unapplySeq[A](xs: Foreach[A]): Some[scSeq[A]]                = Some(xs.seq)

  abstract class WrapEach[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    def foreach(f: A => Unit): Unit = mf(f)
  }
  abstract class WrapDirect[A, R](val size: Precise, f: Long => A) extends Direct[A] {
    def view: DirectView[A, R]      = new DirectView[A, R](this)
    def elemAt(i: Vdex): A          = f(i.indexValue)
    def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final class WrapString(xs: String)        extends WrapDirect[Char, String](xs.length, xs charAt _.toInt)
  final class WrapArray[A](xs: Array[_])    extends WrapDirect[A, Array[A]](xs.length, i => cast[A](xs(i.toInt)))
  final class WrapReverse[A](xs: Direct[A]) extends WrapDirect[A, Direct[A]](xs.size, i => xs(xs.lastIndex - i))
  final class WrapPair[A](xs: A -> A)       extends WrapDirect[A, A -> A](2, i => cond(i == 0, fst(xs), snd(xs)))

  final class WrapSuspended[A](size: Size, mf: Suspended[A]) extends WrapEach(size, mf)
  final class WrapJoin[A](xs: Each[A], ys: Each[A])          extends WrapEach[A](xs.size + ys.size, (xs foreach _) &&& (ys foreach _))
  final class WrapContinually[A](expr: => A)                 extends WrapEach[A](Infinite, f => while (true) f(expr))
}
