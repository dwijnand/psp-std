package psp
package std

import api._, all._

object Direct extends Constructions[Direct] {
  def construct[A](size: Size, mf: Suspended[A]): Vec[A] = Vec.newBuilder[A] build Each(mf)
  def array[A](xs: Array[A]): Direct[A]                  = new WrapArray[A](xs)
  def reversed[A](xs: Direct[A]): Reversed[A]            = new Reversed(xs)
  def string(s: String): Direct[Char]                    = new WrapString(s)

  trait Common[+A] extends Any with Direct[A] {
    @inline final def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final case class WrapString(xs: String) extends AnyVal with Common[Char] {
    def size              = Size(xs.length)
    def elemAt(i: Vindex) = xs charAt i.getInt
  }
  final case class WrapArray[A](val xs: Array[_]) extends AnyVal with Common[A] {
    def size                 = Size(xs.length)
    def elemAt(i: Vindex): A = xs(i.getInt).castTo[A]
  }
  final class Reversed[A](val xs: Direct[A]) extends AnyVal with Common[A] {
    def size              = xs.size
    def elemAt(i: Vindex) = xs elemAt xs.lastIndex - i.get
  }
}

object Each extends Constructions[Each] {
  def apply[A](mf: Suspended[A]): Each[A]                 = new Impl[A](Size.Unknown, mf)
  def construct[A](size: Size, mf: Suspended[A]): Each[A] = new Impl[A](size, mf)
  def continually[A](elem: => A): Continually[A]          = new Continually(elem)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]          = new Joined(xs, ys)

  final class Impl[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final class Joined[A](xs: Each[A], ys: Each[A]) extends Each[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = sideEffect(xs foreach f, ys foreach f)
  }
  final class Continually[A](expr: => A) extends Each[A] {
    def size = Infinite
    @inline def foreach(f: A => Unit): Unit = while (true) f(expr)
  }

  def unapplySeq[A](xs: Foreach[A]): Some[scSeq[A]] = Some(xs.seq)
}
