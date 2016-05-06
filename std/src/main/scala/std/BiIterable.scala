package psp
package std

import api._, all._

/** Classes which implement both the java and scala interfaces.
 */
trait BiIterable[+A] extends jIterable[A @uV] with scIterable[A] {
  def iterator(): BiIterator[A]
}
trait BiIterator[+A] extends jIterator[A @uV] with scIterator[A] {
  // def remove(): Unit = unsupportedOperationException("not supported")
  def hasNext: Boolean
  def next(): A
}

object BiIterator {
  // def empty[A] : BiIterator[A] = Empty

  def apply[A](xs: Foreach[A]): BiIterator[A]              = direct(Direct each xs)
  // def array[A](xs: Array[A]): BiIterator[A]                = direct(Direct array xs)
  def direct[A](xs: Direct[A]): DirectIterator[A]          = new DirectIterator(xs)
  def reverse[A](xs: Direct[A]): ReverseIterator[A]        = new ReverseIterator(xs)
  // def enumeration[A](enum: jEnumeration[A]): BiIterator[A] = new EnumerationIterator(enum)

  final class EnumerationIterator[A](enum: jEnumeration[A]) extends BiIterator[A] {
    def hasNext = enum.hasMoreElements
    def next()  = enum.nextElement()
  }
  // private object Empty extends BiIterator[Nothing] {
  //   def hasNext         = false
  //   def next(): Nothing = illegalArgumentException("next on empty iterator")
  // }
  final class DirectIterator[A](xs: Direct[A]) extends BiIterator[A] {
    private[this] var index: Index = Index(0)
    def hasNext   = xs containsIndex index
    def next(): A = sideEffect(xs(index), index += 1)
  }
  final class ReverseIterator[A](xs: Direct[A]) extends BiIterator[A] {
    private[this] var index: Index = xs.lastIndex
    def hasNext   = xs containsIndex index
    def next(): A = sideEffect(xs(index), index -= 1)
  }
}

object BiIterable {
  private class BiIterableImpl[A](f: => BiIterator[A]) extends BiIterable[A] { def iterator() = f }
  private class Impl[A](hasNextFn: => Boolean, nextFn: => A) extends BiIterator[A] {
    def hasNext   = hasNextFn
    def next(): A = nextFn
  }
  // private class DirectBased[A](xs: Direct[A])    extends BiIterableImpl(BiIterator direct xs)
  private class JavaBased[A](xs: jIterable[A])   extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))
  // private class ScalaBased[A](xs: scIterable[A]) extends BiIterableImpl(xs.iterator |> (it => new Impl(it.hasNext, it.next)))

  // def apply[A](xs: Foreach[A]): BiIterable[A]    = apply[A](Direct each xs)
  // def apply[A](xs: Direct[A]): BiIterable[A]     = new DirectBased(xs)
  // def apply[A](xs: Array[A]): BiIterable[A]      = apply[A](Direct array xs)
  def apply[A](xs: jIterable[A]): BiIterable[A]  = new JavaBased(xs)
  // def apply[A](xs: scIterable[A]): BiIterable[A] = new ScalaBased(xs)
  // def apply[A](s: String): BiIterable[Char]      = apply(Direct string s)
}
