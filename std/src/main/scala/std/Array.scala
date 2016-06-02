package psp
package std

import all._

trait InPlaceCommon[A] extends Any {
  def xs: Array[A]

  private def len: Int       = xs.length
  private def midIndex: Int  = len / 2 - 1
  private def lastIndex: Int = len - 1
  private def swap(i1: Int, i2: Int): Unit = {
    val tmp = xs(i1)
    xs(i1) = xs(i2)
    xs(i2) = tmp
  }
  private def randomPosInt(max: Int): Int = scala.util.Random.nextInt(max + 1)

  protected def andThis(expr: Unit): Array[A] = xs

  def map(f: ToSelf[A]): Array[A] = andThis(ll.foreachInt(0, lastIndex, i => xs.update(i, f(xs(i)))))
  def reverse(): Array[A]         = andThis(ll.foreachInt(0, midIndex, i => swap(i, lastIndex - i)))
  def shuffle(): Array[A]         = andThis(ll.foreachInt(0, lastIndex, i => swap(i, i + randomPosInt(lastIndex - i))))

  // TODO: rotate right and left
  // def >>(n: Int): Array[A]
  // def <<(n: Int): Array[A]
}


final class InPlacePrimitive[A >: Primitive <: AnyVal](val xs: Array[A]) extends AnyVal with InPlaceCommon[A] {
  def sort: Array[A] = doalso(xs)(
    (xs: Array[_]) match {
      case xs: Array[Char]    => java.util.Arrays.sort(xs)
      case xs: Array[Byte]    => java.util.Arrays.sort(xs)
      case xs: Array[Short]   => java.util.Arrays.sort(xs)
      case xs: Array[Int]     => java.util.Arrays.sort(xs)
      case xs: Array[Long]    => java.util.Arrays.sort(xs)
      case xs: Array[Float]   => java.util.Arrays.sort(xs)
      case xs: Array[Double]  => java.util.Arrays.sort(xs)
      case _                  =>
    }
  )
}

final class InPlaceReference[A <: AnyRef](val xs: Array[A]) extends AnyVal with InPlaceCommon[A] {
  private def sortRef[B >: A](cmp: Comparator[B]) = java.util.Arrays.sort[A](cast[Array[Ref[A]]](xs), cmp)

  def sortBy[B: Order](f: A => B): Array[A] = sort(Order.by[A](f))
  def sort(implicit z: Order[A]): Array[A] = (xs: Array[_]) match {
    case _: Array[AnyRef] => andThis(sortRef(z.comparator))
    case _                => abort("No custom comparator on primitive array")
  }
}
