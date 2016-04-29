package psp
package std
package ops


import api._, all._
import spire.math.Sorting

final class ArrayClassTagOps[A: CTag](val xs: Array[A]) {
  def mergeSort(implicit z: Order[A]): Array[A] =
    sideEffect(xs, Sorting.mergeSort[A](xs)(z, ?))

  def ++(that: Array[A]): Array[A] = {
    val arr = newArray[A](xs.length + that.length)
    arraycopy(xs, 0, arr, 0, xs.length)
    arraycopy(that, 0, arr, xs.length, that.length)
    arr
  }
}

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal {
  // def size: Precise                          = Size(xs.length)
  // def apply(idx: Index): A                   = xs(idx.getInt)
  // def updated(idx: Index, value: A): xs.type = sideEffect(xs, xs(idx.getInt) = value)
  def inPlace: InPlace[A]                    = new InPlace[A](xs)
}

final class InPlace[A](val xs: Array[A]) extends AnyVal {
  private def lastIndex = xs.length - 1
  private def sortRef(cmp: Comparator[A]) = java.util.Arrays.sort[A](xs.castTo[Array[Ref[A]]], cmp)
  private def isReference = (xs: Any) match {
    case _: Array[AnyRef] => true
    case _                => false
  }
  private def midpoint: Int  = xs.length / 2
  private def swap(i1: Int, i2: Int): Unit = {
    val tmp = xs(i1)
    xs(i1) = xs(i2)
    xs(i2) = tmp
  }
  private def sortInPlace[A](xs: Array[A]): Unit = xs match {
    case xs: Array[Byte]   => java.util.Arrays.sort(xs)
    case xs: Array[Char]   => java.util.Arrays.sort(xs)
    case xs: Array[Short]  => java.util.Arrays.sort(xs)
    case xs: Array[Int]    => java.util.Arrays.sort(xs)
    case xs: Array[Long]   => java.util.Arrays.sort(xs)
    case xs: Array[Double] => java.util.Arrays.sort(xs)
    case xs: Array[Float]  => java.util.Arrays.sort(xs)
    case xs: Array[AnyRef] => java.util.Arrays.sort[AnyRef](xs, Eq.refComparator)
    case _                 =>
  }

  // def insertionSort(implicit z: Order[A]): Array[A] = sideEffect(xs, Sorting.insertionSort[A](xs)(z, null))
  // def quickSort(implicit z: Order[A]): Array[A]     = sideEffect(xs, Sorting.quickSort[A](xs)(z, null))
  def sort(implicit z: Order[A]): Array[A]          = sideEffect(xs, if (isReference) sortRef(Order.comparator) else sortInPlace(xs))
  // def sortBy[B: Order](f: A => B): Array[A]         = sort(orderBy[A](f))

  def map(f: ToSelf[A]): Array[A] = sideEffect(xs, 0 to lastIndex foreach (i => xs(i) = f(xs(i))))
  def reverse(): Array[A]         = sideEffect(xs, 0 until midpoint foreach (i => swap(i, lastIndex - i)))
  def shuffle(): Array[A]         = sideEffect(xs, 0 until lastIndex foreach (i => swap(i, i + randomPosInt(lastIndex - i))))

  // TODO: rotate right and left
  // def >>(n: Int): Array[A]
  // def <<(n: Int): Array[A]
}
