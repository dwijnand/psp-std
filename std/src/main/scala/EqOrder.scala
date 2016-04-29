package psp
package std

import api._, all._

trait PrimitiveInstances {
  implicit def boolOrder: Order[Bool]   = orderBy[Bool](x => if (x) 1 else 0)
  // implicit def byteOrder: Order[Byte]   = Order.fromInt(_ - _)
  implicit def charOrder: Order[Char]   = Order.fromInt[Char](_ - _)
  // implicit def shortOrder: Order[Short] = Order.fromInt[Short](_ - _)
  implicit def intOrder: Order[Int]     = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]   = Order.fromLong[Long](_ - _)
  // implicit def unitOrder: Order[Unit]   = Order.fromInt[Unit]((x, y) => 0)
}

trait EqOrderInstances0 {
  // If this is written in the obvious way, i.e.
  //
  //   implicit def comparableOrder[A <: Comparable[A]] : Order[A]
  //
  // Then it isn't found for infix operations on Comparables. We also can't call
  // Order.natural[A]() because it has that bound, despite the implicit witness.
  implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] = Order.fromInt[A](_ compareTo _)
}

trait EqOrderInstances1 extends EqOrderInstances0 {
  private def corresponds[A](xs: Foreach[A], ys: Foreach[A])(implicit z: Eq[A]): Boolean = {
    val it1 = BiIterator(xs)
    val it2 = BiIterator(ys)
    while (it1.hasNext && it2.hasNext) {
      if (!z.eqv(it1.next, it2.next))
        return false
    }
    !(it1.hasNext || it2.hasNext)
  }

  implicit def unbuildsEq[R, A](implicit b: UnbuildsAs[A, R], e: Eq[A]): Eq[R] =
    Eq[R]((xs, ys) => corresponds(b unbuild xs, b unbuild ys))
}

trait EqOrderInstances extends EqOrderInstances1 {
  // Some unfortunate rocket dentistry necessary here.
  // This doesn't work because scala comes up with "Any" due to the fbound.
  // implicit def enumOrder[A <: jEnum[A]]: Order[A] = Order.fromInt[A](_.ordinal - _.ordinal)
  //
  // This one doesn't work if it's A <:< jEnum[A], but jEnum[_] is just enough to get what we need.
  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A]          = orderBy[A](_.ordinal)

  implicit def vindexOrder: Order[Vindex]                                   = orderBy[Vindex](_.indexValue)
  implicit def preciseOrder: Order[Precise]                                 = orderBy[Precise](_.get)
  implicit def stringOrder: Order[String]                                   = Order.fromLong[String](_ compareTo _)
  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = orderBy[(A, B)](fst) | snd
  // implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = orderBy[(A, B, C)](_._1) | (_._2) | (_._3)

  // implicit def docEq: Hash[Doc]                = inheritEq
  // implicit def classWrapperEq: Hash[JavaClass] = inheritEq
  implicit def classEq: Hash[Class[_]]         = inheritEq
  // implicit def pathEq: Eq[jPath]               = shownEq[jPath](inheritShow)
  implicit def sizeEq: Hash[Size]              = inheritEq

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }
}
