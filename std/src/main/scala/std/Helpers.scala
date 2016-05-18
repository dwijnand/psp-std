package psp
package std

import api._, all._

/** These classes all put the expected result type up front,
  *  where it can either be inferred from an existing value or
  *  supplied directly.
  */
final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A]         = z on f }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]           = z on f }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]           = z on f }
final class AllBy[A]   { def apply[B](f: A => B)(implicit z: HashEqOrd[B]): HashEqOrd[A] = z on f }

class RemakeHelper[R](xs: R) {
  def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
}
class MakeHelper[R] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
}
