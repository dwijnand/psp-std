package psp
package std

import api._, all._

/** These classes all put the expected result type up front,
  *  where it can either be inferred from an existing value or
  *  supplied directly.
  */

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y))) }
final class ShowBy[A] { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]    = Show[A](f andThen z.show) }
final class HashBy[A] { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]    = Eq.hash[A]((x, y) => f(x) === f(y))(f andThen z.hash) }
final class EqBy[A] { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]          = Eq[A]((x, y) => f(x) === f(y)) }

final class HashEqOrdBy[A] {
  def apply[B](f: A => B)(implicit heq: Hash[B], ord: Order[B]): HashEqOrd[A] =
    HashEqOrd(ord.eqv _ on f, ord.cmp _ on f, f andThen heq.hash)
}

class RemakeHelper[R](xs: R) {
  def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
}
class MakeHelper[R] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
}
