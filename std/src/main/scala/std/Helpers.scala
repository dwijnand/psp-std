package psp
package std

import api._, all._

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y))) }
final class ShowBy[A] { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]    = Show[A](f andThen z.show) }
final class HashBy[A] { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]    = Eq.hash[A]((x, y) => z.eqv(f(x), f(y)))(x => z hash f(x)) }
final class EqBy[A] { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]          = Eq[A]((x, y) => z.eqv(f(x), f(y))) }

/** These classes all put the expected result type up front,
  *  where it can either be inferred from an existing value or
  *  supplied directly.
  */
class RemakeHelper[R](xs: R) {
  def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
}
class MakeHelper0[R] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
}
class MakeHelper1[CC[_]] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build expr
}
class MakeHelper2[CC[_, _]] {
  def apply[K, V](expr: => Zip[K, V])(implicit z: Builds[K -> V, CC[K, V]]): CC[K, V] = z build expr.pairs
}
