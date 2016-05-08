// package psp
// package std

// import api._, all._

// trait StdEq0 {
//   implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] = Order.fromInt[A](_ compareTo _)
// }

// trait StdEq1 extends StdEq0 {
//   implicit def unbuildsEq[R, A](implicit b: UnbuildsAs[A, R], z: Eq[A]): Eq[R] =
//     Eq[R]((xs, ys) => (b unbuild xs) zip (b unbuild ys) corresponds z.eqv)
// }
