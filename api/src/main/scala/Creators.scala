package psp
package api

import Api._

trait PspCreators {
  def inView[A](mf: Suspended[A]): View[A]
  def list[A](xs: A*): Each[A]
  def set[A: Eq](xs: A*): ExSet[A]
  def vec[A](xs: A*): Direct[A]
  def view[A](xs: A*): View[A]
  def zip[A, B](xs: (A->B)*): ZipView[A, B]
}

// trait PspOptionType {
//   type Opt[X]

//   def apply[A](x: A): Opt[A]
//   def empty[A] : Opt[A]
//   def join[A](x: Opt[Opt[A]]): Opt[A]

//   abstract class Ops[A](x: Opt[A]) {
//     def isEmpty: Boolean
//     def fold[B](e: => B, f: A => B): B
//     def map[B](f: A => B): Opt[B]
//     def flatMap[B](f: A => Opt[B]): Opt[B]
//     def filter(p: A => Boolean): Opt[A]
//   }
// }
