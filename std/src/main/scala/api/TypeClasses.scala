package psp
package api

/** API level type classes and interfaces other than the collections.
  */
import Api._

/** Type classes for "inside" a context.
  *  The more familiar type classes extends these with M[X] = X.
  *
  *  TODO: Investigate utility of further generalization of the
  *  ground types which appear here: Bool, Long, Vdex, Cmp, String
  */
trait MEq[M[+X], -A] extends Any {
  def eqv(x: M[A], y: M[A]): M[Bool]
}
trait MHash[M[+X], -A] extends Any with MEq[M, A] {
  def hash(x: M[A]): M[Long]
}
trait MOrder[M[+X], -A] extends Any with MEq[M, A] {
  def cmp(x: M[A], y: M[A]): M[Cmp]
}
trait MShow[M[+X], -A] extends Any {
  def show(x: M[A]): M[String]
}
trait MIndexer[M[+X], -R, +A] extends Any {
  def elemAt(x: M[R], index: M[Vdex]): M[A]
}
trait MEmpty[M[+X], +A] extends Any {
  def empty: M[A]
}
trait MSplitter[M[+X], -R, +A, +B] extends Any {
  // Consider: what is the significance of M[A] -> M[B], not M[A -> B] ?
  def split(x: M[R]): M[A] -> M[B]
}
trait MJoiner[M[+X], +R, -A, -B] extends Any {
  def join(x: M[A] -> M[B]): M[R]
}
trait MCleaver[M[+X], R, A, B] extends Any with MJoiner[M, R, A, B] with MSplitter[M, R, A, B]

/** Scala makes certain things impossible with type parameters and a
 *  nearly disjoint set of ambitions impossible with type members.
 *  We are left with building them in redundantly so we always have
 *  what we need.
 */
trait ASplitter[-R] extends Any {
  type Left
  type Right
  type Pair >: R
}
trait AJoiner[+R] extends Any {
  type Left
  type Right
  type Pair <: R
}
trait ACleaver[R] extends Any with AJoiner[R] with ASplitter[R] {
  type Pair = R
}

/** The classic type classes for encoding equality, inequality,
  *  and display, and less classic ones for split/join, indexed access,
  *  emptiness.
  */
trait Eq[-A] extends Any with MEq[Id, A] {
  def eqv(x: A, y: A): Bool
}
trait Hash[-A] extends Any with MHash[Id, A] with Eq[A] {
  def hash(x: A): Long
}
trait Order[-A] extends Any with MOrder[Id, A] with Eq[A] {
  def cmp(x: A, y: A): Cmp
}
trait HashEqOrd[-A] extends Any with Hash[A] with Order[A]

trait Show[-A] extends Any with MShow[Id, A] {
  def show(x: A): String
}
trait Indexer[-R, +A] extends Any with MIndexer[Id, R, A] {
  def elemAt(x: R, index: Vdex): A
}
trait Empty[+A] extends Any with MEmpty[Id, A] {
  def empty: A
}
trait Splitter[-R, +A, +B] extends Any with MSplitter[Id, R, A, B] with ASplitter[R] {
  type Left  <: A
  type Right <: B
  def split(x: R): A -> B
}
trait Joiner[+R, -A, -B] extends Any with MJoiner[Id, R, A, B] with AJoiner[R] {
  type Left  >: A
  type Right >: B
  def join(x: A -> B): R
}
trait Cleaver[R, A, B] extends Any with MCleaver[Id, R, A, B] with Joiner[R, A, B] with Splitter[R, A, B] with ACleaver[R] {
  type Left  = A
  type Right = B
}
