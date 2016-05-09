package psp
package std
package spire

import api._, exp._

/** Classes copied from spire to avoid the dependency.
  */
trait Heyting[A] extends Any with BoundedLattice[A] {
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def imp(a: A, b: A): A
  def complement(a: A): A

  def meet(a: A, b: A): A = and(a, b)
  def join(a: A, b: A): A = or(a, b)
}
object Heyting {
  def apply[A](implicit z: Heyting[A]): Heyting[A] = z
}
object Lattice {
  def min[A](implicit z: Order[A]): Lattice[A] = new MinMaxLattice[A]
}

/**
  * A boolean algebra is a structure that defines a few basic operations, namely
  * as conjunction (&), disjunction (|), and negation (~). Both conjunction and
  * disjunction must be associative, commutative and should distribute over each
  * other. Also, both have an identity and they obey the absorption law; that
  * is `x & (y | x) == x` and `x | (x & y) == x`.
  */
trait Bool[A] extends Any with Heyting[A] {
  def xor(a: A, b: A): A  = or(and(a, complement(b)), and(complement(a), b))
  def imp(a: A, b: A): A  = or(complement(a), b)
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A  = complement(or(a, b))
  def nxor(a: A, b: A): A = and(or(a, complement(b)), or(complement(a), b))
  // def dual: Bool[A] = new DualBool(this)
}

trait Lattice[A]        extends Any with JoinSemilattice[A] with MeetSemilattice[A]
trait BoundedLattice[A] extends Any with Lattice[A] with BoundedMeetSemilattice[A] with BoundedJoinSemilattice[A]

trait JoinSemilattice[A] extends Any {
  def join(lhs: A, rhs: A): A
}
trait MeetSemilattice[A] extends Any {
  def meet(lhs: A, rhs: A): A
}
trait BoundedJoinSemilattice[A] extends Any with JoinSemilattice[A] {
  def zero: A
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)
}
trait BoundedMeetSemilattice[A] extends Any with MeetSemilattice[A] {
  def one: A
  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)
}
class MinMaxLattice[A : Order] extends Lattice[A] {
  def meet(lhs: A, rhs: A): A = min(lhs, rhs)
  def join(lhs: A, rhs: A): A = max(lhs, rhs)
}
