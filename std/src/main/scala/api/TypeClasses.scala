package psp
package api

/** API level type classes and interfaces other than the collections.
 */
import Api._

/** The classic type classes for encoding value equivalence and hash codes.
 */

trait Hash[-A] extends Any with Eq[A] {
  def hash(x: A): Long
}
trait Eq[-A] extends Any with MEq[Id, A] {
  // type M[+X] = X
  def eqv(x: A, y: A): Boolean
}
trait MEq[M[+X], -A] extends Any {
  // type M[+X]
  def eqv(x: M[A], y: M[A]): M[Boolean]
}

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any {
  def show(x: A): String
}

/** The original type class for providing the "empty" value of a particular type.
 *  Suitable only for types with a unique (useful) definition of empty - but that's
 *  a lot of types. You could easily recover the behavior of methods like Option.get
 *  or Seq.head by creating a default instance of Empty[A] which throws an exception.
 */
trait Empty[+A] extends Any {
  def empty: A
}
trait Unbuilds[Repr] extends Any {
  type Elem
  def unbuild(xs: Repr): Foreach[Elem]
}

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the `2^32` states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any with Eq[A] {
  def cmp(x: A, y: A): Cmp
}

trait Indexer[-R, +A] extends Any {
  def lastIndex(x: R): Long
  def elemAt(x: R, index: Long): A
}
trait Splitter[-R, +A, +B] extends Any {
  def split(x: R): A -> B
}
trait Joiner[+R, -A, -B]  extends Any {
  def join(x: A -> B): R
}
trait Cleaver[R, A, B] extends Any with Joiner[R, A, B] with Splitter[R, A, B]

object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: Joiner[R, A, B]): R          = z.join(pair(x, y))
  def unapply[R, A, B](x: R)(implicit z: Splitter[R, A, B]): Some[A -> B] = scala.Some(z split x)
}

object :: {
  def apply[R, A, B](x: A, y: B)(implicit z: Joiner[R, A, B]): R = Pair(x, y)
  def unapply[R, A, B](x: R)(implicit z1: Splitter[R, A, B], z2: Empty[R], z3: Eq[R]): Option[A -> B] =
    if (z3.eqv(x, z2.empty)) none() else some(z1 split x)
}
