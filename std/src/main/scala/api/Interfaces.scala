package psp
package api

import Api._

/** Foreach is the common parent of View and Each.
  *
  *  A View always wraps an indeterminate number of Views
  *  and a single Each which provides the original basis.
  *  An Each may be composed from smaller Eaches but is
  *  otherwise atomic. The size of an Each is known, the
  *  size of a View may not be.
  */
trait Foreach[+A] extends Any {
  def size: Size
  def foreach(f: A => Unit): Unit
}
trait View[+A]    extends Any with Foreach[A]
trait Each[+A]    extends Any with Foreach[A]
trait Indexed[+A] extends Any with Each[A] { def elemAt(i: Vdex): A }
trait Direct[+A]  extends Any with Indexed[A] { def size: Precise }

trait InSet[-A] extends Any {
  def apply(x: A): Bool
}
trait ExSet[A] extends Any with Each[A] with InSet[A] {
  def basis: Each[A]
  def equiv: Hash[A]
  def size: Precise
}

/** When a Show type class is more trouble than it's worth.
  *  Not overriding toString here to leave open the possibility of
  *  using a synthetic toString, e.g. of case classes.
  */
trait ShowDirect extends Any { def to_s: String }
trait ShowSelf extends Any with ShowDirect { override def toString = to_s }
