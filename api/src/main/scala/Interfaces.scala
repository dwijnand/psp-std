package psp
package api

import Api._

/** Name-based extractor methods. These interfaces aren't necessary
 *  for it (thus "name-based") but provide helpful structure when used.
 */
trait IsEmpty extends Any              { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty { def get: A           }

sealed trait Vindex extends Any with Opt[Long] { def indexValue: Long }
trait Index             extends Any with Vindex { type This <: Index }
trait Nth               extends Any with Vindex { type This <: Nth }

// sealed trait MaybeView extends Any                { def isView: Boolean      }
// trait IsView           extends Any with MaybeView { final def isView = true  }
// trait NotView          extends Any with MaybeView { final def isView = false }

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

trait Each[@fspec +A]    extends Any with Foreach[A] /*with NotView*/
trait Indexed[@fspec +A] extends Any with Each[A]                 { def elemAt(i: Vindex): A }
trait Direct[@fspec +A]  extends Any with Indexed[A]              { def size: Precise        }

trait ExSet[A]     extends Any with Each[A] { def apply(x: A): Boolean    }
trait ExMap[K, +V] extends Any              { def lookup: FiniteDom[K, V] }

trait View[@fspec +A] extends Any with Foreach[A] /*with IsView*/ {
  /** Contiguous operations share the property that the result is always
   *  a (possibly empty) uninterrupted subsequence of the elements of the
   *  target collection.
   */
  def drop(n: Precise): View[A]
  def dropRight(n: Precise): View[A]
  def dropWhile(p: ToBool[A]): View[A]
  def take(n: Precise): View[A]
  def takeRight(n: Precise): View[A]
  def takeWhile(p: ToBool[A]): View[A]

  def collect[B](pf: A ?=> B): View[B]
  def map[B](f: A => B): View[B]
  def flatMap[B](f: A => Foreach[B]): View[B]
  def withFilter(p: ToBool[A]): View[A]
  def span(p: ToBool[A]): SplitView[A]
  def partition(p: ToBool[A]): SplitView[A]
}

/** When the operation has an `A` in negative position.
 */
trait IView[A] extends Any with View[A] {
  def join(that: IView[A]): IView[A]
}

/** When a View is split into two disjoint views.
 *  Notably, that's span, partition, and splitAt.
 */
trait SplitView[@fspec +A] extends Any {
  def left: View[A]   // the elements in the left-hand M.
  def right: View[A]  // the elements in the right-hand M.
  def rejoin: View[A] // Moral equivalent of left ++ right.
}

/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait ZipView[@fspec +A1, @fspec +A2] extends Any {
  // def relativeSize: Option[Long]
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}

/** When a Show type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }
trait ShowSelf extends Any with ShowDirect { override def toString = to_s }
