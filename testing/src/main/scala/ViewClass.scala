package psp
package tests

import psp.std._, all._, api._, StdShow._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

object ViewClass {
  type Op    = ToSelf[ViewClass]
  type Trans = ToSelf[Op]
}

/** Methods for comparing against scala views.
 */
trait ViewClass extends ShowSelf {
  type This <: ViewClass

  def name: String
  def collect(pf: Int ?=> Int): This
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def dropWhile(p: ToBool[Int]): This
  def filter(f: ToBool[Int]): This
  def filterNot(f: ToBool[Int]): This
  def flatMap(f: Int => Foreach[Int]): This
  def foreach(f: Int => Unit): Unit
  def map(f: Int => Int): This
  def slice(range: VindexRange): This
  def take(n: Precise): This
  def takeRight(n: Precise): This
  def takeWhile(p: ToBool[Int]): This
  def withFilter(p: ToBool[Int]): This
}

/** Don't lose the toInt calls or we land in an infinite
 *  loop of some kind.
 */
final case class ScalaViewClass(name: String, xs: scIterable[Int]) extends ViewClass {
  type This = ScalaViewClass
  private implicit def liftResult(xs: scIterable[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)        = xs collect pf
  def drop(n: Precise)                = xs drop n.toInt
  def dropRight(n: Precise)           = xs dropRight n.toInt
  def dropWhile(p: ToBool[Int])       = xs dropWhile p
  def filter(p: ToBool[Int])          = xs filter p
  def filterNot(p: ToBool[Int])       = xs filterNot p
  def flatMap(f: Int => Foreach[Int]) = xs flatMap (x => f(x).trav)
  def foreach(f: Int => Unit)         = xs foreach f
  def map(f: Int => Int)              = xs map f
  def slice(range: VindexRange)       = xs slice (range.startInt, range.exclusiveEnd)
  def take(n: Precise)                = xs take n.toInt
  def takeRight(n: Precise)           = xs takeRight n.toInt
  def takeWhile(p: ToBool[Int])       = xs takeWhile p
  def withFilter(p: ToBool[Int])      = xs filter p
  def to_s: String                    = "[ " + (xs mkString ", ") + " ]"
}

final case class PspViewClass(name: String, xs: View[Int]) extends ViewClass {
  type This = PspViewClass
  private implicit def liftResult(xs: View[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)        = xs collect pf
  def drop(n: Precise)                = xs drop n
  def dropRight(n: Precise)           = xs dropRight n
  def dropWhile(p: ToBool[Int])       = xs dropWhile p
  def filter(p: ToBool[Int])          = this withFilter p
  def filterNot(p: ToBool[Int])       = this withFilter !p
  def flatMap(f: Int => Foreach[Int]) = xs flatMap f
  def foreach(f: Int => Unit)         = xs foreach f
  def map(f: Int => Int)              = xs map f
  def slice(range: VindexRange)       = xs drop range.startInt take range.size
  def take(n: Precise)                = xs take n
  def takeRight(n: Precise)           = xs takeRight n
  def takeWhile(p: ToBool[Int])       = xs takeWhile p
  def withFilter(p: ToBool[Int])      = xs withFilter p
  def to_s: String                    = "[ " + (xs mk_s ", ") + " ]"
}
