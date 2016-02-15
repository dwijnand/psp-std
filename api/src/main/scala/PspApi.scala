package psp
package api

import scala.{ Tuple2, collection => sc }
import psp.ext.ExternalLibs

// Importing from this is necessary to use these aliases within the api package,
// where they aren't otherwise visible because there's no api package object.
private[api] final object Api extends PspApi

trait PspApi extends ExternalLibs {
  // Caveat: ?=> associates to the left instead of the right.
  type ->[+A, +B]      = scala.Product2[A, B]        // A less overconstrained product type.
  type ?=>[-A, +B]     = scala.PartialFunction[A, B] // Less clumsy syntax for the all-important partial function.
  type GTOnce[+A]      = sc.GenTraversableOnce[A]    // This is the beautifully named type at the top of scala collections
  type Id[X]           = X                           // The identity type constructor.
  type Ref[+A]         = AnyRef with A               // Promotes an A <: Any into an A <: AnyRef.
  type sCollection[+A] = sc.GenTraversable[A]        // named analogously to jCollection.

  // Aliases and constant values for common function types.
  type BinOp[A]           = (A, A) => A // binary operation
  type OrderRelation[-A]  = (A, A) => Cmp
  type Relation[-A]       = (A, A) => Boolean
  type Suspended[+A]      = ToUnit[ToUnit[A]]
  type ToBool[-A]         = A => Boolean
  type ToInt[-A]          = A => Int
  type ToSelf[A]          = A => A
  type ToString[-A]       = A => String
  type ToUnit[-A]         = A => Unit

  final val ConstantTrue  = (x: scala.Any) => true
  final val ConstantFalse = (x: scala.Any) => false
  final val ->            = Pair

  // A few methods it is convenient to expose at this level.
  def ?[A](implicit value: A): A                         = value
  def abort(msg: String): Nothing                        = runtimeException(msg)
  def doto[A](x: A)(f: A => Unit): A                     = sideEffect(x, f(x))
  def emptyValue[A](implicit z: Empty[A]): A             = z.empty
  def identity[A](x: A): A                               = x
  def none[A](): Option[A]                               = scala.None
  def pair[@fspec A, @fspec B](x: A, y: B): Tuple2[A, B] = new Tuple2(x, y)
  def show[A](implicit z: Show[A]): Show[A]              = z
  def sideEffect[A](result: A, exprs: Any*): A           = result
  def some[A](x: A): Option[A]                           = scala.Some(x)

  def stringFormat(s: String, args: Any*): String = java.lang.String.format(s, args map unwrapArg: _*)

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: scala.math.ScalaNumber => x.underlying
    case x: AnyRef                 => x
  }

  def newArray[A: CTag](length: Int): Array[A] = new Array[A](length)
  def copyArray[A: CTag](src: Array[A]): Array[A] = {
    val target = newArray[A](src.length)
    sideEffect(target, arraycopy(src, 0, target, 0, src.length))
  }

  def arraycopy[A](src: Array[A], srcPos: Int, dest: Array[A], destPos: Int, len: Int): Unit =
    java.lang.System.arraycopy(src, srcPos, dest, destPos, len)
}

sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }
