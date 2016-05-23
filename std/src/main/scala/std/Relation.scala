package psp
package std

import all._

object Relation {
  val Lexical: HashEqOrder[String] = all(Hash.Inherited, Eq.Inherited, Order.Inherited[String])
  val Inherited: HashEq[Any]       = hasheq(Hash.Inherited, Eq.Inherited)
  val Reference: HashEq[AnyRef]    = hasheq(Hash.Reference, Eq.Reference)
  val Longs: HashEqOrder[Long]     = all(Hash(x => x), Eq(_ == _), Order(_ < _))

  def hasheq[A](implicit hz: Hash[A], ez: Eq[A]): HashEq[A]                 = new HashEqImpl[A](hz, ez)
  def all[A](implicit hz: Hash[A], ez: Eq[A], oz: Order[A]): HashEqOrder[A] = new HashEqOrderImpl[A](hz, ez, oz)
  def shown[A](implicit z: Show[A]): HashEqOrder[A]                         = allBy[A](z.show)

  def inheritedBy[A]: InheritedBy[A] = new InheritedBy[A]
  def hasheqBy[A]: HashEqBy[A]       = new HashEqBy[A]
  def allBy[A]: HashEqOrderBy[A]     = new HashEqOrderBy[A]

  class HashEqImpl[A](hz: Hash[A], ez: Eq[A]) extends Hash[A] with Eq[A] {
    def hash(x: A): Long      = hz.hash(x)
    def eqv(x: A, y: A): Bool = ez.eqv(x, y)
  }
  class HashEqOrderImpl[A](hz: Hash[A], ez: Eq[A], oz: Order[A]) extends Hash[A] with Eq[A] with Order[A] {
    def hash(x: A): Long       = hz.hash(x)
    def eqv(x: A, y: A): Bool  = ez.eqv(x, y)
    def less(x: A, y: A): Bool = oz.less(x, y)
  }

  final class InheritedBy[A] {
    def apply[B](f: A => B): HashEq[A] = hasheq[A](Hash.Inherited on f, Eq.Inherited on f)
  }
  final class HashEqBy[A] {
    def apply[B](f: A => B)(implicit hz: Hash[B], ez: Eq[B]): HashEq[A] =
      hasheq[A](hz on f, ez on f)
  }
  final class HashEqOrderBy[A] {
    def apply[B](f: A => B)(implicit hz: Hash[B], ez: Eq[B], oz: Order[B]): HashEqOrder[A] =
      all[A](hz on f, ez on f, oz on f)
  }
}

trait StdRelation0 {
  implicit def sizeHashEq: HashEq[Size] = Relation.hasheq(Hash.Inherited, Eq(Size.equiv))

  implicit def walksHash[A, R](implicit b: Walks[A, R], hz: Hash[A]): Hash[R]    = viewHash[A] on b.walk
  implicit def walksEq[A, R](implicit b: Walks[A, R], ez: Eq[A]): Eq[R]          = viewEq[A] on b.walk
  implicit def walksOrder[A, R](implicit b: Walks[A, R], oz: Order[A]): Order[R] = viewOrder[A] on b.walk
}
trait StdRelation1 extends StdRelation0 {
  implicit def pairEq[A: Eq, B: Eq]: Eq[A->B]             = Eq((x, y) => fst(x) === fst(y) && snd(x) === snd(y))
  implicit def pairOrder[A: Order, B: Order]: Order[A->B] = Order((x, y) => fst(x) < fst(y) && snd(x) < snd(y))
  implicit def pairHash[A: Hash, B: Hash]: Hash[A->B]     = Hash(x => fst(x).hash + snd(x).hash)

  implicit def optionHash[A: Hash]: Hash[Opt[A]] = Hash(_.fold(0L)(_.hash))
  implicit def optionOrder[A: Order]: Order[Opt[A]] = Order {
    case (Some(x), Some(y)) => x < y
    case (None, Some(_))    => true
    case _                  => false
  }
  implicit def optionEq[A: Eq]: Eq[Opt[A]] = Eq {
    case (Some(x), Some(y)) => x === y
    case (x, y)             => x.isEmpty && y.isEmpty
  }
  implicit def tryEq[A: Eq](implicit z: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }

  implicit def longHashEqOrder: HashEqOrder[Long]       = Relation.Longs
  implicit def boolHashEqOrder: HashEqOrder[Bool]       = Relation allBy (x => if (x) 1L else 0L)
  implicit def charHashEqOrder: HashEqOrder[Char]       = Relation allBy (x => x: Long)
  implicit def intHashEqOrder: HashEqOrder[Int]         = Relation allBy (x => x: Long)
  implicit def vindexHashEqOrder: HashEqOrder[Vdex]     = Relation allBy (_.indexValue)
  implicit def preciseHashEqOrder: HashEqOrder[Precise] = Relation allBy (_.getLong)
  implicit def stringHashEqOrder: HashEqOrder[String]   = Relation.Lexical
  implicit def classHashEqOrder: Hash[jClass]           = Relation.Inherited

  implicit def viewHash[A: Hash]: Hash[View[A]] = Hash(_.map(_.hash).foldl(0L)(_ + _))
  implicit def viewEq[A: Eq]: Eq[View[A]]       = Eq((xs, ys) => xs zip ys corresponds (_ === _))
  implicit def viewOrder[A: Order]: Order[View[A]] = Order { (xs, ys) =>
    def loop(x: A, y: A): Opt[Bool] = (
      if (x < y) some(true)
      else if (x < y) some(false)
      else none()
    )
    xs zip ys map loop find (_.isDefined) match {
      case Some(Some(r)) => r
      case _             => false
    }
  }

}
trait StdRelation extends StdRelation1 {
  implicit def intervalHashEqOrder: HashEqOrder[Interval] =
    Relation allBy (x => x.startLong -> x.size.preciseOrMaxLong)
}
