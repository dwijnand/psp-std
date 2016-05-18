package psp
package std

import api._, all._

/** Motley objects for which a file of residence is not obvious.
  */
class FormatFun(val fmt: String) extends (Any => String) with ShowSelf {
  def apply(x: Any): String = stringFormat(fmt, x)
  def to_s                  = fmt
}
final class JvmName(val raw: String) extends ShowSelf {
  def segments: Vec[String] = raw splitChar '.'
  def short: String         = segments.last
  def to_s: String          = raw
}
object JvmName {
  import scala.reflect.NameTransformer.decode

  def asJava(clazz: jClass): JvmName  = new JvmName(clazz.getName)
  def asScala(clazz: jClass): JvmName = new JvmName(clazz.getName.mapSplit('.')(decode))
}
final class Utf8(val bytes: Array[Byte]) extends AnyVal with ShowSelf {
  def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
  def to_s: String       = new String(chars)
}

trait StdEq0 {
  implicit def combineHashAndOrder[A](implicit r1: Order[A], r2: Hash[A]): HashEqOrd[A] = r1 hashWith r2.hash

  implicit def sizeHashEq: Hash[Size] =
    Relation.hash(Size.equiv, _.##)

  implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] =
    Relation.order(((x: A, y: A) => x compareTo y) andThen (x => longCmp(x)))
}
trait StdEq1 extends StdEq0 {
  implicit def eqViewsAs[R, A](implicit b: ViewsAs[A, R], z: Eq[A]): Eq[R] =
    Relation.equiv((xs, ys) => intoView(xs) zip intoView(ys) corresponds z.eqv)

  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A] =
    orderBy[A](_.ordinal)

  implicit def optionEq[A: Eq]: Eq[Opt[A]] = Relation equiv {
    case (None, None)       => true
    case (Some(x), Some(y)) => x === y
    case _                  => false
  }

  implicit def longHashEqOrd: HashEqOrd[Long]       = Relation.Longs
  implicit def boolHashEqOrd: HashEqOrd[Bool]       = Relation allBy (x => if (x) 1 else 0)
  implicit def charHashEqOrd: HashEqOrd[Char]       = Relation allBy (x => x: Long)
  implicit def intHashEqOrd: HashEqOrd[Int]         = Relation allBy (x => x: Long)
  implicit def vindexHashEqOrd: HashEqOrd[Vdex]     = Relation allBy (_.indexValue)
  implicit def preciseHashEqOrd: HashEqOrd[Precise] = Relation allBy (_.getLong)
  implicit def stringHashEqOrd: HashEqOrd[String]   = Relation.Lexical

  implicit def classEq: Hash[Class[_]] = Relation.Inherited

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Relation equiv {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }
}
trait StdEq extends StdEq1 {
  implicit def intervalHashEqOrd: Hash[Interval] = Relation.Inherited

  implicit def product2HashEqOrder[A: HashEqOrd, B: HashEqOrd]: HashEqOrd[A -> B] =
    Relation.all[A -> B](
      orderBy[A -> B](fst) | snd cmp,
      xy => fst(xy).hash + snd(xy).hash
    )
}
object StdShow extends ShowInstances
object Unsafe {
  implicit def promoteIndex(x: Long): Index = Index(x)
  implicit def inheritedShow[A]: Show[A]    = inheritShow
}

object +: {
  def unapply[A](xs: View[A]): Option[A -> View[A]] =
    cond(xs.isEmpty, none, some(xs.head -> xs.tail))
}
object :+ {
  def unapply[A](xs: View[A]): Option[View[A] -> A] =
    cond(xs.isEmpty, none, some(xs.init -> xs.last))
}

sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A]            extends <:<[A, A] { def apply(x: A): A = x }

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction[_, _] => f isDefinedAt x
    case _                              => true
  }
  def apply(x: T): R    = f(x)
  override def toString = to_s
}
