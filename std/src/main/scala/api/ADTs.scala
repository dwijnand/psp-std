package psp
package api

/** Sealed ADTs embedded in the API bedrock.
 */
import Api._

/** The Size hierarchy is:
 *                     Size
 *                  /        \
 *               Atomic     Bounded
 *              /      \
 *          Infinite  Precise
 *
 *  Precise implies the exact size is known. Infinite means it's infinite.
 *  Bounded is a size lower bound and a (possibly infinite) atomic upper bound.
 *  Size forms a partial order, with some liberties taken at present.
 *  Operations on sizes which are ill-defined will result in "Unknown", which
 *  encodes no available size information: Bounded(Zero, Infinite).
 *
 *  Invariants:
 *  - Precise is non-negative
 */

sealed trait Size extends Any
sealed trait Atomic extends Any with Size
final case object Infinite extends Atomic
final class Precise private[api] (val get: Long) extends AnyVal with Atomic {
  def *(n: Long): Precise = Size(get * n)
  def +(n: Long): Precise = Size(get + n)
  def -(n: Long): Precise = Size(get - n)
  def getLong: Long       = get.toLong
  override def toString   = s"$get"
}
final case class Bounded private[api] (lo: Precise, hi: Atomic) extends Size

object Finite extends (Long => Precise) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get < 0 }

  def apply(n: Long): Precise        = new Precise(cond(n < 0, 0, n))
  def unapply(n: Precise): Extractor = new Extractor(n.getLong)

  object Range {
    def apply(lo: Long, hi: Long): Bounded = Bounded(Finite(lo), Finite(hi))
    // Return (lo, hi) as sizes unless arg is or contains Infinite.
    def unapply(x: Size): Option[(Long, Long)] = x match {
      case Finite(n)                       => some((n, n))
      case Bounded(Finite(lo), Finite(hi)) => some((lo, hi))
      case _                               => none()
    }
  }
}

object Size {
  val Zero     = new Precise(0)
  val One      = new Precise(1)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def min(lhs: Size, rhs: Size): Size = (lhs, rhs) match {
    case (Finite(x), Finite(y))                   => if (x <= y) lhs else rhs
    case (_, Infinite)                            => lhs
    case (Infinite, _)                            => rhs
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Range(min(l1, l2), min(h1, h2))
  }
  def max(lhs: Size, rhs: Size): Size = (lhs, rhs) match {
    case (Finite(x), Finite(y))                   => if (x >= y) lhs else rhs
    case (Infinite, _) | (_, Infinite)            => Infinite
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Range(max(l1, l2), max(h1, h2))
  }

  def apply(size: Long): Precise = new Precise( if (size < 0L) 0L else size )

  object Range {
    /** Preserving associativity/commutativity of Size prevents us from
     *  modifying values to enforce any invariants on Bounded.
     */
    def apply(lo: Size, hi: Size): Size = if (lo == hi) lo else (lo, hi) match {
      case (lo: Precise, hi: Atomic)             => Bounded(lo, hi)
      case (Range(l1, h1), Range(l2, h2))        => apply(min(l1, l2), max(h1, h2))
    }
    def unapply(x: Size): Some[(Atomic, Atomic)] = x match {
      case Bounded(lo, hi) => scala.Some((lo, hi))
      case x: Atomic       => scala.Some((x, x))
    }
  }
}

/** A richer function abstraction.
 *
 *  No way to avoid at least having apply as a member method if there's
 *  to be any hope of seeing these converted into scala.Functions.
 */
sealed abstract class Fun[-A, +B] {
  self =>

  final def apply(x: A): B = this match {
    case Opaque(g)       => g(x)
    case OrElse(u1, u2)  => if (u1 isDefinedAt x) u1(x) else u2(x)
    case Defaulted(g, u) => if (u isDefinedAt x) u(x) else g(x)
    case FilterIn(_, u)  => u(x) // filter is checked at isDefinedAt
    case AndThen(u1, u2) => u2(u1(x))
    case ExMap(_, g)     => g(x)
  }
  final def isDefinedAt(x: A): Boolean = this match {
    case Opaque(_)       => true
    case OrElse(u1, u2)  => (u1 isDefinedAt x) || (u2 isDefinedAt x)
    case FilterIn(p, u)  => p(x) && (u isDefinedAt x)
    case Defaulted(_, u) => u isDefinedAt x
    case AndThen(u1, u2) => (u1 isDefinedAt x) && (u2 isDefinedAt u1(x))
    case ExMap(ks, _)    => ks(x)
  }
  def toPartial: A ?=> B = new (A ?=> B) {
    def isDefinedAt(x: A) = self isDefinedAt x
    def apply(x: A)       = self(x)
  }
}
final case class Opaque[-A, +B](f: A => B)                       extends Fun[A, B]
final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])      extends Fun[A, B]
final case class FilterIn[-A, +B](p: A => Boolean, u: Fun[A, B]) extends Fun[A, B]
final case class OrElse[-A, +B](f: Fun[A, B], g: Fun[A, B])      extends Fun[A, B]
final case class AndThen[-A, B, +C](f: Fun[A, B], g: Fun[B, C])  extends Fun[A, C]
final case class ExMap[A, +B](keys: ExSet[A], f: Fun[A, B])      extends Fun[A, B]

object Fun {
  def apply[A, B](f: A => B): Opaque[A, B] = Opaque(f)
}
object Vindex {
  val Zero = new AnyRef
  val One  = new AnyRef
}

/** A not very impressive attempt to improve on string
 *  representations.
 */
sealed abstract class Doc

object Doc {
  final case object NoDoc                             extends Doc
  final case class Group(xs: View[Doc])               extends Doc
  final case class Cat(left: Doc, right: Doc)         extends Doc
  final case class Shown[A](value: A, shows: Show[A]) extends Doc
  final case class Literal(value: String)             extends Doc

  def empty: Doc = NoDoc
  def apply[A](x: A)(implicit z: Show[A]): Shown[A] = Shown[A](x, z)
  def apply(s: String): Literal                     = Literal(s)
}

/** Virtual Index.
 */

final class Vindex[Base] private[api] (val indexValue: Long) extends AnyVal {
  type This = Vindex[Base]

  def create(indexValue: Long): This = new Vindex[Base](indexValue)

  private def mapLong(f: Long => Long): This = if (isInvalid) this else create(f(indexValue))

  def nthValue: Long         = indexValue + 1
  def get: Long              = indexValue
  def getInt: Int            = safeLongToInt(indexValue)
  def isEmpty                = indexValue < 0
  def isInvalid              = indexValue < 0
  def toIndex: Index         = Index(indexValue)
  def toNth: Nth             = Nth(nthValue)
  def sizeExcluding: Precise = Size(indexValue)
  def sizeIncluding: Precise = Size(nthValue)

  def +(n: Long): This = mapLong(_ + n)
  def -(n: Long): This = mapLong(_ - n)
  def next: This       = this + 1
  def prev: This       = this - 1
}

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  Manipulations of invalid values remain invalid, like NaN.
 *  All valid indices give rise to a corresponding Nth which is
 *  one larger, i.e. Index(3) is equivalent to Nth(4).
 */
object Index extends (Long => Index) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get < 0 }

  def invalid: Index            = new Index(-1L)
  def apply(value: Long): Index = if (value < 0) invalid else new Index(value)
  def unapply(x: Vdex)          = new Extractor(x.indexValue)
}

/** Nth is a 1-based index.
 */
object Nth extends (Long => Nth) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get <= 0 }

  def invalid: Nth            = new Nth(-1L)
  def apply(value: Long): Nth = if (value <= 0) invalid else new Nth(value - 1)
  def unapply(x: Vdex)        = new Extractor(x.nthValue)
}
