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
sealed trait Size
sealed trait Atomic extends Size
final case object Infinite extends Atomic
final class Precise private[api](val getLong: Long) extends Atomic {
  def /(n: Long): Precise = Precise(getLong / n)
  def *(n: Long): Precise = Precise(getLong * n)
  def +(n: Long): Precise = Precise(getLong + n)
  def -(n: Long): Precise = Precise(getLong - n)
}
final case class Bounded private[api](lo: Precise, hi: Atomic) extends Size

object Precise extends (Long => Precise) {
  final class Extractor(val get: Long) extends AnyVal { def isEmpty = get < 0 }

  def apply(n: Long): Precise        = new Precise(cond(n < 0, 0L, n))
  def unapply(n: Precise): Extractor = new Extractor(n.getLong)

  object Range {
    def apply(lo: Long, hi: Long): Bounded = Bounded(Precise(lo), Precise(hi))
    // Return (lo, hi) as sizes unless arg is or contains Infinite.
    def unapply(x: Size): Option[(Long, Long)] = x match {
      case Precise(n)                        => some((n, n))
      case Bounded(Precise(lo), Precise(hi)) => some((lo, hi))
      case _                                 => none()
    }
  }
}

object Size {
  val Zero     = new Precise(0)
  val One      = new Precise(1)
  val Unknown  = Bounded(Zero, Infinite)
  val NonEmpty = Bounded(One, Infinite)

  def equiv(lhs: Size, rhs: Size): Bool = (lhs, rhs) match {
    case (Precise(l), Precise(r))           => l == r
    case (Infinite, Infinite)               => true
    case (Bounded(l1, h1), Bounded(l2, h2)) => equiv(l1, l2) && equiv(h1, h2)
    case _                                  => false
  }
  def min(lhs: Size, rhs: Size): Size = (lhs, rhs) match {
    case (Precise(x), Precise(y))                 => if (x <= y) lhs else rhs
    case (_, Infinite)                            => lhs
    case (Infinite, _)                            => rhs
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Range(min(l1, l2), min(h1, h2))
  }
  def max(lhs: Size, rhs: Size): Size = (lhs, rhs) match {
    case (Precise(x), Precise(y))                 => if (x >= y) lhs else rhs
    case (Infinite, _) | (_, Infinite)            => Infinite
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Range(max(l1, l2), max(h1, h2))
  }

  def apply(size: Long): Precise = new Precise(if (size < 0L) 0L else size)

  object Range {

    /** Preserving associativity/commutativity of Size prevents us from
      *  modifying values to enforce any invariants on Bounded.
      */
    def apply(lo: Size, hi: Size): Size = (lo, hi) match {
      case _ if equiv(lo, hi)             => lo
      case (lo: Precise, hi: Atomic)      => Bounded(lo, hi)
      case (Range(l1, h1), Range(l2, h2)) => apply(min(l1, l2), max(h1, h2))
    }
    def unapply(x: Size): Some[(Atomic, Atomic)] = x match {
      case Bounded(lo, hi) => scala.Some((lo, hi))
      case x: Atomic       => scala.Some((x, x))
    }
  }
}

/** Virtual Index.
  */
final class Vindex[Base] private[api](val indexValue: Long) extends AnyVal {
  type This = Vindex[Base]

  def create(indexValue: Long): This = new Vindex[Base](indexValue)

  private def mapLong(f: Long => Long): This = cond(isInvalid, this, create(f(indexValue)))

  def nthValue: Long     = indexValue + 1
  def isEmpty            = indexValue < 0
  def isInvalid          = indexValue < 0
  def toIndex: Index     = Index(indexValue)
  def toNth: Nth         = Nth(nthValue)
  def excluding: Precise = Size(indexValue)
  def including: Precise = Size(nthValue)

  def +(n: Long): This = mapLong(_ + n)
  def -(n: Long): This = mapLong(_ - n)
  def next: This       = this + 1
  def prev: This       = this - 1
}
object Vindex {
  val Zero = new AnyRef
  val One  = new AnyRef
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

object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: Joiner[R, A, B]): R          = z.join(pair(x, y))
  def unapply[R, A, B](x: R)(implicit z: Splitter[R, A, B]): Some[A -> B] = scala.Some(z split x)
}
object :: {
  def apply[R, A, B](x: A, y: B)(implicit z: Joiner[R, A, B]): R = Pair(x, y)
  def unapply[R, A, B](x: R)(implicit z1: Splitter[R, A, B], z2: Empty[R], z3: Eq[R]): Option[A -> B] =
    if (z3.eqv(x, z2.empty)) none() else some(z1 split x)
}
