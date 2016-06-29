package psp
package std

import all._, Size._

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
sealed abstract class Size extends InheritedHashEq with HasToS {
  def atLeast: Size = Range(this, Infinite)
  def atMost: Size  = Range(_0, this)
  def isZero: Bool  = this match {
    case Precise(0) => true
    case _          => false
  }
  def getInt: Int = this match {
    case Precise(n) => safeLongToInt(n)
    case s          => illegalArgumentException(s)
  }
  def preciseOrMaxLong: Precise = this match {
    case n: Precise => n
    case _          => Precise(MaxLong)
  }
  def +(rhs: Size): Size = (this, rhs) match {
    case (x: Atomic, y: Atomic)                   => x + y
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 + l2, h1 + h2)
  }
  def -(rhs: Size): Size = (this, rhs) match {
    case (Precise(l), Precise(r))                 => Precise(l - r)
    case (Precise(_), Infinite)                   => _0
    case (Infinite, Precise(_))                   => Infinite
    case (Infinite, Infinite)                     => Unknown
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 - h2, h1 - l2)
  }
  def hiBound: Atomic = this match {
    case Bounded(_, hi) => hi
    case x: Atomic      => x
  }

  def mapOver(f: Precise => Precise, g: Atomic => Atomic): Size = this match {
    case Bounded(lo, hi) => Bounded(f(lo), g(hi))
    case Infinite        => g(Infinite)
    case n: Precise      => f(n)
  }

  final override def equals(x: Any): Bool = (this, x) match {
    case (Precise(l), Precise(r))           => l == r
    case (Bounded(l1, h1), Bounded(l2, h2)) => l1 == l2 && h1 == h2
    case _                                  => sameRef(this, x)
  }

  def to_s: String = this match {
    case Precise(size)         => any"$size"
    case Bounded(lo, Infinite) => any"${lo.to_s}+"
    case Bounded(lo, hi)       => any"${lo.to_s},${hi.to_s}]"
    case Infinite              => "<inf>"
  }
}
sealed trait Atomic extends Size {
  def slice(r: SliceRange): Atomic = (this - r.startLong) |> (s => r.size.foldInfinite(s, exp.min(s, _)))

  def -(rhs: Precise): Atomic = this match {
    case Precise(l) => Precise(l - rhs.getLong)
    case Infinite   => Infinite
  }
  def +(rhs: Atomic): Atomic = (this, rhs) match {
    case (Precise(l), Precise(r)) => Precise(l + r)
    case _                        => Infinite
  }
  def getLongOrMax: Long = this match {
    case Infinite   => MaxLong
    case n: Precise => n.getLong
  }
}

final case object Infinite extends Atomic
final case class Bounded private[std](lo: Precise, hi: Atomic) extends Size
final class Precise private[std](val getLong: Long) extends Atomic {
  override def slice(r: SliceRange): Precise = cast(super.slice(r))

  def /(n: Long): Precise    = Precise(getLong / n)
  def *(n: Long): Precise    = Precise(getLong * n)
  def +(n: Long): Precise    = Precise(getLong + n)
  def -(n: Long): Precise    = Precise(getLong - n)
  def +(n: Precise): Precise = Precise(getLong + n.getLong)
  def -(n: Atomic): Precise  = n match {
    case Precise(r) => Precise(getLong - r)
    case Infinite   => _0
  }
  def exclusive: Index = Index(getLong)
  def lastIndex: Index = exclusive.prev // effectively maps both undefined and zero to no index.
  def indices          = 0L indexUntil getLong
}

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
  val Zero: Precise = Size(0)
  val One: Precise  = Size(1)
  val Unknown       = Zero.atLeast
  val NonEmpty      = One.atLeast

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
      case _ if lo == hi                  => lo
      case (lo: Precise, hi: Atomic)      => Bounded(lo, hi)
      case (Range(l1, h1), Range(l2, h2)) => apply(min(l1, l2), max(h1, h2))
    }
    def unapply(x: Size): Some[(Atomic, Atomic)] = x match {
      case Bounded(lo, hi) => scala.Some((lo, hi))
      case x: Atomic       => scala.Some((x, x))
    }
  }
}

trait HasSize {
  def size: Size
}
object HasSize {
  def unapply(x: HasSize): Some[Size] = Some(x.size)
}
object HasPreciseSize {
  def unapply(x: HasSize): Opt[Precise] = some(x.size) collect { case x: Precise => x }
}
