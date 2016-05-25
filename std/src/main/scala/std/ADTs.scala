package psp
package std

import exp._, Size._, all.PspLongOps

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
sealed trait Size {
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
    case (Precise(l), Precise(r))                 => Precise(l + r)
    case (Infinite, _) | (_, Infinite)            => Infinite
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 + l2, h1 + h2)
  }
  def -(rhs: Size): Size = (this, rhs) match {
    case (Precise(l), Precise(r))                 => Precise(l - r)
    case (Precise(_), Infinite)                   => _0
    case (Infinite, Precise(_))                   => Infinite
    case (Infinite, Infinite)                     => Unknown
    case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 - h2, h1 - l2)
  }
}
sealed trait Atomic extends Size
final case object Infinite extends Atomic
final class Precise private[std](val getLong: Long) extends Atomic {
  def /(n: Long): Precise    = Precise(getLong / n)
  def *(n: Long): Precise    = Precise(getLong * n)
  def +(n: Long): Precise    = Precise(getLong + n)
  def -(n: Long): Precise    = Precise(getLong - n)
  def +(n: Precise): Precise = Precise(getLong + n.getLong)
  def -(n: Precise): Precise = Precise(getLong - n.getLong)

  def exclusive: Index        = Index(getLong)
  def indices: VdexRange      = 0L indexUntil getLong
  def lastIndex: Index        = exclusive.prev // effectively maps both undefined and zero to no index.
}
final case class Bounded private[std](lo: Precise, hi: Atomic) extends Size

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
final class Vindex[Base] private[std](val indexValue: Long) extends AnyVal {
  type This = Vindex[Base]

  def create(indexValue: Long): This = new Vindex[Base](indexValue)

  private def mapLong(f: Long => Long): This = cond(isInvalid, this, create(f(indexValue)))

  def +(n: Long): This   = mapLong(_ + n)
  def -(n: Long): This   = mapLong(_ - n)
  def excluding: Precise = Size(indexValue)
  def getInt: Int        = safeLongToInt(indexValue) // depend on this
  def including: Precise = Size(nthValue)
  def isEmpty            = indexValue < 0
  def isInvalid          = indexValue < 0
  def next: This         = this + 1
  def nthValue: Long     = indexValue + 1
  def prev: This         = this - 1
  def toIndex: Index     = Index(indexValue)
  def toNth: Nth         = Nth(nthValue)
}
object Vindex {
  val Zero = new AnyRef
  val One  = new AnyRef
}
trait ZeroOne[+A] {
  def zero: A
  def one: A
}

trait ZeroOne0 {
  self: ZeroOne.type =>

  implicit val zeroSize: ZeroOne[Size] = make[Size](Size(0), Size(1))
}
object ZeroOne {
  def make[A](z: A, o: A): ZeroOne[A] = new ZeroOne[A] {
    def zero: A = z
    def one: A  = o
  }

  implicit val zeroIndex: ZeroOne[Index]     = make(Index(0), Index(1))
  implicit val zeroPrecise: ZeroOne[Precise] = make(Size(0), Size(1))
}
