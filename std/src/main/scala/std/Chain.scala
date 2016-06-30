package psp
package std
package chain

import all._, StdShow._, OffsetLength.Empty

final class OffsetLength(private val bits: Long) extends AnyVal {
  def >>(n: Int): OffsetLength = OffsetLength(offset + n, length)
  def <<(n: Int): OffsetLength = OffsetLength(offset - n, length)

  def offset: Int                = (bits >> 32).toInt
  def length: Int                = bits.toInt
  def end: Int                   = offset + length
  def last: Int                  = end - 1
  def take(n: Int): OffsetLength = cond(n <= 0, Empty, OffsetLength(offset, min(n, length)))
  def drop(n: Int): OffsetLength = cond(n <= 0, this, OffsetLength(offset + n, length - n))

  override def toString = pp"$offset:$length"
}
object OffsetLength {
  val Empty = new OffsetLength(0L)

  @inline def apply(offset: Int, length: Int): OffsetLength =
    if (length <= 0) Empty else new OffsetLength((offset.toLong << 32) | length.toLong)
}

final case class ChainView[+A](chain: Chain, get: Long => A) extends Direct[A] {
  def size: Precise                           = chain.size
  def apply(idx: Index): A                    = get(chain(idx))
  def onChain(f: ToSelf[Chain]): ChainView[A] = ChainView(f(chain), get)
}
object ChainView {
  implicit class ChainViewOps[A](private val v: ChainView[A]) extends AnyVal with Chain.TransformChainOps {
    type Rep = ChainView[A]

    protected def self: Chain                             = v.chain
    protected implicit def liftChainResult(c: Chain): Rep = v.copy(chain = c)
  }
}

sealed trait Chain extends ShowSelf {
  def inner_s: String = this match {
    case Chain.NoChain              => "[]"
    case Chain.Band(s, Precise(1L)) => pp"$s"
    case Chain.Band(s, len)         => pp"$s:$len"
    case Chain.Pair(l, r)           => pp"${l.inner_s},${r.inner_s}"
  }
  def to_s: String = pp"[$inner_s]"
}
sealed trait NonEmptyChain extends Chain
sealed trait Pair extends NonEmptyChain {
  def left: NonEmptyChain
  def right: NonEmptyChain
}
object Chain {
  def empty: Chain                            = NoChain
  def apply(start: Long, len: Precise): Chain = cond(len.isZero, empty, Band(start, len))
  def pair(l: Chain, r: Chain): Chain         = (l, r) match {
    case (NoChain, _)                         => r
    case (_, NoChain)                         => l
    case (l: NonEmptyChain, r: NonEmptyChain) => StrictPair(l, r)
  }

  implicit class ChainOps(protected val self: Chain) extends AnyVal with TransformChainOps {
    type Rep = Chain
    protected implicit def liftChainResult(c: Chain) = c
  }

  trait TransformChainOps extends Any with CommonChainOps {
    type Rep

    protected implicit def liftChainResult(c: Chain): Rep

    def >>(shift: Precise): Rep = self match {
      case NoChain      => empty
      case Band(s, len) => Chain(s + shift.getLong, len)
      case Pair(l, r)   => pair(l >> shift, r >> shift)
    }
    def <<(shift: Precise): Rep = self match {
      case NoChain      => empty
      case Band(s, len) => Chain(s - shift.getLong, len)
      case Pair(l, r)   => pair(l << shift, r << shift)
    }
    def take(n: Precise): Rep = self match {
      case NoChain          => empty
      case Band(start, len) => Chain(start, all.min(len, n))
      case Pair(l, r)       => cond(l.size > n, l take n, pair(l, r take n - l.size))
    }
    def takeRight(n: Precise): Rep = self match {
      case NoChain          => empty
      case Band(start, len) => all.min(len, n) |> (n => Chain(exclusiveEnd - n.getLong, n))
      case Pair(l, r)       => cond(l.size >= n, l take n, pair(l, r take n - l.size))
    }
    def drop(n: Precise): Rep = self match {
      case NoChain          => empty
      case Band(start, len) => cond(len <= n, empty, Chain(start + n.getLong, len - n))
      case Pair(l, r)       => cond(l.size > n, pair(l drop n, r), r drop n - l.size)
    }
    def dropRight(n: Precise): Rep = self match {
      case NoChain          => empty
      case Band(start, len) => cond(len <= n, empty, Chain(start, len - n))
      case Pair(l, r)       => cond(r.size > n, pair(l, r drop n), l drop n - r.size)
    }
    def filter(p: ToBool[Long]): Rep = self match {
      case NoChain          => empty
      case Band(start, len) => prefixLength(p) |> (n => pair(self take n, self drop n dropWhile !p filter p))
      case Pair(l, r)       => pair(l filter p, r filter p)
    }
    def takeWhile(p: ToBool[Long]): Rep  = take(prefixLength(p))
    def dropWhile(p: ToBool[Long]): Rep  = drop(prefixLength(p))

    def span(p: ToBool[Long]): PairOf[Rep]    = splitAfter(prefixLength(p))
    def splitAfter(len: Precise): PairOf[Rep] = take(len) -> drop(len)

    // def grouped(n: Precise): Pstream[Rep] = self match {
    //   case NoChain => Pstream.empty
    //   case _       => Pstream(take(n), drop(n) grouped n)
    // }
  }

  trait CommonChainOps extends Any {
    protected def self: Chain

    def isEmpty: Bool = self eq NoChain

    def apply(idx: Index): Long                = self drop idx.indexValue min
    def map[A](f: Long => A): ChainView[A]     = new ChainView(self, f)
    def prefixLength(p: ToBool[Long]): Precise = foldl(0L)((res, n) => if (p(n)) res + 1 else return res.size)
    def foreach(f: Long => Unit): Unit         = if (!isEmpty) ll.foreachLongNonEmpty(min, max, f)
    def foreachReverse(f: Long => Unit): Unit  = if (!isEmpty) ll.foreachLongNonEmptyReverse(max, min, f)

    def foldl[R](zero: R)(f: ScalaFun.LeftFold[R, Long]): R = {
      var res = zero
      foreach(x => res = f(res, x))
      res
    }
    def min: Long = self match {
      case NoChain    => abort("empty")
      case Band(s, _) => s
      case Pair(l, r) => l.min
    }
    def max: Long = self match {
      case NoChain      => abort("empty")
      case Band(s, len) => exclusiveEnd - 1
      case Pair(l, r)   => r.max
    }
    def exclusiveEnd: Long = self match {
      case NoChain      => 0
      case Band(s, len) => s + len.getLong
      case Pair(l, r)   => cond(r.isEmpty, l.exclusiveEnd, r.exclusiveEnd)
    }
    def size: Precise = self match {
      case NoChain      => 0
      case Band(_, len) => len
      case Pair(l, r)   => l.size + r.size
    }
  }

  object Empty {
    def unapply(x: Chain): Bool = x.isEmpty
  }
  object Pair {
    def unapply(x: Pair) = some(x.left -> x.right)
  }

  final case object NoChain extends Chain
  final case class Band(start: Long, size: Precise) extends NonEmptyChain {
    assert(!this.isEmpty, "zero")
  }
  final case class StrictPair(left: NonEmptyChain, right: NonEmptyChain) extends Pair
  final class LazyPair(l: => NonEmptyChain, r: => NonEmptyChain) extends Pair {
    lazy val left  = l
    lazy val right = r
  }
}
