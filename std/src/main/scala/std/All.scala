package psp
package std

import psp.api._
import java.{ lang => jl }
import scala.Tuple2
import java.io.BufferedInputStream

/** One import which includes the implicits, one which doesn't.
  *  This choice is mutually exclusive: everything which is in exp is in all.
  */
object exp extends AllExplicit
object all extends AllExplicit with AllImplicit {
  /** The type of args forces all the interpolation variables to
    * be of a type which is implicitly convertible to Doc.
    */
  implicit class DocInterpolators(private val sc: StringContext) extends AnyVal {
    def pp(args: Doc*): String  = ShowInterpolator(sc).pp(args: _*)
    def fpp(args: Doc*): String = ShowInterpolator(sc).fpp(args: _*)
    def sm(args: Doc*): String  = ShowInterpolator(sc).sm(args: _*)
  }

  implicit class VindexOps(private val vdex: Vdex) {
    def getInt: Int = vdex.indexValue.safeToInt
  }
  implicit class AnyOps[A](private val x: A) extends AnyVal {
    def any_s: String                         = s"$x"
    def id_## : Int                           = java.lang.System.identityHashCode(x)
    def id_==(y: Any): Boolean                = cast[AnyRef](x) eq cast[AnyRef](y)
    def matchIf[B : Empty](pf: A ?=> B): B    = matchOr(emptyValue[B])(pf)
    def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt

    @inline def |>[B](f: A => B): B = f(x) // The famed forward pipe.
  }
  implicit class SizeOps(private val lhs: Size) extends AnyVal {
    import Size._

    def getInt: Int = lhs match {
      case Finite(n) => n.toInt
      case s         => illegalArgumentException(s)
    }
    def isNonZero     = loBound =!= Zero
    def isZero        = lhs === Zero
    def atLeast: Size = Size.Range(lhs, Infinite)
    def atMost: Size  = Size.Range(Zero, lhs)

    def loBound: Atomic = lhs match {
      case Bounded(lo, _) => lo
      case x: Atomic      => x
    }

    /** For instance taking the union of two sets. The new size is
      *  at least the size of the larger operand, but at most the sum
      *  of the two sizes.
      */
    def union(rhs: Size): Size     = Size.Range(lhs max rhs, lhs + rhs)
    def intersect(rhs: Size): Size = Size.Range(Size.Zero, lhs min rhs)
    def diff(rhs: Size): Size      = Size.Range(lhs - rhs, lhs)

    def +(rhs: Size): Size = (lhs, rhs) match {
      case (Finite(l), Finite(r))                   => Finite(l + r)
      case (Infinite, Finite(_))                    => Infinite
      case (Finite(_), Infinite)                    => Infinite
      case (Infinite, Infinite)                     => Infinite
      case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 + l2, h1 + h2)
    }
    def -(rhs: Size): Size = (lhs, rhs) match {
      case (Finite(l), Finite(r))                   => Finite(l - r)
      case (Finite(_), Infinite)                    => Zero
      case (Infinite, Finite(_))                    => Infinite
      case (Infinite, Infinite)                     => Unknown
      case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 - h2, h1 - l2)
    }
    def min(rhs: Size): Size = Size.min(lhs, rhs)
    def max(rhs: Size): Size = Size.max(lhs, rhs)
  }

  implicit class CharOps(private val ch: Char) extends AnyVal {
    // def isAlphabetic = jl.Character isAlphabetic ch
    def isControl = jl.Character isISOControl ch
    // def isDigit      = jl.Character isDigit ch
    // def isLetter     = jl.Character isLetter ch
    // def isLower      = jl.Character isLowerCase ch
    // def isUpper      = jl.Character isUpperCase ch
    // def toLower      = jl.Character toLowerCase ch
    // def isSpace      = jl.Character isWhitespace ch
    def toUpper = jl.Character toUpperCase ch
    def to_s    = ch.toString

    def takeNext(len: Precise): CharRange = ch.toLong takeNext len map (_.toChar)
    def to(end: Char): CharRange          = LongInterval.to(ch.toLong, end) map (_.toChar)
    def until(end: Char): CharRange       = LongInterval.until(ch.toLong, end) map (_.toChar)
  }
  implicit class IntOps(private val self: Int) extends AnyVal {
    def to(end: Int): IntRange    = LongInterval.to(self, end) map (_.toInt)
    def until(end: Int): IntRange = LongInterval.until(self, end) map (_.toInt)
  }
  implicit class LongOps(private val self: Long) extends AnyVal {
    /** Safe in the senses that it won't silently truncate values,
      *  and will translate MaxLong to MaxInt instead of -1.
      *  We depend on this!
      */
    def safeToInt: Int = self match {
      case MaxLong => MaxInt
      case MinLong => MinInt
      case _       => assertInIntRange() ; self.toInt
    }
    def takeNext(len: Precise): LongRange = LongInterval.closed(self, len) map identity
    def to(end: Long): LongRange          = LongInterval.to(self, end) map identity
    def until(end: Long): LongRange       = LongInterval.until(self, end) map identity

    private def assertInIntRange(): Unit = assert(MinInt <= self && self <= MaxInt, s"$self out of range")
  }
  implicit class PreciseOps(private val size: Precise) {
    def indices: VdexRange = indexRange(0, size.getLong)
    def lastIndex: Index   = Index(size.getLong - 1) // effectively maps both undefined and zero to no index.

    def +(n: Precise): Precise             = size + n.getLong
    def -(n: Precise): Precise             = size - n.getLong
    def containsIndex(vdex: Vdex): Boolean = indices containsLong vdex.indexValue

    def min(rhs: Precise): Precise = all.min(size, rhs)
  }

  implicit class FunOps[A, B](private val f: Fun[A, B]) extends AnyVal {
    import Fun._

    def applyOrElse(x: A, g: A => B): B       = cond(f isDefinedAt x, f(x), g(x))
    def zfold[C : Empty](x: A)(g: B => C): C  = cond(f isDefinedAt x, g(f(x)), emptyValue)
    def zapply(x: A)(implicit z: Empty[B]): B = zfold(x)(identity)
    def get(x: A): Option[B]                  = zfold(x)(some)
    def mapIn[C](g: C => A): Fun[C, B]        = AndThen(Opaque(g), f)
    def mapOut[C](g: B => C): Fun[A, C]       = AndThen(f, Opaque(g))

    def defaulted(g: A => B): Defaulted[A, B] = f match {
      case Defaulted(_, u) => Defaulted(g, u)
      case _               => Defaulted(g, f)
    }

    def filterIn(p: A => Boolean): FilterIn[A, B] = f match {
      case FilterIn(p0, u) => FilterIn(x => p0(x) && p(x), u)
      case _               => FilterIn(p, f)
    }

    def traced(in: A => Unit, out: B => Unit): Fun[A, B] =
      mapIn[A](x => doto(x)(in)) mapOut (x => doto(x)(out))
  }

  implicit class ByteArrayOps(private val xs: Array[Byte]) {
    def utf8Chars: Array[Char] = scala.io.Codec fromUTF8 xs
    def utf8String: String     = new String(utf8Chars)
  }

  implicit class ArrayOps[A](private val xs: Array[A]) {
    private def arraycopy[A](src: Array[A], srcPos: Int, dst: Array[A], dstPos: Int, len: Int): Unit =
      java.lang.System.arraycopy(src, srcPos, dst, dstPos, len)

    def inPlace: InPlace[A] = new InPlace(xs)
    def ++(that: Array[A])(implicit z: CTag[A]): Array[A] = {
      val arr = newArray[A](xs.length + that.length)
      arraycopy(xs, 0, arr, 0, xs.length)
      arraycopy(that, 0, arr, xs.length, that.length)
      arr
    }
  }

  implicit class HashEqOrdOps[A](private val z: HashEqOrd[A]) {
    def on[B](f: B => A): HashEqOrd[B] = HashEqOrd(z.eqv _ on f, z.cmp _ on f, f andThen z.hash)
  }

  implicit class ApiOrderOps[A](private val ord: Order[A]) {
    import ord._

    def flip: Order[A] = Order((x, y) => ord.cmp(x, y).flip)

    def |[B](f: A => B)(implicit z: Order[B]): Order[A] = Order((x, y) => cmp(x, y) | z.cmp(f(x), f(y)))

    def comparator[A](implicit z: Order[A]): Comparator[A] = new scala.math.Ordering[A] {
      def compare(x: A, y: A): Int = z.cmp(x, y).intValue
    }
  }

  implicit class ArrowAssocRef[A](private val self: A) extends AnyVal {
    @inline def ->[B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }
  implicit class JavaIteratorOps[A](private val it: jIterator[A]) {
    def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
  }
  implicit class CmpEnumOps(private val cmp: Cmp) {
    import Cmp._
    def flip: Cmp = cmp match {
      case LT => GT
      case GT => LT
      case EQ => EQ
    }
    def |(that: => Cmp): Cmp = if (cmp == EQ) that else cmp
  }
  implicit class ViewOpOps[A, B](private val op: Op[A, B]) {
    def apply[M[X]](xs: M[A])(implicit z: Operable[M]): M[B] = z(xs)(op)
    def ~[C](that: Op[B, C]): Op[A, C]                       = Op.Compose[A, B, C](op, that)
  }
  implicit class ExMapOps[K, V](private val lookup: ExMap[K, V]) {
    import lookup._

    def apply(key: K): V                  = lookup(key)
    def map[V1](g: V => V1): ExMap[K, V1] = keys map (f mapOut g)
    def values: View[V]                   = keys.toVec map lookup
  }
  implicit class ExSetOps[A](private val xs: ExSet[A]) {
    def map [B](f: A => B): ExMap[A, B]           = Fun.finite(xs, f)
    def flatMap[B](f: A => (A => B)): ExMap[A, B] = Fun.finite(xs, x => f(x)(x))
  }

  /** Extension methods for scala library classes.
    *  We'd like to get away from all such classes,
    *  but scala doesn't allow it.
    */
  implicit class OptionOps[A](val x: Option[A]) extends AnyVal {
    def or(alt: => A): A               = x getOrElse alt
    def toVec: Vec[A]                  = this zfold (x => vec(x))
    def zfold[B : Empty](f: A => B): B = x.fold[B](emptyValue)(f)
    def zget(implicit z: Empty[A]): A  = x getOrElse z.empty
    def |(alt: => A): A                = x getOrElse alt
  }
  implicit class TryOps[A](val x: Try[A]) extends AnyVal {
    def |(expr: => A): A = x.toOption | expr
    def fold[B](f: Throwable => B, g: A => B): B = x match {
      case Success(x) => g(x)
      case Failure(t) => f(t)
    }
  }
  implicit class InputStreamOps(val in: InputStream) extends AnyVal {
    def buffered: BufferedInputStream = in match {
      case in: BufferedInputStream => in
      case _                       => new BufferedInputStream(in)
    }
    def slurp(): Array[Byte]             = ll.Streams slurp buffered
    def slurp(len: Precise): Array[Byte] = ll.Streams.slurp(buffered, len)
  }
  implicit class Product2HomoOps[R, A](val x: R)(implicit z: Splitter[R, A, A]) {
    def map2[B](f: A => B): B -> B = z split x mapEach (f, f)
    def each: Each[A]              = Each pair x
    def seq: scSeq[A]              = each.seq
  }
  implicit class Product2HeteroOps[+A, +B](val x: A -> B) extends AnyVal {
    def appLeft[C](f: A => C): C                 = f(fst(x))
    def appRight[C](f: B => C): C                = f(snd(x))
    def app[C](f: (A, B) => C): C                = f(fst(x), snd(x))
    def mapEach[C](f: A => C, g: B => C): C -> C = f(fst(x)) -> g(snd(x))
    def mapLeft[C](f: A => C): C -> B            = f(fst(x)) -> snd(x)
    def mapRight[C](f: B => C): A -> C           = fst(x)    -> f(snd(x))
  }
  implicit class EqViewOps[A](val xs: View[A])(implicit eqv: Eq[A]) {
    def contains(x: A): Boolean = xs exists (_ === x)
    def distinct: View[A]       = xs.zfoldl[Vec[A]]((res, x) => cond(res.m contains x, res, res :+ x))
    def indexOf(x: A): Index    = xs indexWhere (_ === x)
    def toExSet: ExSet[A]       = xs.toExSet
  }
  implicit class ShowableDocOps[A](val lhs: A)(implicit shows: Show[A]) {
    def doc: Doc     = Doc(lhs)
    def show: String = shows show lhs
  }

  /** Conversions which require the elements to be pairs. Obtaining evidence of that
    *  up front simplifies everything else, because we don't have to mix and match
    *  between arity-1 and arity-2 type constructors.
    */
  implicit class SplittableViewOps[R, A, B](val xs: View[R])(implicit sp: Splitter[R, A, B]) {
    def toExMap(implicit z: Eq[A]): ExMap[A, B]                         = toMap[ExMap]
    def toMap[CC[_, _]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z contraMap sp.split build xs
  }
  implicit class BooleanAlgebraOps[A](val lhs: A)(implicit z: BooleanAlgebra[A]) {
    def &&(rhs: A): A = z.and(lhs, rhs)
    def ||(rhs: A): A = z.or(lhs, rhs)
    def unary_! : A   = z complement lhs
  }
  implicit class EqOps[A](val lhs: A)(implicit z: Eq[A]) {
    def ===(rhs: A): Boolean = z.eqv(lhs, rhs)
    def =!=(rhs: A): Boolean = !z.eqv(lhs, rhs)
  }
  implicit class HashOps[A](val lhs: A)(implicit z: Hash[A]) {
    def hash: Long   = z hash lhs
    def hashInt: Int = hash.toInt
  }
  implicit class OrderOps[A](val lhs: A)(implicit z: Order[A]) {
    def <(rhs: A): Boolean  = z.cmp(lhs, rhs) eq Cmp.LT
    def <=(rhs: A): Boolean = z.cmp(lhs, rhs) ne Cmp.GT
    def >(rhs: A): Boolean  = z.cmp(lhs, rhs) eq Cmp.GT
    def >=(rhs: A): Boolean = z.cmp(lhs, rhs) ne Cmp.LT
  }
  implicit class SplitterOps[R, A, B](val lhs: R)(implicit z: Splitter[R, A, B]) {
    def toPair: A -> B = z split lhs
  }
  implicit class SuspendedOps[A](s1: Suspended[A]) {
    def &&& (s2: Suspended[A]): Suspended[A] = f => sideEffect(s1(f), s2(f))
  }
  implicit class Function2Ops[A1, A2, R](f: (A1, A2) => R) {
    def map[S](g: R => S): (A1, A2) => S = (x, y) => g(f(x, y))
  }
  implicit class Function2SameOps[A, R](f: BinTo[A, R]) {
    def on[B](g: B => A): (B, B) => R = (x, y) => f(g(x), g(y))
  }
}
