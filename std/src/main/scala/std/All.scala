package psp
package std

import psp.api._
import java.{ lang => jl }
import scala.Tuple2
import java.io.BufferedInputStream

object Unsafe {
  implicit def promoteIndex(x: scala.Long) = Index(x)
  implicit def inheritedShow[A]: Show[A]   = Show.Inherited
}

/** One import which includes the implicits, one which doesn't.
  *  This choice is mutually exclusive: everything which is in exp is in all.
  */
object exp extends AllExplicit
object all extends NonValueImplicitClasses with AllImplicit  {
  implicit class AnyOps[A](private val x: A) extends AnyVal {
    def any_s: String                     = s"$x"
    def id_## : Int                       = java.lang.System.identityHashCode(x)
    def id_==(y: Any): Boolean            = cast[AnyRef](x) eq cast[AnyRef](y)
    def matchIf[B: Empty](pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else emptyValue

    @inline def |>[B](f: A => B): B = f(x) // The famed forward pipe.
  }
  implicit class ArrowAssocRef[A](private val self: A) extends AnyVal {
    @inline def ->[B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }
  implicit class OptionOps[A](private val x: Option[A]) extends AnyVal {
    def or(alt: => A): A              = x getOrElse alt
    def toVec: Vec[A]                 = this zfold (x => vec(x))
    def zfold[B: Empty](f: A => B): B = x.fold[B](emptyValue)(f)
    def zget(implicit z: Empty[A]): A = x getOrElse z.empty
    def |(alt: => A): A               = x getOrElse alt
  }
  implicit class TryOps[A](private val x: Try[A]) extends AnyVal {
    def |(expr: => A): A = x.toOption | expr
    def fold[B](f: Throwable => B, g: A => B): B = x match {
      case Success(x) => g(x)
      case Failure(t) => f(t)
    }
  }
  implicit class InputStreamOps(private val in: InputStream) extends AnyVal {
    def buffered: BufferedInputStream = in match {
      case in: BufferedInputStream => in
      case _                       => new BufferedInputStream(in)
    }
    def slurp(): Array[Byte]             = ll.Streams slurp buffered
    def slurp(len: Precise): Array[Byte] = ll.Streams.slurp(buffered, len)
  }
  implicit class FunOps[A, B](private val f: Fun[A, B]) extends AnyVal {
    type This = Fun[A, B]

    import Fun._

    def andThen[C](g: B => C): Fun[A, C]      = AndThen(f, Opaque(g))
    def applyOrElse(x: A, g: A => B): B       = cond(f contains x, f(x), g(x))
    def compose[C](g: C => A): Fun[C, B]      = AndThen(Opaque(g), f)
    def get(x: A): Option[B]                  = zfold(x)(some)
    def zapply(x: A)(implicit z: Empty[B]): B = zfold(x)(identity)
    def zfold[C: Empty](x: A)(g: B => C): C   = cond(f contains x, g(f(x)), emptyValue)

    def defaulted(g: A => B): Defaulted[A, B] = f match {
      case Defaulted(_, u) => Defaulted(g, u)
      case _               => Defaulted(g, f)
    }
    def filterIn(p: ToBool[A]): Filtered[A, B] = f match {
      case Filtered(q, u) => Filtered(q && p, u)
      case _              => Filtered(p, f)
    }

    def teeIn(g: ToUnit[A]): This                   = compose[A](x => doto(x)(g))
    def tee(g: ToUnit[B]): This                     = andThen[B](x => doto(x)(g))
    def traced(in: A => Unit, out: B => Unit): This = this teeIn in tee out
  }
}

class NonValueImplicitClasses extends AllExplicit {
  self: all.type =>

  implicit class HasViewsAs[A, R](val repr: R)(implicit z: ViewsAs[A, R]) {
    def m: IdView[A, R] = z viewAs repr
  }
  /** The type of args forces all the interpolation variables to
    * be of a type which is implicitly convertible to Doc.
    */
  implicit class DocInterpolators(private val sc: StringContext) {

    /** The type of args forces all the interpolation variables to
      *  be of a type which is implicitly convertible to Doc.
      */
    def doc(args: Doc*): Doc  = ShowInterpolator(sc).doc(args: _*)
    def fdoc(args: Doc*): Doc = ShowInterpolator(sc).fdoc(args: _*)
    def sdoc(args: Doc*): Doc = ShowInterpolator(sc).sdoc(args: _*)

    def pp(args: Doc*)(implicit z: Renderer): String = z show doc(args: _*)
    def fp(args: Doc*)(implicit z: Renderer): String = z show fdoc(args: _*)
    def sm(args: Doc*)(implicit z: Renderer): String = z show sdoc(args: _*)
  }
  implicit class VindexOps(private val vdex: Vdex) {
    def getInt: Int = vdex.indexValue.safeToInt
  }
  implicit class SizeOps(private val lhs: Size)  {
    import Size._

    def getInt: Int = lhs match {
      case Precise(n) => n.toInt
      case s          => illegalArgumentException(s)
    }
    def isNonZero     = loBound =!= Zero
    def isZero        = lhs === Zero
    def atLeast: Size = Size.Range(lhs, Infinite)
    def atMost: Size  = Size.Range(Zero, lhs)

    def loBound: Atomic = lhs match {
      case Bounded(lo, _) => lo
      case x: Atomic      => x
    }
    def hiBound: Atomic = lhs match {
      case Bounded(_, hi) => hi
      case x: Atomic      => x
    }

    def preciseOrMaxLong: Precise = hiBound match {
      case n: Precise => n
      case Infinite   => Precise(MaxLong)
    }
    /** For instance taking the union of two sets. The new size is
      *  at least the size of the larger operand, but at most the sum
      *  of the two sizes.
      */
    def union(rhs: Size): Size     = Size.Range(lhs max rhs, lhs + rhs)
    def intersect(rhs: Size): Size = Size.Range(Size.Zero, lhs min rhs)
    def diff(rhs: Size): Size      = Size.Range(lhs - rhs, lhs)

    def +(rhs: Size): Size = (lhs, rhs) match {
      case (Precise(l), Precise(r))                 => Precise(l + r)
      case (Infinite, Precise(_))                   => Infinite
      case (Precise(_), Infinite)                   => Infinite
      case (Infinite, Infinite)                     => Infinite
      case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 + l2, h1 + h2)
    }
    def -(rhs: Size): Size = (lhs, rhs) match {
      case (Precise(l), Precise(r))                 => Precise(l - r)
      case (Precise(_), Infinite)                   => Zero
      case (Infinite, Precise(_))                   => Infinite
      case (Infinite, Infinite)                     => Unknown
      case (Size.Range(l1, h1), Size.Range(l2, h2)) => Size.Range(l1 - h2, h1 - l2)
    }
    def min(rhs: Size): Size = Size.min(lhs, rhs)
    def max(rhs: Size): Size = Size.max(lhs, rhs)
  }

  implicit class CharOps(private val ch: Char) {
    def isControl = jl.Character isISOControl ch
    def toUpper   = jl.Character toUpperCase ch
    def to_s      = ch.toString

    def takeNext(len: Precise): CharRange = ch.toLong takeNext len map (_.toChar)
    def to(end: Char): CharRange          = ch.toLong to end.toLong map (_.toChar)
    def until(end: Char): CharRange       = ch.toLong until end.toLong map (_.toChar)
  }
  implicit class IntOps(private val self: Int) {
    def to(end: Int): IntRange    = self.toLong to end.toLong map (_.toInt)
    def until(end: Int): IntRange = self.toLong until end.toLong map (_.toInt)
  }
  implicit class LongOps(private val self: Long) {

    /** Safe in the senses that it won't silently truncate values,
      *  and will translate MaxLong to MaxInt instead of -1.
      *  We depend on this!
      */
    def safeToInt: Int = self match {
      case MaxLong => MaxInt
      case MinLong => MinInt
      case _       => assertInIntRange(); self.toInt
    }
    def takeNext(len: Precise): LongRange = closedRange(self, len)(x => x)
    def to(end: Long): LongRange          = Interval.to(self, end) map identity
    def until(end: Long): LongRange       = Interval.until(self, end) map identity

    private def assertInIntRange(): Unit = assert(MinInt <= self && self <= MaxInt, s"$self out of range")
  }
  implicit class PreciseOps(private val size: Precise) {
    def exclusive: Index   = Index(size.getLong)
    def indices: VdexRange = indexRange(0, size.getLong)
    def lastIndex: Index   = Index(size.getLong - 1) // effectively maps both undefined and zero to no index.

    def +(n: Precise): Precise             = size + n.getLong
    def -(n: Precise): Precise             = size - n.getLong
    def containsIndex(vdex: Vdex): Boolean = indices containsLong vdex.indexValue

    def min(rhs: Precise): Precise = all.min(size, rhs)
  }
  implicit class PartialOps[A, B](pf: A ?=> B) {
    def applyOr(x: A, alt: => B): B           = if (pf isDefinedAt x) pf(x) else alt
    def zapply(x: A)(implicit z: Empty[B]): B = applyOr(x, z.empty)
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

  implicit class EqClassOps[A](private val z: Eq[A]) {
    def on[B](f: B => A): Eq[B]         = Relation.equiv(z.eqv _ on f)
    def hashWith(f: ToLong[A]): Hash[A] = Relation.hash(z.eqv, f)

    def toHash: Hash[A] = z match {
      case heq: Hash[A] => heq
      case _            => hashWith(_ => 0)
    }
  }
  implicit class HashOrderClassOps[A](private val r: HashOrder[A]) {
    def on[B](f: B => A): HashOrder[B] = Relation.all(r.cmp _ on f, f andThen r.hash)
  }
  implicit class HashClassOps[A](private val r: Hash[A]) {
    def on[B](f: B => A): Hash[B] = Relation.hash(r.eqv _ on f, f andThen r.hash)
  }
  implicit class ShowClassOps[A](private val r: Show[A]) {
    def on[B](f: B => A): Show[B] = Show(f andThen r.show)
  }
  implicit class OrderClassOps[A](private val r: Order[A]) {
    import r._

    def flip: Order[A]                                  = Relation.order(cmp _ andThen (_.flip))
    def hashWith(f: ToLong[A]): HashOrder[A]            = Relation.all(cmp, f)
    def on[B](f: B => A): Order[B]                      = Relation.order[B](cmp _ on f)
    def |[B](f: A => B)(implicit z: Order[B]): Order[A] = Relation.order((x, y) => cmp(x, y) | z.cmp(f(x), f(y)))

    def comparator: Comparator[A] = new scala.math.Ordering[A] {
      def compare(x: A, y: A): Int = cmp(x, y).intValue
    }
  }
  implicit class JavaIteratorOps[A](private val it: jIterator[A]) {
    def toScala: scIterator[A] = new scIterator[A] {
      def hasNext: Bool = it.hasNext
      def next(): A     = it.next()
    }
    def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
  }
  implicit class CmpEnumOps(private val cmp: Cmp) {
    import Cmp._
    def flip: Cmp = cmp match {
      case LT => GT
      case GT => LT
      case EQ => EQ
    }
    def |(that: => Cmp): Cmp = if (cmp eq EQ) that else cmp
  }
  implicit class ViewOpOps[A, B](private val op: Op[A, B]) {
    def apply[M[X]](xs: M[A])(implicit z: Operable[M]): M[B] = z(xs)(op)
    def ~[C](that: Op[B, C]): Op[A, C]                       = Op.Compose[A, B, C](op, that)
  }
  implicit class SplittableValueSameTypeOps[R, A](private val x: R)(implicit z: Splitter[R, A, A]) {
    def map2[B](f: A => B): B -> B = z split x mapEach (f, f)
    def each: Each[A]              = Each pair x
  }
  implicit class ProductProductOps[A, B](private val xy: (A->B)->(A->B)) {
    def transpose: (A->A)->(B->B) = {
      val (l1 -> r1) -> (l2 -> r2) = xy
      (l1 -> l2) -> (r1 -> r2)
    }
  }

  implicit class SplittableValueOps[R, A, B](private val x: R)(implicit z: Splitter[R, A, B]) {
    def _1: A                                    = fst(z split x)
    def _2: B                                    = snd(z split x)
    def appLeft[C](f: A => C): C                 = f(_1)
    def appRight[C](f: B => C): C                = f(_2)
    def app[C](f: (A, B) => C): C                = f(_1, _2)
    def mapEach[C](f: A => C, g: B => C): C -> C = f(_1) -> g(_2)
    def mapLeft[C](f: A => C): C -> B            = f(_1) -> _2
    def mapRight[C](f: B => C): A -> C           = _1 -> f(_2)

    def apply[C](f: (A, B) => C): C = f(_1, _2)
  }
  implicit class SplittableViewOps[R, A, B](private val xs: View[R])(implicit sp: Splitter[R, A, B]) {
    def toPmap(implicit z: Hash[A]): Pmap[A, B]                         = toMap[Pmap]
    def toMap[CC[_, _]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z contraMap sp.split build xs
  }

  implicit class EqViewOps[A](private val xs: View[A])(implicit eqv: Eq[A]) {
    def contains(x: A): Boolean = xs exists (_ === x)
    def distinct: View[A]       = xs.zfoldl[Vec[A]]((res, x) => cond(res.m contains x, res, res :+ x))
    def indexOf(x: A): Index    = xs indexWhere (_ === x)

    def hashFun(): HashFun[A] = eqv match {
      case heq: Hash[A] =>
        val buf = bufferMap[Long, View[A]]()
        xs foreach (x => (heq hash x) |> (h => buf(h) = buf(h) :+ x))
        Fun(buf.result mapValues (_.distinct))
      case _ =>
        Fun const xs
    }
  }
  implicit class ShowableDocOps[A](private val lhs: A)(implicit z: Show[A]) {
    def doc: Doc     = Doc(lhs)
    def show: String = z show lhs
  }
  implicit class HeytingAlgebraOps[A](private val lhs: A)(implicit z: Heyting[A]) {
    def unary_! : A     = z complement lhs
    def &&(rhs: A): A   = z.and(lhs, rhs)
    def ||(rhs: A): A   = z.or(lhs, rhs)
    def ==>(rhs: A): A  = !(lhs && !rhs)
    def meet(rhs: A): A = this && rhs
    def join(rhs: A): A = this || rhs
  }
  implicit class EqOps[A](private val lhs: A)(implicit z: Eq[A]) {
    def ===(rhs: A): Boolean = z.eqv(lhs, rhs)
    def =!=(rhs: A): Boolean = !z.eqv(lhs, rhs)
  }
  implicit class HashOps[A](private val lhs: A)(implicit z: Hash[A]) {
    def hash: Long   = z hash lhs
    def hashInt: Int = hash.toInt
  }
  implicit class OrderOps[A](private val lhs: A)(implicit z: Order[A]) {
    def <(rhs: A): Boolean  = z.cmp(lhs, rhs) eq Cmp.LT
    def <=(rhs: A): Boolean = z.cmp(lhs, rhs) ne Cmp.GT
    def >(rhs: A): Boolean  = z.cmp(lhs, rhs) eq Cmp.GT
    def >=(rhs: A): Boolean = z.cmp(lhs, rhs) ne Cmp.LT
  }
  implicit class SplitterOps[R, A, B](private val lhs: R)(implicit z: Splitter[R, A, B]) {
    def toPair: A -> B = z split lhs
  }
  implicit class Function2Ops[A1, A2, R](private val f: (A1, A2) => R) {
    def toFun1: (A1 -> A2) => R = _ app f
    def andThen[S](g: R => S): (A1, A2) => S = (x, y) => g(f(x, y))
  }
  implicit class Function2SameOps[A, R](private val f: BinTo[A, R]) {
    def on[B](g: B => A): BinTo[B, R] = (x, y) => f(g(x), g(y))
  }
}
