package psp
package std

import java.io.BufferedInputStream

object Unsafe {
  implicit def promoteIndex(x: scala.Long) = Index(x)
  implicit def inheritedShow[A]: Show[A]   = Show.Inherited
}

/** One import which includes the implicits, one which doesn't.
  *  This choice is mutually exclusive: everything which is in exp is in all.
  */
object exp extends AllExplicit

object all extends AllExplicit with AllImplicit {
  implicit class PspStringContextOps(val stringContext: StringContext) extends Interpolators
  implicit class PspAnyOps[A](private val x: A) extends AnyVal {
    /** Call this on any value. Produce a view.
     *  If a value of the same type as the original can be
     *  built from the view, it will be. Otherwise it
     *  won't compile.
     */
    def o[B](f: A => View[B])(implicit z: Makes[B, A]): A = z make f(x)

    def any_s: String                     = s"$x"
    def id_## : Int                       = java.lang.System.identityHashCode(x)
    def id_==(y: Any): Boolean            = cast[AnyRef](x) eq cast[AnyRef](y)
    def matchIf[B: Empty](pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else emptyValue

    @inline def ->[B](y: B): Tuple2[A, B] = Tuple2(x, y) // The tupling arrow.
    @inline def |>[B](f: A => B): B       = f(x)         // The famed forward pipe.
  }
  implicit class PspOptionOps[A](private val x: Option[A]) extends AnyVal {
    def view: View[A]                 = x.seq.m
    def or(alt: => A): A              = x getOrElse alt
    def toVec: Vec[A]                 = this zfold (x => vec(x))
    def zfold[B: Empty](f: A => B): B = x.fold[B](emptyValue)(f)
    def zget(implicit z: Empty[A]): A = x getOrElse z.empty
    def |(alt: => A): A               = x getOrElse alt
  }
  implicit class PspTryOps[A](private val x: Try[A]) extends AnyVal {
    def |(expr: => A): A = x.toOption | expr
    def fold[B](f: Throwable => B, g: A => B): B = x match {
      case Success(x) => g(x)
      case Failure(t) => f(t)
    }
  }
  implicit class PspInputStreamOps(private val in: InputStream) extends AnyVal {
    def buffered: BufferedInputStream = in match {
      case in: BufferedInputStream => in
      case _                       => new BufferedInputStream(in)
    }
    def slurp(): Array[Byte]             = ll.Streams slurp buffered
    def slurp(len: Precise): Array[Byte] = ll.Streams.slurp(buffered, len)
  }

  implicit class PspJavaIteratorOps[A](private val it: jIterator[A]) {
    def toScala: scIterator[A] = new scIterator[A] {
      def hasNext: Bool = it.hasNext
      def next(): A     = it.next()
    }
    def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
  }
  implicit class PspPartialFunctionOps[A, B](pf: A ?=> B) {
    def contains(x: A): Bool                  = pf isDefinedAt x
    def applyOr(x: A, alt: => B): B           = cond(contains(x), pf(x), alt)
    def zapply(x: A)(implicit z: Empty[B]): B = applyOr(x, z.empty)
    def toPartial: Fun.Partial[A, B]          = Fun partial pf
  }

  implicit class PspFunction1Ops[A, B](private val f: A => B) {
    def labeled(label: => String): A => B = new LabeledFunction(f, () => label)
  }
  implicit class PspFunction2Ops[A1, A2, R](private val f: (A1, A2) => R) {
    def toFun1: (A1 -> A2) => R = _ app f
    def andThen[S](g: R => S): (A1, A2) => S = (x, y) => g(f(x, y))
  }
  implicit class PspFunction2OpsSame[A, R](private val f: BinTo[A, R]) {
    def on[B](g: B => A): BinTo[B, R] = (x, y) => f(g(x), g(y))
  }
  implicit class PspCharOps(private val ch: Char) {
    def to_s: String                      = ch.toString
    def r: Regex                          = Regex(ch)
    def takeNext(len: Precise): CharRange = ch.toLong takeNext len map (_.toChar)
    def to(end: Char): CharRange          = ch.toLong to end.toLong map (_.toChar)
    def until(end: Char): CharRange       = ch.toLong until end.toLong map (_.toChar)
  }
  implicit class PspIntOps(private val self: Int) {
    def to(end: Int): IntRange    = self.toLong to end.toLong map (_.toInt)
    def until(end: Int): IntRange = self.toLong until end.toLong map (_.toInt)
  }
  implicit class PspLongOps(private val n: Long) {
    def andUp: OpenRange[Long] = Interval(n) map identity
    def index: Index           = Index(n)
    def nth: Nth               = Nth(n)
    def size: Precise          = Precise(n)

    def takeNext(len: Precise): LongRange = Interval(n, len) map identity
    def to(end: Long): LongRange          = takeNext(Size(end - n + 1))
    def until(end: Long): LongRange       = takeNext(Size(end - n))

    def sizeTo(end: Long): SizeRange              = to(end) map Precise
    def indexUntil(end: Long): ClosedRange[Index] = until(end) map Index
    def nthTo(end: Long): ClosedRange[Nth]        = to(end) map Nth
  }
  implicit class PspArrayByteOps(private val xs: Array[Byte]) {
    def utf8Chars: Array[Char] = scala.io.Codec fromUTF8 xs
    def utf8String: String     = new String(utf8Chars)
  }
  implicit class PspArrayCharOps(private val xs: Array[Char]) {
    def utf8Bytes: Array[Byte] = scala.io.Codec.toUTF8(xs, 0, xs.length)
    def utf8String: String     = new String(xs)
  }
  implicit class PspPrimitiveArrayOps[A >: Primitive <: AnyVal](private val xs: Array[A]) {
    def inPlace: InPlacePrimitive[A] = new InPlacePrimitive(xs)
  }
  implicit class PspReferenceArrayOps[A <: AnyRef](private val xs: Array[A]) {
    def inPlace: InPlaceReference[A] = new InPlaceReference(xs)
  }
  implicit class PspArrayOps[A](private val xs: Array[A]) {
    private def arraycopy[A](src: Array[A], srcPos: Int, dst: Array[A], dstPos: Int, len: Int): Unit =
      java.lang.System.arraycopy(src, srcPos, dst, dstPos, len)

    def ++(that: Array[A])(implicit z: CTag[A]): Array[A] = {
      val arr = newArray[A](xs.length + that.length)
      arraycopy(xs, 0, arr, 0, xs.length)
      arraycopy(that, 0, arr, xs.length, that.length)
      arr
    }
  }

  /** Views of specific type.
   */
  implicit class View2DOps[A](xss: View2D[A]) {
    def column(vdex: Vdex) = View(xss) flatMap (_ sliceIndex vdex)
    def transpose          = View(openIndices map column)
    def flatten            = View(xss) flatMap (x => x)
    def mmap[B](f: A => B) = View(xss) map (_ map f)

    def grid_s(implicit z: Show[A]): String = {
      val width = xss.mmap(_.pp.length).flatten.max
      val fmt   = lformat(width)
      val yss   = xss mmap (x => fmt(x.pp))
      val lines = View(yss) map (_ joinWith " ")

      lines.joinLines mapLines (_.trim)
    }
  }
  implicit class ViewStringOps(private val xs: View[String]) {
    def trimmed: View[String]         = xs map (_.trim)
    def joinWith(ch: Char): String    = xs joinWith ch.to_s
    def joinWith(sep: String): String = xs zreducel (_ + sep + _)
    def join: String                  = xs joinWith ""
    def joinWords: String             = trimmed joinWith " "
    def joinLines: String             = xs map (_ stripSuffix "\\s+".r) joinWith "\n"
    def joinList: String              = trimmed joinWith ", "
    def inParens: String              = joinList surround "(" -> ")"
    def inBrackets: String            = joinList surround "[" -> "]"
    def inBraces: String              = joinList surround "{" -> "}"
  }
  implicit class ViewHasIsProductOps[R, A, B](private val xs: View[R])(implicit sp: IsProduct[R, A, B]) {
    def toPmap(implicit ez: Eq[A], hz: Hash[A]): Pmap[A, B]            = toMap[Pmap]
    def toMap[CC[_, _]](implicit z: Makes[A -> B, CC[A, B]]): CC[A, B] = z contraMap sp.split make xs
  }
  implicit class ViewHasShowOps[A](private val xs: View[A])(implicit z: Show[A]) {
    def mkDoc(sep: Doc): Doc          = xs.asDocs zreducel (_ ~ sep ~ _)
    def joinWith(sep: String): String = mkDoc(sep.lit).pp
    def joinString: String            = joinWith("")
  }

  implicit def viewHasEqOps[A : Eq, R](xs: RView[A, R]): xs.EqOps = new xs.EqOps()

  /** Other psp classes.
   */

  implicit class Pair2DOps[A, B](private val x: Pair2D[A, B]) {
    def transpose: (A->A) -> (B->B) = pair(
      fst(fst(x)) -> fst(snd(x)),
      snd(fst(x)) -> snd(snd(x))
    )
  }
  implicit class PspDocOps(private val lhs: Doc) {
    import Doc._
    def pp: String         = pp"$lhs"
    def <>(rhs: Doc): Doc  = lhs ~ rhs
    def <+>(rhs: Doc): Doc = lhs ~ SP ~ rhs
    def ~(rhs: Doc): Doc   = Cat(lhs, rhs)
  }
  implicit class PspFunOps[A, B](private val f: Fun[A, B]) extends AnyVal {
    type This = Fun[A, B]

    import Fun._

    def fn: A => B  = x => f(x)
    def pf: A ?=> B = f.toPartial

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

  /** Ops directly on type class instances.
   */
  implicit class MakesClassOps[A, R](private val z: Makes[A, R]) {
    def apply(xs: A*): R                     = z make Makes.fromArgs(xs: _*)
    def contraMap[Z](g: Z => A): Makes[Z, R] = Makes(xs => z make (xs map g))
    def map[S](g: R => S): Makes[A, S]       = Makes(g compose z.make)
    def scalaBuilder()                       = scala.Vector.newBuilder[A] mapResult (z make _.m)
  }
  implicit class OrderClassOps[A](private val r: Order[A]) {
    def flip: Order[A] = Order((x, y) => r.less(y, x))
    def comparator: Comparator[A] = new Comparator[A] {
      def compare(x: A, y: A): Int = if (r.less(x, y)) -1 else if (r.less(y, x)) 1 else 0
    }
  }
  implicit class BoolAlgebraClassOps[A](private val z: BoolAlgebra[A]) {
    def xmap[B](f: A => B, g: B => A): BoolAlgebra[B] = new BoolAlgebra[B] {
      def and(x: B, y: B): B  = f(z.and(g(x), g(y)))
      def or(x: B, y: B): B   = f(z.or(g(x), g(y)))
      def complement(x: B): B = f(z.complement(g(x)))
      def zero: B             = f(z.zero)
      def one: B              = f(z.one)
    }
  }

  /** "Has" implicit methods on values for which a type class is present.
   */
  implicit class HasIsProductOpsSame[A, R](private val x: R)(implicit z: IsProduct[R, A, A]) {
    def map2[B](f: A => B): B -> B = z split x mapEach (f, f)
    def each: Direct[A]            = elems(x._1, x._2)
  }
  implicit class HasWalksOps[A, R](val repr: R)(implicit z: Walks[A, R]) {
    def as[S] : RView[A, S] = View(repr)
    def m: RView[A, R]      = View(repr)
  }
  implicit class HasShowOps[A](private val lhs: A)(implicit z: Show[A]) {
    def doc: Doc     = Doc(lhs)
    def show: String = z show lhs
    def pp: String   = pp"$doc"
  }
  implicit class HasHeytingOps[A](private val lhs: A)(implicit z: Heyting[A]) {
    def unary_! : A     = z complement lhs
    def &&(rhs: A): A   = z.and(lhs, rhs)
    def ||(rhs: A): A   = z.or(lhs, rhs)
    def ==>(rhs: A): A  = !(lhs && !rhs)
    def meet(rhs: A): A = this && rhs
    def join(rhs: A): A = this || rhs
  }
  implicit class HasEqNegationOps[A](private val lhs: A) {
    def =!=[R](rhs: A)(implicit z: MEq[A, R], ba: BoolAlgebra[R]): R = !z.eqv(lhs, rhs)
  }
  implicit class HasMEqOps[A](private val lhs: A) {
    def ===[R](rhs: A)(implicit z: MEq[A, R]): R = z.eqv(lhs, rhs)
  }
  implicit class HasHashOps[A](private val lhs: A)(implicit z: Hash[A]) {
    def hash: Long   = z hash lhs
    def hashInt: Int = hash.toInt
  }
  implicit class HasOrderOps[A](private val lhs: A)(implicit z: Order[A]) {
    def <(rhs: A): Boolean = z.less(lhs, rhs)
    def >(rhs: A): Boolean = z.less(rhs, lhs)

    def <=(rhs: A)(implicit y: Eq[A]): Boolean = (lhs < rhs) || (lhs === rhs)
    def >=(rhs: A)(implicit y: Eq[A]): Boolean = (lhs > rhs) || (lhs === rhs)
  }
  implicit class HasIsProductOps[R, A, B](private val x: R)(implicit z: IsProduct[R, A, B]) {
    def _1: A = fst(z split x)
    def _2: B = snd(z split x)
    def mkDoc(sep: Doc)(implicit za: Show[A], zb: Show[B]): Doc      = _1.doc ~ sep <> _2
    def mk_s(sep: String)(implicit za: Show[A], zb: Show[B]): String = mkDoc(sep.lit).pp

    def appLeft[C](f: A => C): C                 = f(_1)
    def appRight[C](f: B => C): C                = f(_2)
    def app[C](f: (A, B) => C): C                = f(_1, _2)
    def mapEach[C](f: A => C, g: B => C): C -> C = f(_1) -> g(_2)
    def mapLeft[C](f: A => C): C -> B            = f(_1) -> _2
    def mapRight[C](f: B => C): A -> C           = _1 -> f(_2)
    def apply[C](f: (A, B) => C): C              = f(_1, _2)
    def toPair: A->B                             = _1 -> _2
  }
}
