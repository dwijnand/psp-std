package psp
package std

import psp.api._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import scala.Tuple2

/** One import which includes the implicits, one which doesn't.
 *  This choice is mutually exclusive: everything which is in exp is in all.
 */
object exp extends AllExplicit
object all extends AllExplicit with AllImplicit {
  implicit class ArrowAssocRef[A](val self: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }
  implicit class JavaIteratorOps[A](it: jIterator[A]) {
    def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
  }
  implicit class CmpEnumOps(val cmp: Cmp) {
    def |(that: => Cmp): Cmp = if (cmp == Cmp.EQ) that else cmp
  }
  implicit class BuildsTcOps[Elem, To](z: Builds[Elem, To]) {
    def map[Next](f: To => Next): Builds[Elem, Next] = Builds(xs => f(z build xs))
    def scalaBuilder: scmBuilder[Elem, To]           = sciVector.newBuilder[Elem] mapResult (z build _.toEach)
  }
  implicit class EqViewOps[A](val xs: View[A])(implicit eqv: Eq[A]) {
    def contains(x: A): Boolean = xs exists (_ === x)
    def distinct: View[A]       = xs.zfoldl[Vec[A]]((res, x) => cond(res.m contains x, res, res :+ x))
    def indexOf(x: A): Index    = xs indexWhere (_ === x)
    def toExSet: ExSet[A]       = xs.toExSet
  }
  implicit class ViewOpOps[A, B](op: Op[A, B]) {
    def apply[M[X]](xs: M[A])(implicit z: Operable[M]): M[B] = z(xs)(op)
    def ~[C](that: Op[B, C]): Op[A, C]                       = Op.Compose[A, B, C](op, that)
  }
  implicit class ExMapOps[K, V](val lookup: ExMap[K, V]) {
    import lookup._
    type Entry = K -> V

    def keys: View[K]                     = keySet.m
    // def values: View[V]                = keyVector map xs.lookup
    def keySet: ExSet[K]                  = lookup.keys
    def keyVector: Vec[K]                 = keys.toVec
    def entries: Zip[K, V]                = keyVector mapAndZip lookup
    def map[V1](g: V => V1): ExMap[K, V1] = keySet mapWith (f mapOut g)
    def apply(key: K): V                  = lookup(key)
  }
  implicit class ExSetOps[A](val xs: ExSet[A]) {
    private implicit def equiv = xs.equiv

    def mapWith[B](f: Fun[A, B]): ExMap[A, B] = ExMap(xs, f)
    def union(that: ExSet[A]): ExSet[A]       = xs.m ++ that.m toExSet
  }

  /** Extension methods for scala library classes.
   *  We'd like to get away from all such classes,
   *  but scala doesn't allow it.
   */
  implicit class OptionOps[A](val x: Option[A]) extends AnyVal {
    def or(alt: => A): A              = x getOrElse alt
    def toVec: Vec[A]                 = this zfold (x => vec(x))
    def zfold[B: Empty](f: A => B): B = x.fold[B](emptyValue)(f)
    def zget(implicit z: Empty[A]): A = x getOrElse z.empty
    def | (alt: => A): A              = x getOrElse alt
  }
  implicit class TryOps[A](val x: Try[A]) extends AnyVal {
    def | (expr: => A): A = x.toOption | expr
    def fold[B](f: Throwable => B, g: A => B): B = x match {
      case Success(x) => g(x)
      case Failure(t) => f(t)
    }
  }

  implicit class Product2HomoOps[A](val x: A -> A) extends AnyVal {
    def each = new PairAsEach[A](x)
  }
  implicit class Product2HeteroOps[+A, +B](val x: A -> B) extends AnyVal {
    def mapLeft[C](f: A => C): C -> B          = f(fst(x)) -> snd(x)
    def mapRight[C](f: B => C): A -> C         = fst(x) -> f(snd(x))
    def unify[C](f: A => C, g: B => C): C -> C = f(fst(x)) -> g(snd(x))
  }

  // implicit class InputStreamOps(val in: InputStream) extends AnyVal {
  //   def buffered: BufferedInputStream = in match {
  //     case in: BufferedInputStream => in
  //     case _                       => new BufferedInputStream(in)
  //   }
  //   def slurp(): Array[Byte]             = lowlevel.Streams slurp buffered
  //   def slurp(len: Precise): Array[Byte] = lowlevel.Streams.slurp(buffered, len)
  // }
}

abstract class AllExplicit extends ApiValues with StdEq {
  final val ->        = Pair
  final val Array     = scala.Array
  final val Failure   = scala.util.Failure
  final val Nil       = scala.collection.immutable.Nil
  final val NoFile    = jFile("")
  final val NoIndex   = Index.invalid
  final val NoPath    = jPath("")
  final val NoUri     = jUri("")
  final val None      = scala.None
  final val Option    = scala.Option
  final val Some      = scala.Some
  final val Success   = scala.util.Success
  final val Try       = scala.util.Try
  final val sciList   = sci.List
  final val sciMap    = sci.Map
  final val sciSeq    = sci.Seq
  final val sciSet    = sci.Set
  final val sciVector = sci.Vector
  final val scmMap    = scm.Map

  final val ConstantFalse  = (x: scala.Any) => false
  final val ConstantTrue   = (x: scala.Any) => true
  final val ConstantFalse2 = (x: scala.Any, y: scala.Any) => false
  final val ConstantTrue2  = (x: scala.Any, y: scala.Any) => true

  // Type aliases I don't like enough to have in the API.
  type Bag[A]               = ExMap[A, Precise]
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type VdexRange            = Consecutive.Closed[Vdex]
  type IntRange             = Consecutive.Closed[Int]
  type LongRange            = Consecutive.Closed[Long]
  type CharRange            = Consecutive.Closed[Char]
  type Renderer             = Show[Doc]
  type UnbuildsAs[+A, R]    = Unbuilds[R] { type Elem <: A }
  type View2D[+A]           = View[View[A]]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def eqBy[A]    = new EqBy[A]
  def orderBy[A] = new OrderBy[A]
  def showBy[A]  = new ShowBy[A]
  def hashBy[A]  = new HashBy[A]

  def byEquals[A] : Hash[A]              = Eq.Inherited
  def byReference[A <: AnyRef] : Hash[A] = Eq.Reference
  def byString[A] : Hash[A]              = Eq.ToString
  def byShown[A: Show] : Hash[A]         = hashBy[A](x => render(x))(byString)

  def classFilter[A: CTag] : Partial[Any, A]       = Partial(isInstance[A], cast[A])
  def classNameOf(x: Any): String                  = JvmName asScala x.getClass short
  def inheritShow[A] : Show[A]                     = Show.Inherited
  def lformat[A](n: Int): FormatFun                = new FormatFun(cond(n == 0, "%s", new Pstring("%%-%ds") format n))
  def println[A: Show](x: A): Unit                 = scala.Console.out println render(x)
  def render[A](x: A)(implicit z: Show[A]): String = z show x

  def make[R](xs: R): RemakeHelper[R]  = new RemakeHelper[R](xs)
  def make0[R] : MakeHelper0[R]        = new MakeHelper0[R]
  def make1[CC[_]] : MakeHelper1[CC]   = new MakeHelper1[CC]
  def make2[CC[_,_]] : MakeHelper2[CC] = new MakeHelper2[CC]

  def bufferMap[A, B: Empty](): scmMap[A, B]         = scmMap[A, B]() withDefaultValue emptyValue[B]
  def inView[A](mf: Suspended[A]): View[A]           = new LinearView(Each(mf))
  def indexRange(start: Long, end: Long): VdexRange  = longRange(start, end) map Index
  def indexStream: Indexed[Index]                    = LongInterval open 0 map Index
  def intRange(start: Int, end: Int): IntRange       = longRange(start, end) map (_.toInt)
  def intsFrom(start: Int): Consecutive.Open[Int]    = longsFrom(start) map (_.toInt)
  def longRange(start: Long, end: Long): LongRange   = LongInterval.until(start, end) map identity
  def longsFrom(start: Long): Consecutive.Open[Long] = LongInterval open start map identity

  def crossViews[A, B](l: View[A], r: View[B]): Zip[A, B]          = heteroViews(l, r).cross
  def homoViews[A](l: View[A], r: View[A]): Split[A]               = Split(l, r)
  def heteroViews[A, B](l: View[A], r: View[B]): SplitHetero[A, B] = SplitHetero(l, r)

  def zipSplit[AB, A, B](xs: View[AB])(implicit z: Splitter[AB, A, B]): Zip[A, B] = new Zip.ZipSplit(xs)
  def zipPairs[A, B](xs: View[A -> B]): Zip[A, B]                                 = new Zip.ZipPairs(xs)
  def zipViews[A, B](l: View[A], r: View[B]): Zip[A, B]                           = new Zip.ZipViews(l, r)
  def zipWith[A, B](l: View[A], f: A => B): Zip[A, B]                             = new Zip.ZipWith(l, f)

  def arr[A: CTag](xs: A*): Array[A]          = xs.toArray[A]
  def list[A](xs: A*): Plist[A]               = new Conversions(view(xs: _*)) toPlist
  def rel[A: Eq, B](xs: (A->B)*): ExMap[A, B] = ExMap(set[A](xs map fst: _*), Pmap fromScala (xs map tuple toMap))
  def set[A: Eq](xs: A*): ExSet[A]            = new Conversions(view(xs: _*)) toExSet // can't use toSet, doesn't honor Eq[A]
  def vec[A](xs: A*): Vec[A]                  = new Vec[A](xs.toVector)
  def view[A](xs: A*): DirectView[A, Vec[A]]  = new DirectView[A, Vec[A]](vec(xs: _*))
  def zip[A, B](xs: (A->B)*): Zip[A, B]       = zipPairs(view(xs: _*))
}