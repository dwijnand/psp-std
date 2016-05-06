package psp
package std

import psp.api._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

/** One import which includes the implicits, one which doesn't.
 *  This choice is mutually exclusive: everything which is in exp is in all.
 */
object exp extends AllExplicit
object all extends AllExplicit with AllImplicit {
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
  // implicit class InputStreamOps(val in: InputStream) extends AnyVal {
  //   def buffered: BufferedInputStream = in match {
  //     case in: BufferedInputStream => in
  //     case _                       => new BufferedInputStream(in)
  //   }
  //   def slurp(): Array[Byte]             = lowlevel.Streams slurp buffered
  //   def slurp(len: Precise): Array[Byte] = lowlevel.Streams.slurp(buffered, len)
  // }
}

abstract class AllExplicit extends ApiValues {
  final val ->            = Pair
  final val Array         = scala.Array
  final val ConstantFalse = (x: scala.Any) => false
  final val ConstantTrue  = (x: scala.Any) => true
  final val Failure       = scala.util.Failure
  final val Nil           = scala.collection.immutable.Nil
  final val NoFile        = jFile("")
  final val NoIndex       = Index.invalid
  final val NoPath        = jPath("")
  final val NoUri         = jUri("")
  final val None          = scala.None
  final val Option        = scala.Option
  final val Some          = scala.Some
  final val Success       = scala.util.Success
  final val Try           = scala.util.Try
  final val sciList       = sci.List
  final val sciMap        = sci.Map
  final val sciSeq        = sci.Seq
  final val sciSet        = sci.Set
  final val sciVector     = sci.Vector
  final val scmMap        = scm.Map

  // Type aliases I don't like enough to have in the API.
  type Bag[A]               = ExMap[A, Precise]
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type VdexRange            = Consecutive[Vdex]
  type IntRange             = Consecutive[Int]
  type LongRange            = Consecutive[Long]
  type Renderer             = Show[Doc]
  type UnbuildsAs[+A, R]    = Unbuilds[R] { type Elem <: A }
  type View2D[+A]           = View[View[A]]

  // Helpers for inference when calling 'on' on contravariant type classes.
  def eqBy[A]    = new EqBy[A]
  def orderBy[A] = new OrderBy[A]
  def showBy[A]  = new ShowBy[A]
  def hashBy[A]  = new HashBy[A]

  def render[A](x: A)(implicit z: Show[A]): String = z show x
  def inheritShow[A] : Show[A]                     = Show.Inherited

  def byEquals[A] : Hash[A]              = Eq.Inherited
  def byReference[A <: AnyRef] : Hash[A] = Eq.Reference
  def byString[A] : Hash[A]              = Eq.ToString
  def byShown[A: Show] : Hash[A]         = hashBy[A](x => render(x))(byString)

  def inView[A](mf: Suspended[A]): View[A] = new LinearView(Each(mf))

  private def stdout                    = scala.Console.out
  // private def putOut(msg: Any): Unit = sideEffect(stdout print msg, stdout.flush())
  private def echoOut(msg: Any): Unit   = stdout println msg

  // def print[A: Show](x: A): Unit   = putOut(render(x))
  def println[A: Show](x: A): Unit = echoOut(render(x))
  // def anyprintln(x: Any): Unit     = echoOut(aops(x).any_s)

  def classNameOf(x: Any): String                 = JvmName asScala x.getClass short
  def classFilter[A: CTag] : Partial[Any, A]      = Partial(isInstance[A], cast[A])
  def bufferMap[A, B: Empty](): scmMap[A, B]      = scmMap[A, B]() withDefaultValue emptyValue[B]
  def indexRange(start: Int, end: Int): VdexRange = Consecutive.until(start, end) map (x => Index(x))
  def lformat[A](n: Int): FormatFun               = new FormatFun(cond(n == 0, "%s", new Pstring("%%-%ds") format n))

  def make[R](xs: R): RemakeHelper[R]  = new RemakeHelper[R](xs)
  def make0[R] : MakeHelper[R]         = new MakeHelper[R]
  def make1[CC[_]] : MakeHelper1[CC]   = new MakeHelper1[CC]
  // def make2[CC[_,_]] : MakeHelper2[CC] = new MakeHelper2[CC]

  def arr[A: CTag](xs: A*): Array[A]          = xs.toArray[A]
  def list[A](xs: A*): Plist[A]               = new Conversions(view(xs: _*)) toPlist
  def rel[K: Eq, V](xs: (K->V)*): ExMap[K, V] = ExMap fromScala (xs map tuple toMap)
  def set[A: Eq](xs: A*): ExSet[A]            = new Conversions(view(xs: _*)) toExSet
  def vec[A](xs: A*): Vec[A]                  = new Vec[A](xs.toVector)
  def view[A](xs: A*): DirectView[A, Vec[A]]  = new DirectView[A, Vec[A]](vec(xs: _*))
  def zip[A, B](xs: (A->B)*): ZipView[A, B]   = Zip zip1 view(xs: _*)

  object indices {
    def all: Indexed[Index]                  = Indexed(_.toIndex)
    def from(start: BigInt): Indexed[BigInt] = Indexed(start + _.indexValue)
    def from(start: Long): Indexed[Long]     = Indexed(start + _.indexValue)
    def from(start: Int): Indexed[Int]       = Indexed(start + _.getInt)
  }
}
