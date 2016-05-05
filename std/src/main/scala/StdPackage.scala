package psp
package std

import psp.api._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

abstract class AllExplicit extends PspApi {
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
  final val SafeLong      = spire.math.SafeLong
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
  // def lexicalOrder: Order[String]                  = Order.fromInt(_ compareTo _)
  def inheritShow[A] : Show[A]                     = Show.Inherited
  def inheritEq[A] : Hash[A]                       = Eq.Inherited
  def referenceEq[A <: AnyRef] : Hash[A]           = Eq.Reference
  def stringEq[A] : Hash[A]                        = Eq.ToString
  // def shownEq[A: Show] : Hash[A]                   = hashBy[A](x => render(x))(Eq.ToString)

  def inView[A](mf: Suspended[A]): View[A] = new LinearView(Each(mf))

  private def stdout                    = scala.Console.out
  // private def putOut(msg: Any): Unit = sideEffect(stdout print msg, stdout.flush())
  private def echoOut(msg: Any): Unit   = stdout println msg

  // def print[A: Show](x: A): Unit   = putOut(render(x))
  def println[A: Show](x: A): Unit = echoOut(render(x))
  // def anyprintln(x: Any): Unit     = echoOut(aops(x).any_s)

  def applyIfNonEmpty[A](x: A)(f: A => A)(implicit e: Eq[A], z: Empty[A]): A =
    if (e.eqv(x, z.empty)) x else f(x)

  def ??? : Nothing = throw new scala.NotImplementedError

  def classNameOf(x: Any): String                 = JvmName asScala x.getClass short
  def classFilter[A: CTag] : Partial[Any, A]      = Partial(isInstance[A], cast[A])
  def bufferMap[A, B: Empty](): scmMap[A, B]      = scmMap[A, B]() withDefaultValue emptyValue[B]
  def indexRange(start: Int, end: Int): VdexRange = Consecutive.until(start, end) map (x => Index(x))
  def lformat[A](n: Int): FormatFun               = new FormatFun(cond(n == 0, "%s", new Pstring("%%-%ds") format n))
  def noNull[A](value: A, orElse: => A): A        = if (value == null) orElse else value

  def make[R](xs: R): RemakeHelper[R]  = new RemakeHelper[R](xs)
  def make0[R] : MakeHelper[R]         = new MakeHelper[R]
  def make1[CC[_]] : MakeHelper1[CC]   = new MakeHelper1[CC]
  // def make2[CC[_,_]] : MakeHelper2[CC] = new MakeHelper2[CC]

  def arr[A: CTag](xs: A*): Array[A]                = xs.toArray[A]
  def list[A](xs: A*): Plist[A]                     = new Conversions(view(xs: _*)) toPlist
  def rel[K: Eq, V](xs: (K->V)*): ExMap[K, V]       = ExMap fromScala (xs map tuple toMap)
  def set[A: Eq](xs: A*): ExSet[A]                  = new Conversions(view(xs: _*)) toExSet
  def vec[A](xs: A*): Vec[A]                        = new Vec[A](xs.toVector)
  def view[A](xs: A*): DirectView[A, Vec[A]]        = new DirectView[A, Vec[A]](vec(xs: _*))
  def zip[A, B](xs: (A->B)*): ZipView[A, B]         = Zip zip1 view(xs: _*)

  object indices {
    def all: Indexed[Index]                      = Indexed(_.toIndex)
    def from(start: SafeLong): Indexed[SafeLong] = Indexed(start + _.indexValue)
    def from(start: BigInt): Indexed[BigInt]     = Indexed(start + _.indexValue)
    def from(start: Long): Indexed[Long]         = Indexed(start + _.indexValue)
    def from(start: Int): Indexed[Int]           = Indexed(start + _.getInt)
  }
}
