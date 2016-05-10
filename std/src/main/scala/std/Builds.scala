package psp
package std

/** The organization of the implicit builders is a black art.
  *  It might be better to generate StdBuilds[0-15] and put an implicit in
  *  each one so the prioritization is as unambiguous as scala allows.
  *
  *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
  *  around and it lets us build scala collections at the end of a view with no more code.
  *  Scala maps are built with Tuples even though Product2 should suffice; the types
  *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
  *  The rest of the time one should write it K -> V.
  */
import api._, all._

final class Conversions[A](val xs: View[A]) extends AnyVal with ConversionsMethods[A]

trait ConversionsMethods[A] extends Any {
  def xs: View[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs

  def iterator: scIterator[A]                  = toScalaStream.iterator
  def toArray(implicit z: CTag[A]): Array[A]   = to[Array]
  def toExSet(implicit z: Eq[A]): ExSet[A]     = to[ExSet]
  def toHashSet(implicit z: Hash[A]): ExSet[A] = to[ExSet]
  def toJavaSet: jSet[A]                       = to[jSet]
  def toPlist: Plist[A]                        = to[Plist]
  def toScalaStream: sciStream[A]              = to[sciStream]
  def toScalaSet: sciSet[A]                    = to[sciSet]
  def toScalaVector: sciVector[A]              = to[sciVector]
  def toVec: Vec[A]                            = to[Vec]
}

trait Unbuilds[Repr] extends Any {
  type Elem
  def unbuild(xs: Repr): Foreach[Elem]
}

final class Unbuilder[A, Repr](repr: Repr)(implicit z: UnbuildsAs[A, Repr]) {
  def m: AtomicView[A, Repr] = new IdView(z unbuild repr)
}
object Unbuilds {
  def apply[A, R](f: R => Foreach[A]): UnbuildsAs[A, R] = new Impl[A, R](f)

  final class Impl[A, Repr](val f: Repr => Foreach[A]) extends AnyVal with Unbuilds[Repr] {
    type Elem = A
    def unbuild(xs: Repr): Foreach[A] = f(xs)
  }
}

final class Builds[-Elem, +To](val f: Foreach[Elem] => To) {
  def contraMap[A](g: A => Elem): Builds[A, To]    = new Builds(xs => f(Each(mf => xs foreach (g andThen mf))))
  def map[Next](g: To => Next): Builds[Elem, Next] = new Builds(f andThen g)
  def build(xs: Foreach[Elem]): To                 = f(xs)
  def apply(mf: Suspended[Elem]): To               = build(Foreach(mf, Size.Unknown))
  def scalaBuilder: scmBuilder[Elem, To]           = sciVector.newBuilder[Elem] mapResult (xs => build(xs.m)) // (xs => build(xs.toEach))
}

trait JavaBuilders0 {
  protected def forJava[A, M[X] <: jCollection[X]](empty: M[A]): Builds[A, M[A]] =
    new Builds(xs => doto(empty)(r => xs foreach (r add _)))

  protected def forJavaMap[K, V, M[X, Y] <: jAbstractMap[X, Y]](empty: M[K, V]): Builds[K -> V, M[K, V]] =
    new Builds(xs => doto(empty)(r => xs foreach (x => r.put(fst(x), snd(x)))))

  implicit def buildJavaSet[A] : Builds[A, jSet[A]] = forJava(new jHashSet[A])
}
trait JavaBuilders extends JavaBuilders0 {
  implicit def buildJavaList[A] : Builds[A, jList[A]]          = forJava(new jArrayList[A])
  implicit def buildJavaMap[K, V] : Builds[K -> V, jMap[K, V]] = forJavaMap(new jHashMap[K, V])
}
trait ScalaBuilders0 extends JavaBuilders {
  implicit def forScala[A, That](implicit z: CanBuild[A, That]): Builds[A, That] =
    new Builds(xs => doto(z())(b => xs foreach (b += _)).result)
}
trait ScalaBuilders extends ScalaBuilders0 {
  implicit def forScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] =
    forScala contraMap (x => pair(fst(x), snd(x)))
}
trait PspBuilders0 extends ScalaBuilders {
  implicit def buildPspSet[A : Eq]: Builds[A, ExSet[A]]            = Builds.scalaSet[A] map (xs => new Pset(xs))
  implicit def buildPspMap[K : Eq, V]: Builds[K -> V, ExMap[K, V]] = Builds.javaMap[K, V] map (xs => xs.keySet.byEquals.toExSet mapWith Fun(xs get _))
}
trait PspBuilders1 extends PspBuilders0 {
  implicit def buildPspArray[A : CTag]: Builds[A, Array[A]] = Builds.jvmArray[A]
  implicit def buildPspList[A]: Builds[A, Plist[A]]         = Builds(xs => ll.foldRight[A, Plist[A]](xs, cast(Pnil), _ :: _))
}
trait PspBuilders extends PspBuilders1 {
  implicit def buildPspVec[A] : Builds[A, Vec[A]] = Builds.scalaVector[A] map (xs => new Vec(xs))
}
trait Builders extends PspBuilders
object Builders extends Builders

object Builds {
  def apply[A, R](f: Foreach[A] => R): Builds[A, R] = new Builds(f)

  def javaSet[A] : Builds[A, jSet[A]]               = ?
  def javaList[A] : Builds[A, jList[A]]             = ?
  def javaMap[K, V] : Builds[K -> V, jMap[K, V]]    = ?

  def scalaSet[A] : Builds[A, sciSet[A]]            = ?
  def scalaList[A] : Builds[A, sciList[A]]          = ?
  def scalaMap[K, V] : Builds[K -> V, sciMap[K, V]] = ?
  def scalaVector[A] : Builds[A, sciVector[A]]      = ?

  def pspSet[A : Eq]: Builds[A, ExSet[A]]            = ?
  def pspMap[K : Eq, V]: Builds[K -> V, ExMap[K, V]] = ?
  def pspList[A] : Builds[A, Plist[A]]               = ?
  def pspVec[A] : Builds[A, Vec[A]]                  = ?

  def jvmArray[A : CTag]: Builds[A, Array[A]] = new Builds(xs => doto(Array.newBuilder[A])(_ ++= xs.trav).result)
  def jvmString: Builds[Char, String]         = new Builds(xs => doto(new StringBuilder)(b => xs foreach (c => b append c)) toString)
}

object Each {
  def apply[A](mf: Suspended[A]): Each[A]                      = new Impl[A](Size.Unknown, mf)
  def array[A](xs: Array[A]): WrapArray[A]                     = new WrapArray[A](xs)
  def const[A](elem: A): Each[A]                               = construct(Size.Unknown, mf => while (true) mf(elem))
  def construct[A](size: Size, mf: Suspended[A]): Each[A]      = new Impl[A](size, mf)
  def continually[A](elem: => A): Continually[A]               = new Continually(elem)
  def each[A](xs: Foreach[A]): Each[A]                         = construct(xs.size, xs foreach _)
  def javaMap[A, B](xs: jMap[A, B]): Each[A -> B]              = construct(xs.size, mf => xs.keySet foreach (k => mf((k, xs get k))))
  def scalaMap[A, B](xs: scMap[A, B]): Each[A -> B]            = construct(xs.size, xs foreach _)
  def java[A](xs: jIterable[A]): Each[A]                       = construct(Size.Unknown, xs.iterator foreach _)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]               = new Joined(xs, ys)
  def jvmString(s: String): Direct[Char]                       = new WrapString(s)
  def pair[R, A](x: R)(implicit z: Splitter[R, A, A]): Each[A] = new PairOf(z split x)
  def pure[A](f: Suspended[A]): Each[A]                        = construct(Size.Unknown, f)
  def reversed[A](xs: Direct[A]): Reversed[A]                  = new Reversed(xs)
  def scala[A](xs: sCollection[A]): Each[A]                    = construct(xs.size, xs foreach _)

  final class Impl[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final class Joined[A](xs: Each[A], ys: Each[A]) extends Each[A] {
    def size                                = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = sideEffect(xs foreach f, ys foreach f)
  }
  final class Continually[A](expr: => A) extends Each[A] {
    def size                                = Infinite
    @inline def foreach(f: A => Unit): Unit = while (true) f(expr)
  }
  final case class WrapString(xs: String) extends Direct[Char] {
    def size: Precise                  = Size(xs.length)
    def elemAt(i: Vdex)                = xs charAt i.getInt
    def foreach(f: Char => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final case class WrapArray[A](val xs: Array[_]) extends Direct[A] {
    def size: Precise               = Size(xs.length)
    def elemAt(i: Vdex): A          = cast(xs(i.getInt))
    def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final class Reversed[A](val xs: Direct[A]) extends Direct[A] {
    def size: Precise               = xs.size
    def elemAt(i: Vdex): A          = xs elemAt xs.lastIndex - i.indexValue
    def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final case class PairOf[A](x: A -> A) extends AnyVal with Direct[A] {
    def size = 2
    def elemAt(idx: Vdex): A = idx.indexValue match {
      case 0 => fst(x)
      case 1 => snd(x)
      case _ => noSuchElementException(idx)
    }
    def foreach(f: A => Unit): Unit = sideEffect(f(fst(x)), f(snd(x)))
  }

  def unapplySeq[A](xs: Foreach[A]): Some[scSeq[A]] = Some(xs.seq)
}


/** These classes all put the expected result type up front,
  *  where it can either be inferred from an existing value or
  *  supplied directly.
  */
class RemakeHelper[R](xs: R) {
  def apply[A](f: R => View[A])(implicit z: Builds[A, R]): R = z build f(xs)
}
class MakeHelper0[R] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, R]): R = z build expr
}
class MakeHelper1[CC[_]] {
  def apply[A](expr: => View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build expr
}
class MakeHelper2[CC[_, _]] {
  def apply[K, V](expr: => Zip[K, V])(implicit z: Builds[K -> V, CC[K, V]]): CC[K, V] = z build expr.pairs
}
