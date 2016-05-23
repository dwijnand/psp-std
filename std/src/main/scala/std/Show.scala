package psp
package std

import api._, all._, Show.by

object StdShow extends StdShow

import StdShow._

/** A not very impressive attempt to improve on string
  *  representations.
  */
sealed abstract class Doc {
  override def toString = abort("Doc.toString")
}

object Doc {
  val SP = " ".lit

  final case object NoDoc extends Doc
  final case class Group(xs: View[Doc])               extends Doc
  final case class Cat(left: Doc, right: Doc)         extends Doc
  final case class Shown[A](value: A, shows: Show[A]) extends Doc
  final case class Literal(value: String)             extends Doc

  def empty: Doc                                    = NoDoc
  def apply[A](x: A)(implicit z: Show[A]): Shown[A] = Shown[A](x, z)
  def apply(s: String): Literal                     = Literal(s)
}

object Show {

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
    *  There is however an implicit universal instance in the Unsafe object.
    */
  val Inherited: Show[Any] = apply[Any](s => zcond(s != null, s.toString))

  def apply[A](f: ToString[A]): Show[A] = new Impl[A](f)
  def by[A]: ShowBy[A]                  = new ShowBy[A]

  final class Impl[-A](val f: ToString[A]) extends AnyVal with Show[A] {
    def show(x: A) = f(x)
  }
  final class ShowBy[A] {
    def apply[B](f: A => B)(implicit z: Show[B]): Show[A] = z on f
  }
}

class FullRenderer(elemRange: SizeRange) extends Renderer {
  def minElements = elemRange.head
  def maxElements = elemRange.last

  private object UnderMax {
    def unapply(xs: View[Doc]) = xs splitAfter maxElements match {
      case SplitView(xs, EmptyView()) => Some(xs)
      case _                          => None
    }
  }
  def show(x: Doc): String = x match {
    case Doc.NoDoc               => ""
    case Doc.Cat(l, r)           => show(l) append show(r)
    case Doc.Group(UnderMax(xs)) => xs map (_.pp) joinWith ", " surround ("[ ", " ]")
    case Doc.Group(xs)           => xs take minElements map (_.pp) joinWith ", " surround ("[ ", ", ... ]")
    case Doc.Shown(value, z)     => z show value
    case Doc.Literal(s)          => s
  }
}

case class ShowInterpolator(val stringContext: StringContext) {
  def escapedParts: View[String] = stringContext.parts.toVec map (_.processEscapes)
  def escaped: String            = escapedParts.join

  def strippedParts: View[String] = escapedParts map (_ mapLines (_.stripMargin))
  def stripped: String            = strippedParts.join

  /** TODO. See
   *  https://github.com/scala/scala/blob/2.12.x/src/compiler/scala/tools/reflect/FormatInterpolator.scala
   */
  val FormatSpec = """%(?:(\d+)\$)?([-#+ 0,(\<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r

  /** There's one more escaped part than argument, so
   *  to collate them we tack an empty Doc onto the arg list.
   */
  def doc(args: Doc*): Doc = Split(escapedParts.asDocs, args :+ Doc.empty).collate reducel (_ ~ _)

  /** Can't see any way to call the standard (type-safe) f-interpolator, will
    *  apparently have to reimplement it entirely.
    */
  def fdoc(args: Doc*): Doc = {
    // val fms = FormatSpec findAll escaped
    escaped.format(args.map(_.pp): _*)
  }

  def sdoc(args: Doc*): Doc = new StringContext(strippedParts.seq: _*).raw(args: _*).trim
}

/** An incomplete selection of show compositors.
  *  Not printing the way scala does.
  */
trait StdShow extends StdShow1 {
  implicit def showBoolean: Show[Boolean]     = Show.Inherited
  implicit def showChar: Show[Char]           = Show.Inherited
  implicit def showDouble: Show[Double]       = Show.Inherited
  implicit def showInt: Show[Int]             = Show.Inherited
  implicit def showLong: Show[Long]           = Show.Inherited
  implicit def showString: Show[String]       = Show.Inherited
  implicit def showThrowable: Show[Throwable] = Show.Inherited

  implicit def showClass: Show[jClass]                  = Show(JvmName asScala _ short)
  implicit def showDirect: Show[ShowDirect]             = Show(_.to_s)
  implicit def showVdex: Show[Vdex]                     = by(_.indexValue)
  implicit def showOption[A: Show]: Show[Option[A]]     = Show(_.fold("-")(_.pp))
  implicit def showPair[A: Show, B: Show]: Show[A -> B] = Show(_ mkDoc " -> " pp)
  implicit def showOp[A, B]: Show[Op[A, B]]             = Show(op => op[ConstDoc]("".lit).pp)

  implicit def showView2DLive[A, B](implicit z: Show[B]): Show[View2D.Live[A, B]] = by(_.lines.joinLines)

  implicit def showSize: Show[Size] = Show[Size] {
    case Precise(size)         => pp"$size"
    case Bounded(lo, Infinite) => pp"$lo+"
    case Bounded(lo, hi)       => pp"[$lo,$hi]"
    case Infinite              =>   "<inf>"
  }

  implicit def showInterval: Show[Interval] = Show {
    case Interval(start, Infinite) => pp"[$start..)"
    case Interval(_, Size.Zero)    => "[0,0)"
    case Interval(start, Size.One) => pp"[$start]"
    case Interval(start, end)      => pp"[$start..${ end - 1 }]"
  }

  implicit def showRange[A: Show, CC[X] <: Consecutive[X]] : Show[CC[_ <: A]] = Show {
    case Consecutive(s, None)    => pp"[$s..)"
    case Consecutive(s, Some(e)) => pp"[$s..$e]"
    case _                       => "[]"
  }
}
trait StdShow0 {
  implicit def showView[A: Show]: Show[View[A]] = Show(xs => Doc.Group(xs.asDocs).pp)
}
trait StdShow1 extends StdShow0 {
  implicit def showPmap[K: Show, V: Show] = by[Pmap[K, V]](_.pairs mapLive (_.pp)) //  xs => funGrid(xs.pairs)(_.show))
  implicit def showPset[A: Show]          = by[Pset[A]](_.basis.asShown.inBraces)

  implicit def showJavaMap[K: Show, V: Show]: Show[jMap[K, V]]   = Show(_.m.pairs map (_ mkDoc "=" pp) inBraces)
  implicit def showJavaIterable[A: Show]: Show[jIterable[A]]     = Show(_.m.asShown.inBrackets)
  implicit def showScalaIterable[A: Show]: Show[scIterable[A]]   = Show(xs => xs.m.asShown.inParens prepend xs.stringPrefix)
  implicit def showEach[A: Show]: Show[Each[A]]                  = by(_.m)
  implicit def showZipped[A1: Show, A2: Show]: Show[Zip[A1, A2]] = by(_.pairs)
  implicit def showArray[A: Show]: Show[Array[A]]                = by(_.m)
  implicit def showSplit[A: Show]: Show[Split[A]]                = Show(_ app (_.doc <+> "/" <+> _ pp))
}
