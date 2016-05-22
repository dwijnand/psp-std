package psp
package std

import api._, all._

object StdShow extends StdShow

import StdShow._

/** A not very impressive attempt to improve on string
  *  representations.
  */
sealed abstract class Doc {
  override def toString = abort("Doc.toString")
}

object Doc {
  final case object NoDoc extends Doc
  final case class Group(xs: View[Doc])               extends Doc
  final case class Cat(left: Doc, right: Doc)         extends Doc
  final case class Shown[A](value: A, shows: Show[A]) extends Doc
  final case class Literal(value: String)             extends Doc

  def empty: Doc                                    = NoDoc
  def apply[A](x: A)(implicit z: Show[A]): Shown[A] = Shown[A](x, z)
  def apply(s: String): Literal                     = Literal(s)

  implicit class DocOps(private val lhs: Doc) {
    def render(implicit z: Renderer): String = z show lhs
    def ~(rhs: Doc): Doc                     = Doc.Cat(lhs, rhs)
  }
}

object Show {

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
    *  There is however an implicit universal instance in the Unsafe object.
    */
  val Inherited: Show[Any] = apply[Any](s => zcond(s != null, s.toString))

  def apply[A](f: ToString[A]): Show[A] = new Impl[A](f)

  final class Impl[-A](val f: ToString[A]) extends AnyVal with Show[A] { def show(x: A) = f(x) }
}

class FullRenderer(minElements: Precise, maxElements: Precise) extends Renderer {
  private object UnderMax {
    def unapply(xs: View[Doc]) = xs splitAt maxElements.lastIndex match {
      case Split(xs, ys) if ys.isEmpty => Some(xs)
      case _                           => None
    }
  }
  def show(x: Doc): String = x match {
    case Doc.NoDoc               => ""
    case Doc.Cat(l, r)           => show(l) append show(r)
    case Doc.Group(UnderMax(xs)) => xs joinWith ", " surround ("[ ", " ]")
    case Doc.Group(xs)           => xs take minElements joinWith ", " surround ("[ ", ", ... ]")
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
    escaped.format(args.map(_.render): _*)
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
  implicit def showVdex: Show[Vdex]                     = showBy(_.indexValue)
  implicit def showOption[A: Show]: Show[Option[A]]     = Show(_.fold("-")(_.doc.render))
  implicit def showPair[A: Show, B: Show]: Show[A -> B] = Show { case a -> b => a.doc ~ " -> " ~ b render }
  implicit def showOp[A, B]: Show[Op[A, B]]             = Show(op => op[ConstDoc](Doc.empty).render)

  implicit def showFunGrid[A, B](implicit z: Show[B]): Show[View2D.FunGrid[A, B]] = showBy(_.lines.joinLines)

  def joinPair[A: Show, B: Show](xy: A->B)(f: BinOp[String]): String = f(fst(xy).show, snd(xy).show)

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
  implicit def showView[A: Show]: Show[View[A]] = Show(xs => Doc.Group(xs.asDocs).show)
}
trait StdShow1 extends StdShow0 {
  implicit def showPmap[K: Show, V: Show] = showBy[Pmap[K, V]](xs => funGrid(xs.zipped.pairs)(_.show))
  implicit def showPset[A: Show]          = showBy[Pset[A]](_.basis.asShown.inBraces)

  implicit def showJavaMap[K: Show, V: Show]: Show[jMap[K, V]]   = Show(xs => xs.m.pairs map (_ mk_s "=") inBraces)
  implicit def showJavaIterable[A: Show]: Show[jIterable[A]]     = Show(xs => xs.m.asShown.inBrackets)
  implicit def showScalaIterable[A: Show]: Show[scIterable[A]]   = Show(xs => xs.m.asShown.inParens prepend xs.stringPrefix)
  implicit def showEach[A: Show]: Show[Each[A]]                  = Show(_.asDocs.show)
  implicit def showZipped[A1: Show, A2: Show]: Show[Zip[A1, A2]] = showBy[Zip[A1, A2]](_.pairs)
  implicit def showArray[A: Show]: Show[Array[A]]                = showBy[Array[A]](_.toVec)
  implicit def showSplit[A: Show]: Show[Split[A]]                = showBy[Split[A]](x => "Split(".lit ~ x.leftView ~ ", " ~ x.rightView ~ ")")
}
