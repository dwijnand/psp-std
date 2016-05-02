package psp
package std

import api._, all._, StdShow._, StdEq._

class FullRenderer extends Renderer {
  def minElements: Precise = 3
  def maxElements: Precise = 10

  def showEach(xs: Each[Doc]): String = "[ " + (xs.m splitAt maxElements.lastIndex match {
    case Split(xs, ys) if ys.isEmpty => xs map show mk_s ", "
    case Split(xs, _)                => (xs take minElements map show mk_s ", ") + ", ..."
  }) + " ]"

  def show(x: Doc): String = x match {
    case Doc.NoDoc           => ""
    case Doc.Cat(l, r)       => show(l) + show(r)
    case Doc.Group(xs)       => showEach(xs)
    case Doc.Shown(value, z) => z show value
    case Doc.Literal(s)      => s
  }
}

final class ShowInterpolator(val stringContext: StringContext) extends AnyVal {
  def escapedParts    = stringContext.parts.toList map (_.processEscapes)
  def escaped: String = escapedParts.join_s

  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Doc.
   */
  def show(args: Doc*): String = StringContext(escapedParts: _*).raw(args.map(_.render): _*)
  def pp(args: Doc*): String   = show(args: _*)

  /** Can't see any way to reuse the standard (type-safe) f-interpolator, will
   *  apparently have to reimplement it entirely.
   */
  def fshow(args: Doc*): String = escaped.format(args.map(_.render): _*)

  final def sm(args: Doc*): String = {
    def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
    def stripTrailingPart(s: String): String = {
      val index        = s indexWhere isLineBreak
      val pre: String  = s take index.sizeExcluding force
      val post: String = s drop index.sizeExcluding force;
      pre append post.stripMargin
    }
    val stripped = applyIfNonEmpty(escapedParts)(xs => xs.head.stripMargin :: (xs.tail map stripTrailingPart))
    (new StringContext(stripped: _*).raw(args: _*)).trim
  }
}

object Show {
  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   *  There is however an implicit universal instance in the Unsafe object.
   */
  val Inherited: Show[Any] = apply[Any](s => noNull(s, "").toString)

  def apply[A](f: ToString[A]): Show[A] = new Impl[A](f)

  final class Impl[-A](val f: ToString[A]) extends AnyVal with Show[A] { def show(x: A) = f(x) }
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowInstances extends ShowEach {
  implicit def showBoolean: Show[Boolean]     = inheritShow
  implicit def showChar: Show[Char]           = inheritShow
  // implicit def showDouble: Show[Double]       = inheritShow
  implicit def showInt: Show[Int]             = inheritShow
  implicit def showLong: Show[Long]           = inheritShow
  // implicit def showPath: Show[jPath]          = inheritShow
  implicit def showString: Show[String]       = inheritShow
  // implicit def showThrowable: Show[Throwable] = inheritShow

  implicit def showClass: Show[jClass]                                     = Show(_.shortName)
  implicit def showDirect: Show[ShowDirect]                                = Show(_.to_s)
  implicit def showIndex: Show[Vdex]                                       = showBy(_.indexValue)
  implicit def showOption[A: Show] : Show[Option[A]]                       = Show(_.fold("-")(_.render))
  implicit def showPair[A: Show, B: Show] : Show[A -> B]                   = Show(x => x._1 ~ " -> " ~ x._2 render)
  // implicit def showStackTraceElement: Show[java.lang.StackTraceElement] = Show(x => s"\tat$x\n")

  implicit def showSize: Show[Size] = Show[Size] {
    case Finite(size)          => pp"$size"
    case Bounded(lo, Infinite) => pp"$lo+"
    case Bounded(lo, hi)       => pp"[$lo,$hi]"
    case Infinite              => "<inf>"
  }
}
trait ShowEach0 {
  implicit def showView[A: Show](implicit z: FullRenderer): Show[View[A]] = showBy[View[A]](z showEach _.toEach.map(_.doc))
}
trait ShowEach1 extends ShowEach0 {
  implicit def showEach[A: Show](implicit z: FullRenderer): Show[Each[A]] = Show(xs => z showEach (xs map (_.doc)))
}
trait ShowEach extends ShowEach1 {
  case class FunctionGrid[A, B](values: View[A], functions: View[A => B]) {
    def rows: View2D[B]   = values map (v => functions map (f => f(v)))
    def columns:View2D[B] = functions map (f => values map (v => f(v)))

    def renderLines(implicit z: Show[B]): Vec[String]               = {
      val widths    = columns map (_ map z.show map (_.length) max)
      val formatFns = widths map lformat

      rows map (formatFns zip _ map (_ apply _) mk_s ' ')
    }
    def render(implicit z: Show[B]): String = renderLines.joinLines
  }

  private def tabular[A](xs: View[A])(columns: ToString[A]*): String =
    if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.toVec, columns.m).render(inheritShow) else ""

  implicit def showExMap[K: Show, V: Show] : Show[ExMap[K, V]]        = Show(xs => tabular(xs.entries.pairs)(_.render))
  implicit def showZipped[A1: Show, A2: Show] : Show[ZipView[A1, A2]] = showBy[ZipView[A1, A2]](_.pairs)
  implicit def showArray[A: Show] : Show[Array[A]]                    = showBy[Array[A]](_.toVec)
  // implicit def showJavaEnum[A <: jEnum[A]] : Show[jEnum[A]]           = inheritShow
}
