package psp
package std

import api._, all._

/** "Native" psp collections.
 */

sealed abstract class Plist[A] extends Each[A] {
  def head: A
  def tail: Plist[A]
  def isEmpty = this eq Pnil
  def size    = if (isEmpty) Size.Zero else Size.NonEmpty

  def ::(head: A): Plist[A] = Pcons(head, this)
  @inline final def foreach(f: A => Unit): Unit = {
    def loop(xs: Plist[A]): Unit = xs match {
      case Pcons(hd, tl) => f(hd) ; loop(tl)
      case _             =>
    }
    loop(this)
  }
}
final case class Pcons[A](head: A, tail: Plist[A]) extends Plist[A]
final case object Pnil extends Plist[Nothing] {
  def head = abort("Pnil.head")
  def tail = abort("Pnil.tail")
}
final class Vec[A](private val underlying: sciVector[A]) extends AnyVal with Direct[A] {
  def isEmpty       = length <= 0
  def nonEmpty      = length > 0
  def lastIntIndex  = length - 1
  def length: Int   = underlying.length
  def size: Precise = Size(length)

  def updated(i: Vdex, elem: A): Vec[A] = new Vec[A](underlying.updated(i.getInt, elem))
  def :+(elem: A): Vec[A] = new Vec[A](underlying :+ elem)
  def +:(elem: A): Vec[A] = new Vec[A](elem +: underlying)
  def ++(that: Vec[A]): Vec[A] = (
    if (that.isEmpty) this
    else if (this.isEmpty) that
    else new Vec[A](underlying ++ that.trav)
  )

  def applyInt(index: Int): A    = underlying(index)
  def drop(n: Vdex): Vec[A]      = new Vec[A](underlying drop n.getInt)
  def dropRight(n: Vdex): Vec[A] = new Vec[A](underlying dropRight n.getInt)
  def elemAt(i: Vdex): A         = underlying(i.getInt)
  def take(n: Vdex): Vec[A]      = new Vec[A](underlying take n.getInt)
  def takeRight(n: Vdex): Vec[A] = new Vec[A](underlying takeRight n.getInt)

  @inline def foreach(f: A => Unit): Unit = {
    if (!isEmpty)
      ll.foreachConsecutive(0, lastIntIndex, i => f(applyInt(i)))
  }
}
object FunctionGrid {
  def apply[A](xs: View[A])(columns: ToString[A]*): FunctionGrid[A, String] = new FunctionGrid(xs, columns.toVec)
}
class FunctionGrid[A, B](values: View[A], functions: View[A => B]) {
  import StdShow._
  def rows: View2D[B]    = values map (v => functions map (f => f(v)))
  def columns: View2D[B] = rows.transpose

  def renderLines(implicit z: Show[B]): Vec[String] = {
    if (values.isEmpty || functions.isEmpty) return vec()
    val widths    = columns map (_ map z.show map (_.length) max)
    val formatFns = widths map lformat

    rows map (formatFns zip _ map (_ apply _) mk_s ' ')
  }
  def render(implicit z: Show[B]): String = renderLines mk_s EOL
}

object Plist {
  def empty[A] : Plist[A]                 = Pnil.castTo[Plist[A]]
  def newBuilder[A] : Builds[A, Plist[A]] = new Builds(xs => ll.foldRight[A, Plist[A]](xs, empty[A], _ :: _))
  def apply[A](xs: A*): Plist[A]          = xs.zfoldr[Plist[A]](_ :: _)
}
object Indexed {
  def apply[A](f: Vdex => A): Pure[A] = Pure(f)

  final case class Pure[A](f: Vdex => A) extends Indexed[A] {
    def size               = Infinite // ...sometimes infinite via overflow, but hey
    def isEmpty            = false
    def elemAt(i: Vdex): A = f(i)
    @inline def foreach(f: A => Unit): Unit = {
      var current: Long = 0L
      while (true) { f(elemAt(Index(current))) ; current += 1 }
    }
  }
}
object Vec {
  private val NIL = new Vec[Any](sciVector())

  def empty[A] : Vec[A]                        = NIL.castTo[Vec[A]]
  def apply[A](xs: A*): Vec[A]                 = new Vec[A](xs.toScalaVector)
  def unapplySeq[A](x: Vec[A]): Some[scSeq[A]] = Some(x.seq)
  def newBuilder[A](): Builds[A, Vec[A]]       = Builds(xs => new Vec[A](xs.seq.toScalaVector))
}
object Direct extends Constructions[Direct] {
  def construct[A](size: Size, mf: Suspended[A]): Vec[A] = Vec.newBuilder[A]()(mf)
  def array[A](xs: Array[A]): Direct[A]                  = new WrapArray[A](xs)
  def reversed[A](xs: Direct[A]): Reversed[A]            = new Reversed(xs)
  def string(s: String): Direct[Char]                    = new WrapString(s)

  trait Common[+A] extends Any with Direct[A] {
    @inline final def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final case class WrapString(xs: String) extends AnyVal with Common[Char] {
    def size            = Size(xs.length)
    def elemAt(i: Vdex) = xs charAt i.getInt
  }
  final case class WrapArray[A](val xs: Array[_]) extends AnyVal with Common[A] {
    def size               = Size(xs.length)
    def elemAt(i: Vdex): A = xs(i.getInt).castTo[A]
  }
  final class Reversed[A](val xs: Direct[A]) extends AnyVal with Common[A] {
    def size            = xs.size
    def elemAt(i: Vdex) = xs elemAt xs.lastIndex - i.indexValue
  }
}
object Each extends Constructions[Each] {
  def apply[A](mf: Suspended[A]): Each[A]                 = new Impl[A](Size.Unknown, mf)
  def construct[A](size: Size, mf: Suspended[A]): Each[A] = new Impl[A](size, mf)
  def continually[A](elem: => A): Continually[A]          = new Continually(elem)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]          = new Joined(xs, ys)

  final class Impl[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final class Joined[A](xs: Each[A], ys: Each[A]) extends Each[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = sideEffect(xs foreach f, ys foreach f)
  }
  final class Continually[A](expr: => A) extends Each[A] {
    def size = Infinite
    @inline def foreach(f: A => Unit): Unit = while (true) f(expr)
  }

  def unapplySeq[A](xs: Foreach[A]): Some[scSeq[A]] = Some(xs.seq)
}
