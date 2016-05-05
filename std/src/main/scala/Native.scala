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
