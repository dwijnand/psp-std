package psp
package std

import api._, all._

sealed abstract class Plist[A] extends Each[A] {
  def head: A
  def tail: Plist[A]

  def ::(head: A): Plist[A] = Pcons(head, this)
  def isEmpty = this eq Pnil
  def size    = if (isEmpty) Size(0) else Size(1).atLeast

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

object Plist {
  def empty[A] : Plist[A]                 = Pnil.castTo[Plist[A]]
  def newBuilder[A] : Builds[A, Plist[A]] = new Builds(xs => ll.foldRight[A, Plist[A]](xs, empty[A], _ :: _))
  def apply[A](xs: A*): Plist[A]          = xs.zfoldr[Plist[A]](_ :: _)
}
