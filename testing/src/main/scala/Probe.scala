package psp
package tests

import psp.std._, all._, api._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

object Probe {
  final case class Direct(range: IntRange, counter: RecorderCounter) extends api.Direct[Int] {
    def size                          = range.size
    def elemAt(i: Vindex)             = counter record range(i)
    def foreach(f: Int => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
    def isEmpty                       = range.isEmpty
    def view                          = new DirectView(this)
    override def toString             = s"${this.shortClass} ($counter)"
  }
  final case class ScalaDirect(range: IntRange, counter: RecorderCounter) extends sci.IndexedSeq[Int] with sc.IndexedSeqLike[Int, ScalaDirect] with sc.IndexedSeqOptimized[Int, ScalaDirect] {
    override def newBuilder: scmBuilder[Int, ScalaDirect] = sciVector.newBuilder[Int] mapResult (xs => ScalaDirect(xs.head to xs.last, counter))
    def apply(index: Int): Int                            = counter record range(Index(index))
    def length: Int                                       = range.size.getInt
    override def toString                                 = s"$view ($counter)"
  }
}
