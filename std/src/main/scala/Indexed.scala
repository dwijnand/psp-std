package psp
package std

import api._, all._ //, StdEq._
// import java.util.concurrent.LinkedBlockingQueue

object indices {
  def all: Indexed[Index]                      = Indexed(i => i.toIndex)
  def from(start: SafeLong): Indexed[SafeLong] = Indexed(i => start + i.get)
  def from(start: BigInt): Indexed[BigInt]     = Indexed(i => start + i.get)
  def from(start: Long): Indexed[Long]         = Indexed(i => start + i.get)
  def from(start: Int): Indexed[Int]           = Indexed(i => start + i.getInt)
}

/** Indexed is somewhere between Each and Direct.
 *  There's an apply(index) method, but its size may not be known and may be infinite.
 *  We can memoize an Each into an Indexed.
 */
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

  // final class MemoIterator[+A](memo: Memo[A]) extends scIterator[A] {
  //   @volatile private[this] var index: Index = Index(0)
  //   def hasNext = memo isDefinedAt index
  //   def next: A = sideEffect(memo(index), index += 1)
  // }

  // private def spawn[A](body: => A): Unit = {
  //   val t = new Thread() { override def run(): Unit = body }
  //   t setDaemon true
  //   t.start()
  // }

  // final class Memo[+A](xs: Each[A]) extends Indexed[A] {
  //   @volatile private[this] var doneProducing = false
  //   @volatile private[this] var doneConsuming = false
  //   @volatile private[this] var memo = vec[A]()
  //   private[this] val handoff = new LinkedBlockingQueue[A](1)

  //   private[this] lazy val thread: Unit = spawn({ xs foreach handoff.put ; doneProducing = true })
  //   private[this] def seen = Size(memo.length)
  //   private[this] def next(): A = handoff.poll match {
  //     case null => nullAs[A]
  //     case elem => doto(elem)(x => memo = memo :+ x)
  //   }
  //   private[this] def hasNext: Boolean = !doneConsuming && {
  //     handoff.peek match {
  //       case null if !doneProducing => threadYield() ; hasNext
  //       case null                   => doneConsuming = true ; false
  //       case _                      => true
  //     }
  //   }
  //   private[this] def advanceTo(index: Index): A = {
  //     if (index.isInvalid) abort(s"NoIndex")

  //     thread
  //     while (index.sizeIncluding > seen && hasNext) {
  //       next()
  //     }
  //     memo(index.getInt)
  //   }
  //   def isDefinedAt(i: Index): Boolean = {
  //     !i.isInvalid && (
  //          (seen containsIndex i)
  //       || (Try(sideEffect(true, advanceTo(i))) | false)
  //     )
  //   }

  //   def iterator: scIterator[A]     = new MemoIterator(this)
  //   def foreach(f: A => Unit): Unit = iterator foreach f
  //   def apply(index: Vindex): A     = advanceTo(index)
  //   def elemAt(index: Vindex): A    = advanceTo(index)
  //   def size: Size                  = if (doneConsuming) seen else seen.atLeast
  // }
}
