package psp
package std
package lowlevel

import api._, all._ //, StdEq._
import scala.Tuple2
// import java.nio.ByteBuffer
import java.io.{ ByteArrayOutputStream, BufferedInputStream }

object ll {
  /** Can't refer directly to fields because scala bloats all the bytecode
   *  going through getters. This way the parameters are locals.
   *  @pre non-empty sequence
   *  @param start    the first int
   *  @param last     the last int
   *  @param f        the function to apply
   */
  @inline final def foreachConsecutive(start: Int, last: Int, f: Int => Unit): Unit = {
    var elem = start - 1
    while (true) {
      elem += 1
      f(elem)
      if (elem == last) return
    }
  }

  /** Here's the bytecode the above produces. We'd like a test which ensures it
   *  stays this way. There is some code in the scala distribution which verifies bytecode
   *  is as expected but it's still not the smoothest process. TODO: test.
   *
   *  In the interim one can inspect the bytecode from "sbt console" via
   *
   *    :javap psp.std.lowlevel.ll$
   *
   *  The critical elements are that the only non-local call is the function
   *  application, and the function application is specialized - that is, its name
   *  is "apply$mcVI$sp" and it has a (I)V descriptor, as opposed to one called
   *  "apply" which accepts an Object. Also that the function comes in under 35 bytes,
   *  which is the default hotspot threshold for function inlining.
   *
   *  Preserving those qualities is why the method assumes non-emptiness and has a
   *  slightly unconventional structure. The emptiness check is performed once so can
   *  be lifted outside the method.
   */

  // public final void foreachConsecutive(int, int, scala.Function1<java.lang.Object, scala.runtime.BoxedUnit>);
  //   descriptor: (IILscala/Function1;)V
  //        0: iload_1
  //        1: iconst_1
  //        2: isub
  //        3: istore        4
  //        5: iload         4
  //        7: iconst_1
  //        8: iadd
  //        9: istore        4
  //       11: aload_3
  //       12: iload         4
  //       14: invokeinterface #20,  2           // InterfaceMethod scala/Function1.apply$mcVI$sp:(I)V
  //       19: iload         4
  //       21: iload_2
  //       22: if_icmpne     5
  //       25: return

  final def foldLeft[@fspec A, @fspec B](xs: Foreach[A], initial: B, f: (B, A) => B): B = {
    var res = initial
    xs foreach (x => res = f(res, x))
    res
  }
  // final def foldLeftIndexed[@fspec A, @fspec B](xs: Each[A], initial: B, f: (B, A, Index) => B): B = {
  //   var res = initial
  //   xs.zipIndex foreach ((x, i) => res = f(res, x, i))
  //   res
  // }
  final def foldRight[@fspec A, @fspec B](xs: Foreach[A], initial: B, f: (A, B) => B): B = {
    val arr: Array[Ref[A]] = doto(xs.toRefArray)(_.inPlace.reverse)
    var res: B = initial
    arr foreach (x => res = f(x, res))
    res
  }
}

/** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
 */
// final class ArrowAssocInt(val self: Int) extends AnyVal {
//   @inline def -> [@fspec B](y: B): Tuple2[Int, B] = Tuple2(self, y)
// }
// final class ArrowAssocLong(val self: Long) extends AnyVal {
//   @inline def -> [@fspec B](y: B): Tuple2[Long, B] = Tuple2(self, y)
// }
// final class ArrowAssocDouble(val self: Double) extends AnyVal {
//   @inline def -> [@fspec B](y: B): Tuple2[Double, B] = Tuple2(self, y)
// }
// final class ArrowAssocChar(val self: Char) extends AnyVal {
//   @inline def -> [@fspec B](y: B): Tuple2[Char, B] = Tuple2(self, y)
// }
// final class ArrowAssocBoolean(val self: Boolean) extends AnyVal {
//   @inline def -> [@fspec B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
// }
final class ArrowAssocRef[A](val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}

final class CircularBuffer[@fspec A](capacity: Precise) extends Direct[A] {
  assert(!capacity.isZero, "CircularBuffer capacity cannot be 0")

  private[this] def cap: Int            = capacity.getInt
  private[this] val buffer              = newArray[Any](cap)
  private[this] var seen                = 0L
  private[this] def writePointer: Int   = (seen % cap).toInt
  private[this] def readPointer         = if (isFull) writePointer else 0
  private[this] def setHead(x: A): Unit = sideEffect(buffer(writePointer) = x, seen += 1)

  @inline def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))

  def isFull                         = seen >= cap
  def elemAt(index: Vdex): A         = buffer((readPointer + index.getInt) % cap).castTo[A]
  def size: Precise                  = capacity min Size(seen)
  def ++=(xs: Foreach[A]): this.type = sideEffect(this, xs foreach setHead)
  def += (x: A): this.type           = sideEffect(this, setHead(x))
  def push(x: A): A                  = if (isFull) sideEffect(this.head, setHead(x)) else abort("push on non-full buffer")
}
// final class ByteBufferInputStream(b: ByteBuffer) extends InputStream {
//   private def empty = !b.hasRemaining

//   override def read()                                       = if (empty) -1 else b.get & 0xFF
//   override def read(bytes: Array[Byte], off: Int, len: Int) = if (empty) -1 else doto(len min b.remaining)(b.get(bytes, off, _))
// }

// final class ByteBufferOutputStream(b: ByteBuffer) extends OutputStream {
//   override def write(x: Int)                                 = b put x.toByte
//   override def write(bytes: Array[Byte], off: Int, len: Int) = b.put(bytes, off, len)
// }

// object ArrowAssoc {
//   val Types = new scala.Specializable.Group((scala.Int, scala.Long, scala.Double, scala.Char, scala.Boolean))
// }

object CircularBuffer {
  def builder[@fspec A](capacity: Precise): Builds[A, CircularBuffer[A]] = Builds(xs => CircularBuffer[A](capacity) ++= xs)
  def apply[@fspec A](capacity: Precise): CircularBuffer[A]              = new CircularBuffer[A](capacity)
}
object Streams {
  final val InputStreamBufferSize = 8192

  def slurp(in: BufferedInputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream
    val buf = new Array[Byte](InputStreamBufferSize)
    def loop(): Array[Byte] = in read buf match {
      case -1 => out.toByteArray
      case n  => out.write(buf, 0, n) ; loop()
    }
    sideEffect(loop(), in.close())
  }
  def slurp(in: BufferedInputStream, size: Precise): Array[Byte] = {
    val len = size.toInt
    val buf = newArray[Byte](len)
    def loop(remaining: Int): Array[Byte] = {
      if (remaining == 0) buf
      else in.read(buf, len - remaining, remaining) match {
        case -1 => buf
        case n  => loop(remaining - n)
      }
    }
    sideEffect(loop(len), in.close())
  }
}
