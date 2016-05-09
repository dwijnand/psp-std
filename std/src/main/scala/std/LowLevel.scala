package psp
package std

import api._, all._
import java.io.{ ByteArrayOutputStream, BufferedInputStream }

object ll {

  /** Can't refer directly to fields because scala bloats all the bytecode
    *  going through getters. This way the parameters are locals.
    *  @pre non-empty sequence
    *  @param start    the first int
    *  @param last     the last int
    *  @param f        the function to apply
    */
  @inline def foreachIntNonEmpty(start: Int, last: Int, f: Int => Unit): Unit = {
    var elem = start - 1
    while (true) {
      elem += 1
      f(elem)
      if (elem == last) return
    }
  }
  @inline def foreachLongNonEmpty(start: Long, last: Long, f: Long => Unit): Unit = {
    var elem = start - 1
    while (true) {
      elem += 1
      f(elem)
      if (elem == last) return
    }
  }

  @inline def foreachLong(start: Long, last: Long, f: Long => Unit): Unit =
    if (start <= last) foreachLongNonEmpty(start, last, f)

  @inline def foreachInt(start: Int, last: Int, f: Int => Unit): Unit =
    if (start <= last) foreachIntNonEmpty(start, last, f)

  /** Here's the bytecode the above produces. We'd like a test which ensures it
    *  stays this way. There is some code in the scala distribution which verifies bytecode
    *  is as expected but it's still not the smoothest process. TODO: test.
    *
    *  In the interim one can inspect the bytecode from "sbt console" via
    *
    *    :javap psp.std.ll$
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

  final def foldLeft[A, B](xs: Foreach[A], initial: B, f: (B, A) => B): B = {
    var res = initial
    xs foreach (x => res = f(res, x))
    res
  }
  // final def foldLeftIndexed[A, B](xs: Each[A], initial: B, f: (B, A, Index) => B): B = {
  //   var res = initial
  //   xs.zipIndex foreach ((x, i) => res = f(res, x, i))
  //   res
  // }
  final def foldRight[A, B](xs: Foreach[A], initial: B, f: (A, B) => B): B = {
    val arr: Array[Ref[A]] = doto(xs.toRefArray)(_.inPlace.reverse)
    var res: B = initial
    arr foreach (x => res = f(x, res))
    res
  }

  def foreachTakeWhile[A](xs: Foreach[A], f: A => Unit, p: ToBool[A]): Int = {
    var taken = 0
    xs foreach { x =>
      if (!p(x)) return taken
      f(x)
      taken += 1
    }
    taken
  }
  def foreachDropWhile[A](xs: Foreach[A], f: A => Unit, p: ToBool[A]): Int = {
    var dropping = true
    var dropped  = 0
    xs foreach { x =>
      if (dropping && p(x)) dropped += 1
      else {
        if (dropping) dropping = false
        f(x)
      }
    }
    dropped
  }
  def foreachSlice[A](xs: Foreach[A], range: VdexRange, f: A => Unit): Unit = {
    if (range.isEmpty) return
    val start = range.head.indexValue
    val last  = range.last.indexValue
    var current = 0L

    xs foreach { x =>
      if (start <= current && current <= last) f(x)
      current += 1
      if (current > last) return
    }
  }

  // Precondition: n > 0
  def foreachTakeRight[A](xs: Foreach[A], f: A => Unit, n: Precise): Unit =
    (CBuf[A](n) ++= xs) foreach f

  // Precondition: n > 0
  def foreachDropRight[A](xs: Foreach[A], f: A => Unit, n: Precise): Unit =
    foldLeft[A, CBuf[A]](xs, CBuf[A](n), (buf, x) => if (buf.isFull) sideEffect(buf, f(buf push x)) else buf += x)

  /** Circular Buffer. */
  private case class CBuf[A](capacity: Precise) extends Direct[A] {
    assert(!capacity.isZero, "CBuf capacity cannot be 0")

    private[this] def cap: Int = capacity.getInt
    private[this] val buffer = newArray[Any](cap)
    private[this] var seen = 0L
    private[this] def writePointer: Int   = (seen % cap).toInt
    private[this] def readPointer         = cond(isFull, writePointer, 0)
    private[this] def setHead(x: A): Unit = sideEffect(buffer(writePointer) = x, seen += 1)

    @inline def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))

    def isFull                         = seen >= cap
    def elemAt(index: Vdex): A         = cast(buffer((readPointer + index.getInt) % cap))
    def size: Precise                  = capacity min Size(seen)
    def ++=(xs: Foreach[A]): this.type = sideEffect(this, xs foreach setHead)
    def +=(x: A): this.type            = sideEffect(this, setHead(x))
    def push(x: A): A                  = if (isFull) sideEffect(this.head, setHead(x)) else abort("push on non-full buffer")
  }
  object Streams {
    final val InputStreamBufferSize = 8192

    def slurp(in: BufferedInputStream): Array[Byte] = {
      val out = new ByteArrayOutputStream
      val buf = new Array[Byte](InputStreamBufferSize)
      def loop(): Array[Byte] = in read buf match {
        case -1 => out.toByteArray
        case n  => out.write(buf, 0, n); loop()
      }
      sideEffect(loop(), in.close())
    }
    def slurp(in: BufferedInputStream, size: Precise): Array[Byte] = {
      val len = size.getLong.toInt
      val buf = newArray[Byte](len)
      def loop(remaining: Int): Array[Byte] = {
        if (remaining == 0) buf
        else
          in.read(buf, len - remaining, remaining) match {
            case -1 => buf
            case n  => loop(remaining - n)
          }
      }
      sideEffect(loop(len), in.close())
    }
  }
}
