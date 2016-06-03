package psp
package tests

import psp.std._, all._, StdShow._
import org.scalacheck._, Prop.forAll, Gen._

class OperationCounts extends ScalacheckBundle {
  type LongView = RView[Long, Counter]
  type LongOp   = LongView => LongView

  def bundle    = "Operation Counts"
  def max: Long = 100L
  def fullRange = 0L upTo max
  def lowHalf   = 0L upTo max / 2
  def highHalf  = max / 2 upTo max
  def genSmall  = 1L upTo max / 20
  def genRange  = (genSmall zipWith fullRange)(_ indexUntil _)
  def rangeView = 1L to max m

  class Counter(r: LongRange) extends Direct[Long] {
    private var _access: Long = 0L

    def count: Long            = _access
    def size: Precise          = r.size
    def apply(idx: Vdex): Long = doalso(r(idx))(_access += 1)
  }
  def checkOp(op: LongOp): Bool = synchronized {
    val c = new Counter(1L to max)
    val v = op(c)
    val r: Vec[Long] = v take 3 force;

    err println fp"${c.count}%4s ops for ${ v.opDoc }%-80s // $r%s"
    true
  }

  private def divides(n: Long)  = ((_: Long) % n === 0) labeled pp"/$n"
  private def less(n: Long)     = ((_: Long) < n) labeled pp"<$n"
  private def multiply(n: Long) = ((_: Long) * n) labeled pp"*$n"
  private def pairup            = ((x: Long) => view(x, x)) labeled pp"=>(x,x)"
  private def cdivides(n: Long) = Fun.partial(divides(n), (_: Long) / n)

  private def mk(fn: LongOp): LongOp = fn

  def genOneOp: Gen[LongOp] = oneOf(
    lowHalf   ^^ (n => mk(_ drop n)),
    highHalf  ^^ (n => mk(_ take n)),
    fullRange ^^ (n => mk(_ dropRight n)),
    fullRange ^^ (n => mk(_ takeRight n)),
    lowHalf   ^^ (n => mk(_ dropWhile less(n))),
    lowHalf   ^^ (n => mk(_ takeWhile less(n))),
    genSmall  ^^ (n => mk(_ map multiply(n))),
    genSmall  ^^ (n => mk(_ filter divides(n))),
    genSmall  ^^ (n => mk(_ collect cdivides(n))),
    genSmall  ^^ (n => mk(_ flatMap pairup))
  )
  implicit def arbCompositeOp: Arbitrary[LongOp] = Arbitrary(genCompositeOp)

  def genCompositeOp: Gen[LongOp] = genOneOp * (2 upTo 4) ^^ (_ reducel (_ andThen _))
  def compositeProp: Prop         = forAll(checkOp _)

  def props() = vec("views behave as expected" -> compositeProp)
}
