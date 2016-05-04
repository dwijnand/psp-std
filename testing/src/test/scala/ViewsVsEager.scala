package psp
package tests

import psp.std._, all._, api._, StdShow._, StdEq._
import org.scalacheck._, Prop.forAll, Gen._

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ShowSelf {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction[_,_] => f isDefinedAt x
    case _                             => true
  }
  def apply(x: T): R = f(x)
}

class OperationCounts extends ScalacheckBundle {
  import Op._

  type IntOp = Op[Int, Int]
  def numComposite                = 2 upTo 4

  private[this] var displaysRemaining = maxDisplay

  def bundle                 = "Operation Counts"
  def max                    = 100
  def minSuccessful: Precise = 1000

  val counter = OperableCounter(1 to max)

  def maxDisplay: Precise    = 20
  def chooseMax   = 0 upTo max
  def lowHalf     = 0 upTo max / 2
  def highHalf    = max / 2 upTo max
  def chooseSmall = 1 upTo max / 20
  def chooseRange = gen.indexRangeFrom(max / 2, max)

  private def lop[A, B](label: String, f: A => B): A => B = new LabeledFunction(f, label)

  private def divides(n: Int)  = lop(s"/$n", (_: Int) % n == 0)
  private def less(n: Int)     = lop(s"<$n", (_: Int) < n)
  private def multiply(n: Int) = lop(s"*$n", (_: Int) * n)
  private def pairup           = lop("=>(x,x)", (x: Int) => view(x, x))

  def genOneOp: Gen[IntOp] = oneOf(
    lowHalf     ^^ (n => Pos[Int](Drop(n))),
    highHalf    ^^ (n => Pos[Int](Take(n))),
    chooseMax   ^^ (n => Pos[Int](DropRight(n))),
    chooseMax   ^^ (n => Pos[Int](TakeRight(n))),
    lowHalf     ^^ (n => DropWhile(less(n))),
    lowHalf     ^^ (n => TakeWhile(less(n))),
    chooseSmall ^^ (n => Maps(multiply(n))),
    chooseSmall ^^ (n => Filter(divides(n))),
    chooseSmall ^^ (n => Collect(Partial(divides(n), (_: Int) / n))),
    chooseSmall ^^ (n => FlatMap(pairup)),
    chooseRange ^^ (r => Pos[Int](Slice(r)))
  )
  def genCompositeOp: Gen[IntOp] = genOneOp * numComposite ^^ (_ reducel (_ ~ _))

  def composite: Gen[CompositeOp] = genCompositeOp ^^ CompositeOp

  case class CompositeOp(op: IntOp) {
    lazy val (maybeRes, views, eager) = counter(op ~ Take(3))
    lazy val countsPass = views.accesses <= eager.accesses && views.allocations <= eager.allocations
    lazy val isPass = maybeRes.isRight && countsPass
    lazy val passed = sideEffect(isPass, maybeShow())

    def compare(lhs: Long, rhs: Long): String = "%3s %-2s %-3s".format(lhs, if (lhs <= rhs) "<=" else ">", rhs)

    def counts1 = compare(views.accesses, eager.accesses)
    def counts2 = compare(views.allocations, eager.allocations)

    def res_s = maybeRes match {
      case scala.Right(res) => res.to_s
      case scala.Left(err)  => err
    }
    def pass_s = "|%s  %s  %s  // %s".format("%-70s".format(op), counts1, counts2, res_s) // $op $counts // ${optResult.get}"
    def fail_s = s"Inconsistent results for $op: $res_s"

    private def maybeShow(): Unit = {
      if (!isPass)
        println(fail_s)
      else if (isTestDebug || displaysRemaining > 0)
        sideEffect(println(pass_s), displaysRemaining -= 1)
    }
  }

  implicit def arbCompositeOp: Arbitrary[CompositeOp] = Arbitrary(composite)

  def compositeProp: Prop = forAll((_: CompositeOp).passed) minSuccessful minSuccessful
  def props() = Vec[NamedProp](
    NamedProp(s"Showing $maxDisplay/$minSuccessful ops, compares accesses/allocations views v. eager", Prop(true)),
    NamedProp("views never performs more accesses or allocations than eager", compositeProp)
  )
}

object OperableCounter {
  import Operable._
  import Op._

  class CountXs[A](xs: api.Direct[A], val counter: OpCount) extends api.Direct[A] with ShowSelf {
    import Unsafe.inheritedShow

    def size: Precise               = xs.size
    def elemAt(idx: Vdex): A        = sideEffect(xs elemAt idx, counter access idx)
    def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))

    // toString accesses not to be counted
    def to_s = (
      if (size > 3)
        "[ " + (xs take 3 mk_s ", ") + ", ...]"
      else
        "[ " + (xs mk_s ", ") + " ]"
    )
  }

  case class ViewOpCount(label: String, accesses: Long, allocations: Long) extends ShowSelf {
    def to_s = "%5s: %4s accesses, %4s allocations".format(label, accesses, allocations)
  }

  class OpCount(label: String) {
    private var _access, _alloc    = 0L

    def reset(): Unit              = sideEffect(_access = 0, _alloc = 0)
    def access(vdex: Vdex): Unit   = _access += 1
    def alloc(size: Precise): Unit = _alloc += size.getLong
    def result(): ViewOpCount      = ViewOpCount(label, _access, _alloc)

    override def toString = result.toString
  }

  implicit object OperableCountXs extends Operable[CountXs] {
    /** This is the key moment. We decompose the composite
     *  operation and force it at each step so as to obtain access
     *  and allocation counts. If we simply pass it through, only
     *  one operation will be applied, leaving the eager case about
     *  the same as the view case.
     */
    def apply[A, B](xs: CountXs[A])(op: Op[A, B]): CountXs[B] = op match {
      case Compose(o1, o2) => apply(apply(xs)(o1))(o2)
      case _               => OperableView(xs)(op).force |> (res => sideEffect(new CountXs(res, xs.counter), xs.counter alloc res.size))
    }
  }

  class Compared(range: IntRange) {
    def apply(op: Op[Int, Int]) = {
      val c1 = new OpCount("views")
      val c2 = new OpCount("eager")
      val r1 = op[View](new CountXs(range, c1)).force
      val r2 = op[CountXs](new CountXs(range, c2)).force

      val res = r1 === r2 match {
        case true  => scala.Right(r1)
        case false => scala.Left(s"$r1 != $r2")
      }
      ((res, c1.result, c2.result))
    }
  }

  def apply(range: IntRange) = new Compared(range)
}
