package psp
package tests

import psp._, std._, all._, api._, StdShow._
import Color._

trait Bundle extends ShowSelf {
  def run(): Bool
}

/** Needed because scalacheck doesn't expose the label if you add
 *  labels with the |: operator.
 */
final class NamedProp(val label: String, p: Prop) {
  def prop = p :| label
  def check: Test.Result = p match {
    case NamedProp.MapParams(prop, f) => Test.check(prop)(f)
    case _                            => Test.check(p)(identity)
  }
}
object NamedProp {
  final case class MapParams(underlying: Prop, f: ToSelf[TestParams]) extends Prop {
    def apply(prms: GenParams)                 = underlying(prms)
    override def check(prms: TestParams): Unit = super.check(f(prms))
  }

  def apply(label: String, p: Prop): NamedProp                 = new NamedProp(label, p)
  implicit def liftSeqPair(x: String -> View[Prop]): NamedProp = NamedProp(fst(x), snd(x) reducel (_ && _))
  implicit def liftPair(x: String -> Prop): NamedProp          = NamedProp(fst(x), snd(x))
}

trait ScalacheckBundle extends Bundle {
  def bundle: String
  def props: Direct[NamedProp]

  def pass  = Green("\u2713") // check mark
  def fail  = Red("\u2717")   // cross mark
  def start = Cyan bold bundle

  def runOne(p: NamedProp): Boolean = p.check |> (r =>
    doto(r.passed) {
      case true  => println(doc"+ $pass  ${p.label}")
      case false => println(doc"- $fail  ${p.label}\nFalsified after ${r.succeeded} passed tests\n$r")
    }
  )

  def run(): Boolean = {
    println(doc"\n+ $start")
    props map runOne forall (x => x)
  }

  @Test
  def runBundle(): Unit = junitAssert(run())

  def to_s: String = bundle
}
