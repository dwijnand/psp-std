package psp
package tests

import psp._, std._, all._, StdShow._
import Color._

trait Bundle extends ShowSelf {
  def run(): Bool
}

/** Needed because scalacheck doesn't expose the label if you add
 *  labels with the |: operator.
 */
final class NamedProp(val label: String, p: Prop) {
  def prop: Prop         = p :| label
  def check: Test.Result = Test.check(p)(identity)
}
object NamedProp {
  def apply(label: String, p: Prop): NamedProp = new NamedProp(label, p)

  implicit def liftSeqPair(x: String -> View[Prop]): NamedProp = NamedProp(fst(x), snd(x) reducel (_ && _))
  implicit def liftPair(x: String -> Prop): NamedProp          = NamedProp(fst(x), snd(x))
}

abstract class ScalacheckBundle extends Bundle {
  def bundle: String
  def props: Direct[NamedProp]

  def pass  = Green("\u2713") // check mark
  def fail  = Red("\u2717")   // cross mark
  def start = Cyan bold bundle

  def runOne(p: NamedProp): Boolean = p.check |> (r =>
    doto(r.passed) {
      case true  => log"+ $pass  ${p.label}"
      case false => log"- $fail  ${p.label}\nFalsified after ${r.succeeded} passed tests\n$r"
    }
  )

  def run(): Boolean = {
    log"\n+ $start"
    props map runOne forall (x => x)
  }

  @Test
  def runBundle(): Unit = junitAssert(run())

  def to_s: String = bundle
}
