package psp
package tests

import api._, std._, all._, StdShow._
import org.scalacheck.Prop._, Color._

object Color {
  import scala.Console.{ println => _, _ }

  val Reset = Color(RESET)
  val Green = Color(GREEN)
  val Red   = Color(RED)
  val Cyan  = Color(CYAN)

  implicit def showColor: Show[Color] = Show(_.code)
}

final case class Color(code: String) {
  import scala.Console.{ BOLD }
  def bold(x: Doc): Doc  = doc"$code$BOLD$x$Reset"
  def apply(x: Doc): Doc = doc"$code$x$Reset"
}

trait Explicit {
  type Arb[A]             = org.scalacheck.Arbitrary[A]
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C[A]]
  type Choose[A]          = org.scalacheck.Gen.Choose[A]
  type Forall1[-A]        = ToBool[A]
  type Forall2[-A]        = EqRelation[A]
  type Forall3[-A]        = (A, A, A) => Boolean
  type GenParams          = Gen.Parameters
  type Gen[A]             = org.scalacheck.Gen[A]
  type Pretty             = org.scalacheck.util.Pretty
  type Prop               = org.scalacheck.Prop
  type Result             = org.scalacheck.Test.Result
  type Test               = org.junit.Test
  type TestParams         = Test.Parameters

  val Arb    = org.scalacheck.Arbitrary
  val Choose = org.scalacheck.Gen.Choose
  val Gen    = org.scalacheck.Gen
  val Pretty = org.scalacheck.util.Pretty
  val Prop   = org.scalacheck.Prop
  val Test   = org.scalacheck.Test
  val Predef = scala.Predef

  private implicit def hashThrowable: Hash[Throwable] = hashBy[Throwable](_.getClass)

  def arb[A](implicit z: Arb[A]): Arb[A] = z

  implicit def genToArb[A](g: Gen[A]): Arb[A] = Arb(g)
  implicit def arbToGen[A](g: Arb[A]): Gen[A] = g.arbitrary

  def assert(p: => Boolean, msg: => Any)(implicit z: Assertions): Unit = Assertions.using(z)(p, s"assertion failed: $msg")
  def associative[A: Arb : Eq](f: BinOp[A]): Prop                      = forAll((a: A, b: A, c: A) => sameBehavior(f(f(a, b), c), f(a, f(b, c))))
  def commutative[A: Arb : Eq](f: BinOp[A]): Prop                      = forAll((a: A, b: A) => sameBehavior(f(a, b), f(b, a)))
  def expectType(expected: jClass, found: jClass): NamedProp           = fp"$expected%15s  >:>  $found%s" -> Prop(expected isAssignableFrom found)
  def expectType[A: CTag](result: A): NamedProp                        = expectType(classOf[A], result.getClass)
  def expectTypes(expected: jClass, found: Each[jClass]): NamedProp    = fp"$expected%15s  >:>  $found%s" -> found.map(c => Prop(expected isAssignableFrom c))
  def expectTypes[A: CTag](results: A*): NamedProp                     = expectTypes(classOf[A], results map (_.getClass) toVec)
  def expectValue[A: Eq: Show](expected: A)(x: A): NamedProp           = x.show -> (expected =? x)
  def junitAssert(body: => Boolean): Unit                              = org.junit.Assert assertTrue body
  def preNewline(s: String): String                                    = if (s containsChar '\n') "\n" + s.mapLines("| " append _) else s
  def printResultIf[A: Show : Eq](x: A, msg: String)(result: A): A     = doto(result)(r => if (r === x) println(pp"$msg: $r"))
  def printResult[A: Show](msg: String)(result: A): A                  = doto(result)(r => println(pp"$msg: $r"))
  def sameDocAsToString[A: Show](expr: A): Unit                        = same(expr.show, expr.toString)
  def sameDoc(expr: Doc, expected: Doc): Unit                          = same(expr.show, expected.show)
  def seqShows[A: Show](expected: String, xs: View[A]): NamedProp      = preNewline(expected) -> (expected =? (xs joinWith ", "))
  def showsAs[A: Show](expected: String, x: A): NamedProp              = preNewline(expected) -> (expected =? pp"$x")

  def same[A : Eq : Show](expr: A, expected: A): Unit = {
    assert(expr === expected, pp"""
      |Expected: $expected
      |  Actual: $expr
      """)
  }
  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  def sameBehavior[T: Eq](p1: => T, p2: => T): Bool           = Try(p1) === Try(p2)
  def sameOutcomes[A: Arb, B: Eq](f: A => B, g: A => B): Prop = forAll((x: A) => sameBehavior(f(x), g(x)))

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  def functionRelation[A : Arb, B : Eq] : Eq[A => B] =
    Relation equiv ((f, g) => (Test check forAll((x: A) => f(x) === g(x)))(identity).passed)
}

trait Implicit extends Explicit {
  import org.scalacheck.Prop._
  import org.scalacheck.util.Pretty.{ pretty, Params }

  implicit class ColorDocOps(val doc: Doc) {
    def in(c: Color): Doc = c(doc)
  }

  implicit def arbIndex: Arb[Index] = gen.index
  implicit def arbNth: Arb[Nth]     = gen.index ^^ (_.toNth)
  implicit def arbSize: Arb[Size]   = gen.size
  implicit def arbWord: Arb[String] = gen.text.word

  implicit def arbPair[A: Arb, B: Arb]: Arb[A->B] = arb[A] zip arb[B]

  implicit def assertions: Assertions             = ImmediateTraceAssertions
  implicit def showScalacheckResult: Show[Result] = Show(r => pretty(r, Params(0)))

  implicit def buildsBuildable[A, CC[X]](implicit z: Makes[A, CC[A]]): Buildable[A, CC] =
    new Buildable[A, CC] { def builder = z.scalaBuilder }

  implicit def walksWalkable[A, CC[X]](implicit z: Walks[A, CC[A]]): CC[A] => scTraversable[A] =
    z walk _ trav

  implicit class ArbitraryOps[A](x: Arb[A]) {
    def map[B](f: A => B): Arb[B]    = Arb(x.arbitrary map f)
    def filter(p: ToBool[A]): Arb[A] = Arb(x.arbitrary filter p)
  }
  implicit class LiftConverter[A](gen: Gen[A]) {
    def to[B](implicit f: A => B): Gen[B] = gen map f
  }
  implicit class Gen2Ops[A, B](g: (Gen[A], Gen[B])) {
    def filter(p: (A, B) => Boolean)       = Gen.zip(g._1, g._2) suchThat p.tupled
    def map[C](f: (A, B) => C): Gen[C]     = Gen.zip(g._1, g._2) map f.tupled
    def ^^[C](f: (A, B) => C): Gen[C]      = map(f)
    def >>[C](f: (A, B) => Gen[C]): Gen[C] = g._1 >> (x => g._2 >> (y => f(x, y)))
  }
  implicit class ArbOps[A](val self: Arb[A]) extends GenTransform[Arb, A] {
    def transform[B](f: Gen[A] => Gen[B]): Arb[B] = Arb(f(self.arbitrary))
    def flatMap[B](f: A => Gen[B]): Arb[B]        = Arb(self.arbitrary flatMap f)
  }
  implicit class GenOps[A](val self: Gen[A]) extends GenTransform[Gen, A] {
    def transform[B](f: Gen[A] => Gen[B]): Gen[B] = f(self)
    def stream: Each[A]                           = Each continually self.sample flatMap (_.view) toVec
    def take(n: Int): Direct[A]                   = stream take n toVec
  }
  implicit class TestLongOps(val self: Long) {
    def upTo(hi: Long): Gen[Long] = Gen.choose(self, hi)
  }
  implicit class TestIntOps(val self: Int) {
    def upTo(hi: Int): Gen[Int] = Gen.choose(self, hi)
  }
  implicit class PropOps(p: Prop) {
    def mapParams(f: ToSelf[TestParams]): Prop = new NamedProp.MapParams(p, f)
    def minSuccessful(size: Precise): Prop     = mapParams(_ withMinSuccessfulTests size.getLong.toInt)
  }
  implicit class PropResultOps(r: Prop.Result) {
    def unary_! : Prop.Result = r.copy(status = !r.status)
  }
  implicit class StatusOps(s: Prop.Status) {
    def unary_! : Prop.Status = s match {
      case Proof => False
      case True  => False
      case False => True
      case x     => x
    }
  }
  implicit class TestOnlyAnyOps[A](private val lhs: A) {
    def =?(rhs: A)(implicit eqv: Eq[A], z: Show[A]): Prop = {
      def label = doc"Expected $rhs but got $lhs"
      cond(lhs === rhs, proved, falsified :| label.render)
    }
  }
}
