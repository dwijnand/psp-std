package psp

import api._, std._, all._, StdShow._, StdEq._
import scala.Console.{ println => _, _ }

package object tests {
  val PassGreen = GREEN + "\u2713" + RESET // check mark
  val FailRed   = RED + "\u2717" + RESET   // cross mark

  /** Scala, so aggravating.
   *  [error] could not find implicit value for parameter equiv: Eq[A => Bool]
   *  The parameter can be given explicitly, it just won't be found unless the
   *  function type is invariant. The same issue arises with intensional sets.
   */
  type InvariantPredicate[A] = A => Bool

  lazy val isTestDebug = sys.props contains "psp.test.debug"

  implicit def assertions: Assertions = ImmediateTraceAssertions

  type Gen[A]             = org.scalacheck.Gen[A]
  type Arb[A]             = org.scalacheck.Arbitrary[A]
  val Arb                 = org.scalacheck.Arbitrary
  val Gen                 = org.scalacheck.Gen
  val Test                = org.scalacheck.Test
  type Choose[A]          = org.scalacheck.Gen.Choose[A]
  val Choose              = org.scalacheck.Gen.Choose
  type Forall1[-A]        = ToBool[A]
  type Forall2[-A]        = Relation[A]
  type Forall3[-A]        = (A, A, A) => Boolean
  type Prop               = org.scalacheck.Prop
  val Prop                = org.scalacheck.Prop
  val Pretty              = org.scalacheck.util.Pretty
  type Result             = org.scalacheck.Test.Result
  type Failed             = org.scalacheck.Test.Failed
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C[A]]
  type GenParams          = Gen.Parameters
  type TestParams         = Test.Parameters
  type Test               = org.junit.Test

  import Prop._

  def junitAssert(body: => Boolean): Unit = org.junit.Assert assertTrue body

  def assert(assertion: => Boolean, msg: => Any)(implicit z: Assertions): Unit =
    Assertions.using(z)(assertion, s"assertion failed: $msg")

  def arb[A](implicit z: Arb[A]): Arb[A] = z

  def commutative[T: Arb : Eq](op: BinOp[T]): Prop = forAll((p1: T, p2: T) => printResultIf(false, s"op($p1, $p2)")(sameBehavior(op(p1, p2), op(p2, p1))))
  def associative[T: Arb : Eq](op: BinOp[T]): Prop = forAll((p1: T, p2: T, p3: T) => printResultIf(false, s"op($p1, $p2, $p3)")(sameBehavior(op(op(p1, p2), p3), op(p1, op(p2, p3)))))

  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  def sameBehavior[T: Eq](p1: => T, p2: => T): Boolean = {
    implicit def t: Eq[Throwable] = eqBy[Throwable](_.getClass)
    Try(p1) === Try(p2)
  }

  def sameOutcomes[A: Arb, B: Eq](f: A => B, g: A => B): Prop =
    forAll((x: A) => sameBehavior(f(x), g(x)))

  def sameOutcomes2[A1: Arb, A2: Arb, B: Eq](f: (A1, A2) => B, g: (A1, A2) => B): Prop =
    forAll((p1: A1, p2: A2) => sameBehavior(f(p1, p2), g(p1, p2)))

  implicit class ArbitraryOps[A](x: Arb[A]) {
    def map[B](f: A => B): Arb[B]    = Arb(x.arbitrary map f)
    def filter(p: ToBool[A]): Arb[A] = Arb(x.arbitrary filter p)
  }

  implicit def arbProduct[A1: Arb, A2: Arb](implicit z: Arb[(A1, A2)]): Arb[A1->A2] = Arb(z.arbitrary ^^ (x => x))

  implicit def arbIndex: Arb[Index]     = Arb(gen.index)
  implicit def arbNth: Arb[Nth]         = Arb(gen.index ^^ (_.toNth))
  implicit def arbSize: Arb[Size]       = Arb(gen.size)
  implicit def arbWord: Arb[String]     = Arb(gen.text.word)
  implicit def arbitraryPint: Arb[Pint] = Arb(gen.int ^^ Pint)
  implicit class LiftConverter[A](gen: Gen[A]) {
    def to[B](implicit f: A => B): Gen[B] = gen map f
  }

  implicit class Gen2Ops[A, B](g: (Gen[A], Gen[B])) {
    def filter(p: (A, B) => Boolean)       = Gen.zip(g._1, g._2) suchThat p.tupled
    def map[C](f: (A, B) => C): Gen[C]     = Gen.zip(g._1, g._2) map f.tupled
    def ^^[C](f: (A, B) => C): Gen[C]      = map(f)
    def >>[C](f: (A, B) => Gen[C]): Gen[C] = g._1 >> (x => g._2 >> (y => f(x, y)))
  }
  implicit class Gen3Ops[A, B, C](g: (Gen[A], Gen[B], Gen[C])) {
    def filter(f: (A, B, C) => Boolean)   = Gen.zip(g._1, g._2, g._3) suchThat f.tupled
    def map[D](f: (A, B, C) => D): Gen[D] = Gen.zip(g._1, g._2, g._3) map f.tupled
    def ^^[D](f: (A, B, C) => D): Gen[D]  = map(f)
  }
  implicit class TestIntOps(val x: Int) extends AnyVal {
    def upTo(max: Int): Gen[Int] = Gen.choose(x, max)
  }
  implicit class TestLongOps(val x: Long) extends AnyVal {
    def upTo(max: Long): Gen[Long] = Gen.choose(x, max)
  }

  trait GenTransform[CC[X], A] {
    def self: CC[A]
    def transform[B](f: Gen[A] => Gen[B]): CC[B]

    def ^^^[B](x: B): CC[B]                                               = transform(_ => Gen const x)
    def ^^[B](f: A => B): CC[B]                                           = transform(_ map f)
    def ?(p: A => Bool): CC[A]                                            = transform(_ filter p)
    def >>[B](f: A => Gen[B]): CC[B]                                      = transform(_ flatMap f)
    def +^^[B](f: A => B): CC[A->B]                                       = transform(_ map (x => x -> f(x)))
    def ^?[B](pf: A ?=> B): CC[B]                                         = transform(_ collect pf)
    def *(n: Int): CC[Vec[A]]                                             = transform(gen.directOfN(n, _))
    def *(r: Gen[Int]): CC[Vec[A]]                                        = transform(g => r flatMap (g * _))
    def zip[B](h: Gen[B]): CC[A -> B]                                     = transform(_ flatMap (x => h map (x -> _)))
    def zipWith[B, C](h: Gen[B])(f: (A, B) => C): CC[C]                   = transform(g => (g, h) map f)
    def collect[B](pf: A ?=> B): CC[B]                                    = transform(_ suchThat pf.isDefinedAt map pf.apply)
    def collectN[B](n: Int)(pf: Each[A] ?=> B)(implicit z: Arb[A]): CC[B] = transform(g => gen.eachOfN(n, g) collect pf)
  }

  implicit class ArbOps[A](val self: Arb[A]) extends GenTransform[Arb, A] {
    def transform[B](f: Gen[A] => Gen[B]): Arb[B] = Arb(f(self.arbitrary))
    def flatMap[B](f: A => Gen[B]): Arb[B]        = Arb(self.arbitrary flatMap f)
  }
  implicit class GenOps[A](val self: Gen[A]) extends GenTransform[Gen, A] {
    def transform[B](f: Gen[A] => Gen[B]): Gen[B] = f(self)
    def stream: Each[A]                           = Each continually self.sample flatMap (_.toVec)
    def take(n: Int): Vec[A]                      = stream take n toVec
  }

  implicit def chooseIndex: Choose[Index]  = Choose.xmap[Long, Index](Index, _.get)
  implicit def chooseSize: Choose[Precise] = Choose.xmap[Long, Precise](Finite, _.getLong)
  implicit def chooseNth: Choose[Nth]      = Choose.xmap[Long, Nth](Nth, _.get)

  def preNewline(s: String): String                               = if (s containsChar '\n') "\n" + s.mapLines("| " append _) else s
  def showsAs[A: Show](expected: String, x: A): NamedProp         = preNewline(expected) -> (expected =? show"$x")
  def seqShows[A: Show](expected: String, xs: Each[A]): NamedProp = preNewline(expected) -> (expected =? (xs mk_s ", "))

  def expectValue[A](expected: A)(x: A): NamedProp = x.any_s -> (expected =? x)

  def expectType(expected: jClass, found: jClass): NamedProp        = fshow"$expected%15s  >:>  $found%s" -> Prop(expected isAssignableFrom found)
  def expectTypes(expected: jClass, found: Each[jClass]): NamedProp = fshow"$expected%15s  >:>  $found%s" -> found.map(c => Prop(expected isAssignableFrom c))
  def expectType[A: CTag](result: A): NamedProp                     = expectType(classOf[A], result.getClass)
  def expectTypes[A: CTag](results: A*): NamedProp                  = expectTypes(classOf[A], results.toVec mapNow (_.getClass))

  implicit def buildsToBuildable[A, CC[X]](implicit z: Builds[A, CC[A]]): Buildable[A, CC] = new Buildable[A, CC] {
    def builder: scmBuilder[A, CC[A]] = z.scalaBuilder
  }

  implicit class PropOps(p: Prop) {
    def mapParams(f: ToSelf[TestParams]): Prop = new NamedProp.MapParams(p, f)
    def minSuccessful(size: Precise): Prop    = mapParams(_ withMinSuccessfulTests size.toInt)
    def unary_! : Prop                        = p map (r => !r)
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

  def printResult[A: Show](msg: String)(result: A): A              = doto(result)(r => println(show"$msg: $r"))
  def printResultIf[A: Show : Eq](x: A, msg: String)(result: A): A = doto(result)(r => if (r === x) println(show"$msg: $r"))

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  def observationalEq[M[X], A : Arb, B : Eq](f: (M[A], A) => B): Eq[M[A]] = Eq[M[A]] { (xs, ys) =>
    val prop = forAll((elem: A) => f(xs, elem) === f(ys, elem))
    (Test check prop)(identity).passed
  }
  implicit def pintEq: Hash[Pint]                                     = inheritEq
  implicit def pintShow: Show[Pint]                                   = inheritShow
  implicit def predicateEq[A : Arb] : Eq[InvariantPredicate[A]] = observationalEq[InvariantPredicate, A, Boolean](_ apply _)

  implicit object IdentityAlgebra extends BooleanAlgebra[Boolean] {
    def and(x: Boolean, y: Boolean): Boolean = x && y
    def or(x: Boolean, y: Boolean): Boolean  = x || y
    def complement(x: Boolean): Boolean      = !x
    def zero: Boolean                        = false
    def one: Boolean                         = true
  }
}

package tests {
  trait Assertions {
    def failed(msg: => String): Unit
    def assert(assertion: Boolean, msg: => String): Unit = if (!assertion) failed(msg)
  }
  object Assertions {
    private[this] var instance: Assertions = DefaultAssertions
    def using[A](x: Assertions)(assertion: => Boolean, msg: => String): Unit = {
      val saved = instance
      instance = x
      try instance.assert(assertion, msg) finally instance = saved
    }
    implicit object DefaultAssertions extends Assertions {
      def failed(msg: => String): Unit = assertionError(msg)
    }
  }
  object ImmediateTraceAssertions extends Assertions {
    def failed(msg: => String): Unit =
      new AssertionError(msg) |> (t => sideEffect(t.printStackTrace, throw t))
  }

  final case class Pint(x: Int) { override def toString = s"$x" }
}
