package psp
package tests

import std._, all._, StdShow._
import Prop.forAll

class MiscTests {
  @Test(expected = Predef.classOf[AssertionError])
  def junitFail(): Unit = junitAssert(false)

  @Test(expected = Predef.classOf[scala.NotImplementedError])
  def notImplementedError(): Unit = ???

  @Test
  def assertTrue(): Unit = assert(true, "boom")

  @Test(expected = Predef.classOf[IllegalArgumentException])
  def iae(): Unit = illegalArgumentException("boom")

  @Test(expected = Predef.classOf[IndexOutOfBoundsException])
  def ioobe(): Unit = indexOutOfBoundsException("boom")

  @Test(expected = Predef.classOf[NoSuchElementException])
  def nsee(): Unit = noSuchElementException("boom")
}

class ArraySpec extends ScalacheckBundle {
  def bundle = "Array operations"
  val ys: Array[Int] = 0 to 100 toArray

  def props = vec(
    expectValue(5050)(ys.toArray.inPlace.shuffle.reducel(_ + _))
  )
}

class ADTSpec extends ScalacheckBundle {
  def bundle = "ADTs defined in psp-api"

  val f1 = Fun((_: Int) * 2)
  val f2 = f1 andThen (_ * 3)
  val f3 = f2 filterIn (_ <= 2)
  val f4 = f3 defaulted (_ => 99)
  val xs = vec(1, 2, 3)

  var seen = ""
  val m1 = f1.traced(
    x => seen += pp"f($x): ",
    x => seen += pp"$x "
  )

  lazy val m1trace = {
    xs map m1.fn force;
    seen.trim
  }

  def props = vec(
    "size.+ is commutative"   -> commutative[Size](_ + _),
    "size.max is associative" -> associative[Size](Size.max),
    "size.max is commutative" -> commutative[Size](Size.max),
    "size.min is associative" -> associative[Size](Size.min),
    "size.min is commutative" -> commutative[Size](Size.min),
    "index/nth are consistent" -> forAll((x: Index) => x.indexValue === x.toNth.indexValue),
    "nth/index are consistent" -> forAll((x: Nth) => x.nthValue === x.toIndex.nthValue),
    seqShows("1, 1", vec(xs(_0), xs(Nth(1)))),
    seqShows("2, 4, 6", xs map f1.fn),
    seqShows("6, 12, 18", xs map f2.fn),
    seqShows("6, 12", xs collect f3.pf),
    seqShows("6, 12", xs collect f4.pf),
    seqShows("6, 12, 99", xs map f4.fn),
    showsAs("18", f2 get 3),
    showsAs("-", f3 get 3),
    showsAs("-", f4 get 3),
    showsAs("99", f4(3)),
    showsAs("f(1): 2 f(2): 4 f(3): 6", m1trace),
    showsAs("#1", Nth(1)),
    showsAs("0", Nth(1).indexValue),
    showsAs("0", Index(0)),
    showsAs("0", Index(0).indexValue),
    showsAs("0", (Nth(1): Vdex).indexValue),
    showsAs("[ #1, #2, #3 ]", 1 nthTo 3),
    showsAs("[ 0, 1, 2 ]", 0 indexUntil 3),
    showsAs("[ [ 0, 1, 2 ], [ 0, 1, 2 ] ]", vec(1 nthTo 3, 0 indexUntil 3))
  )
}

class StringExtensions extends ScalacheckBundle {
  import scala.collection.immutable.StringOps

  def bundle = "String Extensions"

  def s = "123 monkey dog ^^.* hello mother 456"
  val pf: Char ?=> Char = { case 'a' => 'z' }

  def scalaOps(s: String) = new StringOps(s)

  def newProp[A: Eq](f: StringOps => A, g: String => A): Prop =
    forAll((s: String) => sameBehavior(f(scalaOps(s)), g(s)))

  def newProp2[B] = new {
    def apply[R](f: (StringOps, B) => R)(g: (String, B) => R)(implicit z1: Arb[B], z2: Eq[R]): Prop =
    forAll((s: String, x: B) => sameBehavior(f(scalaOps(s), x), g(s, x)))
  }

  // dropRight and takeRight have the domain limited because of a scala bug with
  // take/dropRight with values around MinInt.
  def mostInts = arb[Int] filter (_ > MinInt + 5000)

  def props: Direct[NamedProp] = vec(
    "stripSuffix" -> newProp2[String](_ stripSuffix _)(_ stripSuffix _),
    "stripPrefix" -> newProp2[String](_ stripPrefix _)(_ stripPrefix _),
    "take"        -> newProp2[Int](_ take _)(_ take _ build),
    "drop"        -> newProp2[Int](_ drop _)(_ drop _ build),
    "takeRight"   -> newProp2[Int](_ takeRight _)(_ takeRight _ build)(mostInts, ?),
    "dropRight"   -> newProp2[Int](_ dropRight _)(_ dropRight _ build)(mostInts, ?),
    // Not quite the same - "0xc".toInt is 12 for us, exception for them. XXX.
    // "toInt"       -> newProp[Int](_.toInt, _.toInt),
    "tail"        -> newProp[String](_.tail, _.tail.force),
    "head"        -> newProp(_.head, _.head),
    "drop"        -> newProp[Char](_.head, _.head),
    "reverse"     -> newProp[String](_.reverse, _.reverseChars.force),
    expectValue("")("".capitalize),
    expectValue("Bob")("bob".capitalize),
    expectValue("Bob johnson")("bob johnson".capitalize),
    expectValue("Bob Johnson")("bob johnson" mapWords (_.capitalize)),
    expectValue("zbc")("abc" mapIf pf force),
    expectValue("Bob Johnson")("bob\njohnson".mapLines(_.capitalize).lines.joinWords),
    expectValue("\u0001\u0002b\u0020b\u0003".sanitize)("??b b?")
  )
}

class GridSpec extends ScalacheckBundle {
  def bundle = "Grid Operations"

  def primePartition               = 2L.andUp mpartition { case HeadTailView(n, _) => _ % n === 0 }
  def primePartitionGrid(n: Int)   = primePartition take n map (_ take n)
  def primePartitionGrid_t(n: Int) = primePartition.transpose take n map (_ take n)

  def primePartition6 = sm"""
  |2   4   6   8   10  12
  |3   9   15  21  27  33
  |5   25  35  55  65  85
  |7   49  77  91  119 133
  |11  121 143 187 209 253
  |13  169 221 247 299 377
  """
  def primePartition6_t = sm"""
  |2   3   5   7   11  13
  |4   9   25  49  121 169
  |6   15  35  77  143 221
  |8   21  55  91  187 247
  |10  27  65  119 209 299
  |12  33  85  133 253 377
  """

  def props = vec(
    seqShows("[ 2, 4, 6, ... ], [ 3, 9, 15, ... ], [ 5, 25, 35, ... ]", primePartition take 3),
    showsAs(primePartition6, primePartitionGrid(6).grid_s),
    showsAs(primePartition6_t, primePartitionGrid_t(6).grid_s)
  )
}
