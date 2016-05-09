package psp
package tests

import std._, api._, all._, StdShow._
import Prop.forAll
import Unsafe._

class MiscTests {
  @Test(expected = Predef.classOf[AssertionError])
  def junitFail(): Unit = junitAssert(false)
}

class EmptySpec extends ScalacheckBundle {
  def bundle = "Empty"
  class Bippy(val to_s: String) extends ShowSelf
  val eint = -123
  implicit def emptyBippy: Empty[Bippy] = Empty(new Bippy("-"))
  implicit def emptyInt: Empty[Int] = Empty(eint)

  def props = vec(
    seqShows("-, -, -, mom", vec[Bippy](
      sciList[Bippy]().m.zhead, vec[Bippy]().m.zhead,
      Option.empty[Bippy].zget, Some(new Bippy("mom")).zget)
    ),
    seqShows("0, 0, -1, -1, 0", vec[Long](
      emptyValue[jPath].any_s.length,
      emptyValue[jFile].any_s.length,
      emptyValue[Index].get,
      emptyValue[Nth].get,
      emptyValue[String].length)
    ),
    expectValue(eint)(view[Int]() zreducel (_ + _)),
    expectValue(eint)(view[Int]().zfoldl[Int](_ + _)),
    expectValue(3)(view(2, 3, 4) zreducer (_ - _)), // 2 - (3 - 4)
    expectValue(-5)(view(2, 3, 4) zreducel (_ - _)), // (2 - 3) - 4
    expectValue(7)(view(7) zreducel (_ * _)),
    expectValue(7)(view(7) zreducer (_ * _))
  )
}

class ArraySpec extends ScalacheckBundle {
  def bundle = "Array operations"

  val y = 123
  def props = vec(
    expectValue(y * 3)(Array(y, y, y).inPlace.shuffle.reducel(_ + _)),
    expectValue(y * y * y)(Array(y, y, y).inPlace.shuffle.m.reducel(_ * _))
  )
}

class ADTSpec extends ScalacheckBundle {
  def bundle = "ADTs defined in psp-api"

  val f1 = Opaque((_: Int) * 2)
  val f2 = f1 mapOut (_ * 3)
  val f3 = f2 filterIn (_ <= 2)
  val f4 = f3 defaulted (_ => 99)
  val xs = vec(1, 2, 3)

  var seen = ""
  val m1 = f1.traced(
    x => seen += s"f($x): ",
    x => seen += s"$x "
  )

  lazy val m1trace = {
    xs map m1 force;
    seen.trim
  }

  def props = vec(
    "size.+ is commutative"   -> commutative[Size](_ + _),
    "size.max is associative" -> associative[Size](_ max _),
    "size.max is commutative" -> commutative[Size](_ max _),
    "size.min is associative" -> associative[Size](_ min _),
    "size.min is commutative" -> commutative[Size](_ min _),
    "index/nth are consistent" -> forAll((x: Index) => x.indexValue == x.toNth.indexValue),
    "nth/index are consistent" -> forAll((x: Nth) => x.nthValue == x.toIndex.nthValue),
    seqShows("1, 1", vec(xs(Index(0)), xs(Nth(1)))),
    seqShows("2, 4, 6", xs map f1),
    seqShows("6, 12, 18", xs map f2),
    seqShows("6, 12", xs collect f3),
    seqShows("6, 12", xs collect f4),
    seqShows("6, 12, 99", xs map f4),
    showsAs("18", f2 get 3),
    showsAs("-", f3 get 3),
    showsAs("-", f4 get 3),
    showsAs("99", f4(3)),
    showsAs("f(1): 2 f(2): 4 f(3): 6", m1trace)
  )
}

class StringExtensions extends ScalacheckBundle {
  import scala.collection.immutable.StringOps

  def bundle = "String Extensions"

  def s = "123 monkey dog ^^.* hello mother 456"
  val pf: Char ?=> Char = { case 'a' => 'z' }

  def scalaOps(s: String) = new StringOps(s)

  def newProp[A: Eq](f: StringOps => A, g: String => A): Prop = forAll((s: String) => sameBehavior(f(scalaOps(s)), g(s)))

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
    expectValue("zbc")("abc" mapIf pf force),
    // expectValue("Bob Johnson")("bob\njohnson".mapLines(_.capitalize).lines mk_s ' '),
    expectValue("\u0001\u0002b\u0020b\u0003".sanitize)("??b b?")
  )
}

class GridSpec extends ScalacheckBundle {
  type LongGrid = View2D[Long]
  def bundle = "Grid Operations"

  def primePartition: LongGrid               = longsFrom(2).m mpartition (xs => _ % xs.head == 0)
  def primePartitionGrid(n: Int): LongGrid   = primePartition take n map (_ take n)
  def primePartitionGrid_t(n: Int): LongGrid = primePartition.transpose take n map (_ take n)

  def showGrid(xss: LongGrid): String = {
    val yss   = xss mmap (_.doc.render)
    val width = yss.flatten maxOf (_.length)
    val fmt   = lformat(width)

    (yss mmap fmt map (_ mk_s ' ') mk_s EOL).trimLines
  }
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
    showsAs(primePartition6, showGrid(primePartitionGrid(6))),
    showsAs(primePartition6_t, showGrid(primePartitionGrid_t(6)))
  )
}

class ViewBasic extends ScalacheckBundle {
  def bundle = "Views, Basic"

  def plist   = list(1, 2, 3)
  def pvector = vec(1, 2, 3)
  def parray  = Array(1, 2, 3)
  def pseq    = Each.elems(1, 2, 3)
  def punfold = intsFrom(1)

  case class Bippy(s: String, i: Int) {
    override def toString = s
  }

  // Testing different kinds of "distinct" calls.
  val s1 = new Bippy("abc", 1)
  val s2 = new Bippy("abc", 2)
  val s3 = new Bippy("def", 3)
  val s4 = new Bippy("def", 3)
  val strs = sciVector(s1, s2, s3, s4)

  // def closure              = transitiveClosure(parray)(x => view(x.init.force, x.tail.force))
  // def closureBag           = closure flatMap (x => x) toBag // That's my closure bag, baby
  def xxNumbers: View[Int] = intsFrom(0).m grep """^(.*)\1""".r

  def props = miscProps ++ vecProps ++ rangeProps

  lazy val rangeProps = {
    type Triple[A, B, C] = A -> (B -> C)
    type RTriple = Triple[LongRange, Index, Precise]

    // A size and and index each no greater than the halfway point lets
    // us formulate lots of relationships without creating out-of-bounds
    // conditions.
    val len  = 100
    val half = len / 2

    def pair(r: LongRange): Gen[Index -> Precise] = for {
      i <- 0 upTo half
      s <- 0 upTo half
    } yield Index(i) -> Size(s)

    implicit val arbRange = Arb[LongRange](Gen const (0 until len))
    implicit val arbTriple: Arb[RTriple] = arbRange flatMap (r => pair(r) flatMap (x => r -> x))
    implicit val emptyInt = Empty[Int](MinInt)

    vec[NamedProp](
      "take/drop vs. slice" -> sameOutcomes[Triple[LongRange, Int, Int], LongRange](
        { case (xs, (start, len)) => xs drop start take len },
        { case (xs, (start, len)) => xs.slice(start, max(start, 0).toLong + len) }
      ),
      "drop/apply" -> sameOutcomes[RTriple, Long](
        { case xs -> (idx -> size) => (xs drop size)(idx.getInt) },
        { case xs -> (idx -> size) => xs(idx + size.getLong) }
      ),
      "dropRight/apply" -> sameOutcomes[RTriple, Long](
        { case xs -> (idx -> size) => (xs dropRight size)(idx) },
        { case xs -> (idx -> size) => xs(idx) }
      ),
      "splitAt/drop" -> sameOutcomes[RTriple, View[Long]](
        { case xs -> (idx -> size) => xs.m splitAt idx onRight (_ drop size) },
        { case xs -> (idx -> size) => xs.m drop size splitAt idx onRight identity }
      ),
      expectValue(MinInt)(view[Int]().zhead),
      expectValue(5)(view(5).zhead)
      // Just to observe the scalacheck arguments being generated
      // , "dump" -> sameOutcomes[RTriple, Unit](
      //   { case xs -> (idx -> size) => { println(s"$xs -> ($idx -> $size)") ; () } },
      //   { case xs -> (idx -> size) => () }
      // )
    )
  }

  def miscProps = vec[NamedProp](
    showsAs("[ 1, 2, 3 ]", plist),
    showsAs("[ 1, 2, 3 ]", pvector),
    showsAs("[ 1, 2, 3 ]", parray),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", plist.m ++ plist.m force),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", pvector ++ pvector force),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray ++ parray force),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray.m ++ parray.m force),
    showsAs("[1..)", punfold),
    // showsAs("[ 1, 2, 3 ], [ 1, 2 ], [ 1 ], [  ], [ 2 ], [ 2, 3 ], [ 3 ]", closure mk_s ", "),
    // showsAs("1 -> 3, 2 -> 4, 3 -> 3", closureBag.entries.to[Vec[Int -> Precise]] mk_s ", "),
    seqShows("1 -> 0, 2 -> 1, 3 -> 2", pvector.zipIndex map (_ -> _)),
    seqShows("11, 22, 33, 44", indexRange(1, 50) grep """(.)\1""".r),
    seqShows("99, 1010, 1111", xxNumbers slice (8 takeNext 3).map(Index)),
    expectValue[Size](4)(strs.byRef.distinct.force.size),
    expectValue[Size](3)(strs.byEquals.distinct.force.size),
    expectValue[Size](2)(strs.byString.distinct.force.size)
  )

  lazy val vecProps = {
    val vec1  = Each const 1 take 32 toVec
    val vec2  = vec1 map (_ => vec1) reducel (_ ++ _)
    val vec3  = vec1 map (_ => vec2) reducel (_ ++ _)
    val vec4  = vec3 :+ 1
    val size4 = (32 * 32 * 32) + 1

    vec[NamedProp](
      expectValue[Int](vec4 drop 10 length)(size4 - 10),
      expectValue[Int](vec4 dropRight 10 length)(size4 - 10),
      expectValue[Int](vec4.updated(Index(100), 12345).apply(100))(12345),
      expectValue[Int](vec4 take size4 + 10 length)(size4),
      expectValue[Int](vec4 take size4 - 10 length)(size4 - 10),
      expectValue[Int](vec4 takeRight size4 - 10 length)(size4 - 10)
    )
  }
}

class ViewSplitZip extends ScalacheckBundle {
  def bundle = "Views, Split/Zip"

  def pvec     = 1 to 6 toVec
  def span     = pvec span (_ <= 3)
  def mod      = pvec partition (_ % 2 === 0)
  def zipped   = mod.zip
  def collated = mod.collate
  def sums     = zipped map (_ + _)

  def props: Direct[NamedProp] = vec(
    showsAs("[ 1, 2, 3 ]", span.leftView),
    showsAs("[ 4, 5, 6 ]", span.rightView),
    showsAs("[ 2, 4, 6 ]", mod.leftView),
    showsAs("[ 1, 3, 5 ]", mod.rightView),
    showsAs("[ 2, 4, 6 ]", zipped.lefts),
    showsAs("[ 1, 3, 5 ]", zipped.rights),
    showsAs("[ 2 -> 1, 4 -> 3, 6 -> 5 ]", zipped),
    showsAs("[ 20 -> 1, 40 -> 3, 60 -> 5 ]", zipped mapLeft (_ * 10)),
    showsAs("[ 3, 7, 11 ]", sums),
    showsAs("[ 3 ]", zipped filterLeft (_ == 4) rights),
    showsAs("[ 2, 4, 6, 1, 3, 5 ]", mod.join),
    showsAs("[ 2, 1, 4, 3, 6, 5 ]", collated),
    showsAs("6 -> 5", zipped findLeft (_ == 6)),
    showsAs("[ 2 -> 1 ]", zipped takeWhileFst (_ < 4)),
    showsAs("[ 5 -> 6 ]", zipped dropWhileSnd (_ < 4) map swap),
    showsAs("-", zipped findLeft (_ == 8)),
    seqShows("10 -> 2, 30 -> 4", zip(1 -> 2, 3 -> 4) mapLeft (_ * 10) force)
  )
}

class CollectionsSpec extends ScalacheckBundle {
  def bundle = "Type Inference, General"

  val arr  = Array[Int](1, 2, 3)
  val smap = sciMap("a" -> 1, "b" -> 2, "c" -> 3)
  val mmap = scmMap("a" -> 1, "b" -> 2, "c" -> 3)
  val sseq = sciSeq("a" -> 1, "b" -> 2, "c" -> 3)
  val svec = sciVector("a" -> 1, "b" -> 2, "c" -> 3)
  val sset = sciSet("a" -> 1, "b" -> 2, "c" -> 3)
  val jseq = Java.List("a" -> 1, "b" -> 2, "c" -> 3)
  val jset = Java.Set("a" -> 1, "b" -> 2, "c" -> 3)
  val jmap = Java.Map("a" -> 1, "b" -> 2, "c" -> 3)

  def paired[A](x: A): (A, Int) = x -> ("" + x).length

  def props = pspProps ++ javaProps ++ scalaProps ++ jvmProps

  def jvmProps = vec[NamedProp](
    expectTypes[String](
      make("abc")(_ map identity),
      make("abc")(_ map (_.toInt.toChar)),
      make("abc")(_ map (_.toInt) map (_.toChar)),
      make("abc")(_ flatMap (_.toString * 3)),
      "abc" map identity build,
      "abc" map (_.toInt.toChar) build,
      "abc" map (_.toInt) map (_.toChar) build,
      "abc" flatMap (_.toString * 3) build,
      "abc" flatMap (_.toString * 3) build,
      "abc" map identity flatMap ("" + _) build
    ),
    expectTypes[Array[Int]](
      make(Array(1, 2, 3))(_ map identity),
      make(Array(1, 2, 3))(_ flatMap (x => vec(x))),
      make(Array(1, 2, 3))(_ map (_.toString) map (_.toInt)),
      make(Array(1, 2, 3))(_ map (_.toString) flatMap (_.toString) map (_.toInt)),
      arr.inPlace map identity,
      arr.inPlace.reverse,
      arr ++ arr,
      arr.m ++ arr.m force,
      arr.m.build,
      arr flatMap (x => vec(x)) build,
      arr flatMap (x => list(x)) build,
      arr flatMap (x => view(x)) build,
      arr.m flatMap (x => vec(x)) build,
      arr.m flatMap (x => list(x)) build,
      arr.m flatMap (x => view(x)) build,
      arr.flatMap(x => view(x)).force[Array[Int]],
      arr.m.flatMap(x => vec(x)).force[Array[Int]]
    ),
    expectTypes[Array[Long]](
      make0[Array[Long]](1 to 10),
      make0[Array[Long]](1 to 10 m),
      make0[Array[Long]](1 to 10 toVec),
      make1[Array](1 to 10),
      make1[Array](1 to 10 m),
      make1[Array](1 to 10 toVec),
      make0[Array[Long]](1L to 10L),
      make0[Array[Long]](1L to 10L m),
      make0[Array[Long]](1L to 10L toVec),
      make1[Array](1L to 10L),
      make1[Array](1L to 10L m),
      make1[Array](1L to 10L toVec),
      arr.map(_.toLong).to[Array].inPlace.reverse
    )
  )

  def scalaProps = vec[NamedProp](
    expectTypes[sciSet[_]](
      sset map identity,
      sset.m build,
      sset.m map identity build,
      sset.m.map(fst) map paired build
      ),
    expectTypes[sciMap[_, _]](
      (smap map identity).force[sciMap[_, _]],
      smap.m build,
      smap.m map identity build,
      smap.m map fst map identity map paired build
      ),
    expectTypes[scmMap[_, _]](
      (mmap map identity).force[scmMap[_,_]],
      mmap.m build,
      mmap.m map identity build,
      mmap.m map fst map identity map paired build
      ),
    expectTypes[scSeq[_]](
      sseq map identity,
      sseq.m build,
      sseq.m map identity build,
      sseq.m.map(fst).map(paired).force[scSeq[_]]
      ),
    expectTypes[sciVector[_]](
      svec map identity,
      svec.m.build,
      svec.m map identity build,
      svec.m.map(fst).map(paired).force[sciVector[_]]
    )
  )

  def javaProps = {
    import Java._

    vec[NamedProp](
      expectTypes[jList[_]](
        jseq map identity build,
        jseq.m.build,
        jseq.m map identity build,
        jseq.m.map(fst).map(paired).force[jList[_]]
        ),
      expectTypes[jSet[_]](
        jset map identity build,
        jset.m build,
        jset.m map identity build,
        jset.m.map(fst) map paired build
        ),
      expectTypes[jMap[_, _]](
        (jmap map identity).force[jMap[_, _]]
        // jmap.m build,
        // jmap.m map identity build,
        // jmap.m map fst map identity map paired build
        //
        // After changing the package object to a regular object, the commented lines fail like:
        //
        // [error] could not find implicit value for parameter z: UnbuildsAs[this.Elem, jMap[String,Int]]
        // [error]         jmap.m build,
        // [error]         ^
        //
        // The "this.Elem" is a typical scala substitution bug.
      )
    )
  }

  def pspProps: Vec[NamedProp] = {
    val pset = set("a" -> 1, "b" -> 2, "c" -> 3)
    val pvec = vec("a" -> 1, "b" -> 2, "c" -> 3)

    vec(
      expectTypes[ExSet[_]](
        pset.build,
        pset map identity build,
        pset map fst map paired build,
        pset union pset
      ),
      expectTypes[Vec[_]](
        pvec map identity build,
        pvec ++ pvec,
        pvec.m ++ pvec.m build,
        pvec.tail.force,
        pvec.m.build,
        pvec.m map identity build,
        pvec.m.map(fst).map(paired).force[Vec[_]]
      )
    )
  }
}
