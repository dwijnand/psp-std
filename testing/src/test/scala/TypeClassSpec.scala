package psp
package tests

import std._, api._, all._, StdShow._
import Unsafe.promoteIndex

class EmptySpec extends ScalacheckBundle {
  def bundle = "Empty"
  class Bippy(val to_s: String) extends ShowSelf
  val eint = -123

  implicit def emptyBippy: Empty[Bippy] = Empty(new Bippy("-"))
  implicit def emptyInt: Empty[Int]     = Empty(eint)

  def props = vec(
    seqShows(
      "-, -, hi, hi, -, hi, -, hi",
      vec[Bippy](
        sciList[Bippy]().zhead,
        sciList[Bippy]().zlast,
        sciList(new Bippy("hi")).zhead,
        sciList(new Bippy("hi")).zlast,
        vec[Bippy]().zhead,
        vec(new Bippy("hi")).zhead,
        none[Bippy].zget,
        some(new Bippy("hi")).zget
      )
    ),
    seqShows(
      "0, 0, -1, -1, 0",
      vec[Long](
        emptyValue[jPath].any_s.length,
        emptyValue[jFile].any_s.length,
        emptyValue[Index].indexValue,
        emptyValue[Nth].indexValue,
        emptyValue[String].length
      )
    ),
    expectValue(eint)(view[Int]() zreducel (_ + _)),
    expectValue(eint)(view[Int]().zfoldl[Int](_ + _)),
    expectValue(3)(view(2, 3, 4) zreducer (_ - _)), // 2 - (3 - 4)
    expectValue(-5)(view(2, 3, 4) zreducel (_ - _)), // (2 - 3) - 4
    expectValue(7)(view(7) zreducel (_ * _)),
    expectValue(7)(view(7) zreducer (_ * _))
  )
}

class ViewBasic extends ScalacheckBundle {
  def bundle = "Views, Basic"

  def plist: Plist[Int]  = elems(1, 2, 3)
  def pvector: Vec[Int]  = elems(1, 2, 3)
  def parray: Array[Int] = arr(1, 2, 3)
  def pseq: Each[Int]    = elems(1, 2, 3)
  def punfold: Interval  = Interval open 1

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
  def xxNumbers: View[Int] = openRange(0)(_.toInt).m grep """^(.*)\1""".r

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
      i <- gen.genLong(0, half)
      s <- gen.genLong(0, half)
    } yield Index(i) -> Size(s)

    implicit val arbRange = Arb[LongRange](Gen const (0L until len))
    implicit val arbTriple: Arb[RTriple] = arbRange flatMap (r => pair(r) flatMap (x => r -> x))
    implicit val emptyInt = Empty[Int](MinInt)

    vec[NamedProp](
      "take/drop vs. slice" -> sameOutcomes[Triple[LongRange, Int, Int], LongRange](
        { case (xs, (start, len)) => xs drop max(start, 0) take len },
        { case (xs, (start, len)) => xs.slice(Index(max(start, 0)), Size(len)) }
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
        { case xs -> (idx -> size) => xs.m splitAt idx appRight (_ drop size) },
        { case xs -> (idx -> size) => xs.m drop size splitAt idx appRight identity }
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
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", plist ++ plist),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", plist.m ++ plist.m),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray ++ parray),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray.m ++ parray.m),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", pvector ++ pvector),
    showsAs("[1..)", punfold),
    // showsAs("[ 1, 2, 3 ], [ 1, 2 ], [ 1 ], [  ], [ 2 ], [ 2, 3 ], [ 3 ]", closure mk_s ", "),
    // showsAs("1 -> 3, 2 -> 4, 3 -> 3", closureBag.entries.to[Vec[Int -> Precise]] mk_s ", "),
    seqShows("1 -> 0, 2 -> 1, 3 -> 2", pvector.zipIndex map (_ -> _)),
    seqShows("11, 22, 33, 44", indexRange(1, 50) grep """(.)\1""".r),
    seqShows("99, 1010, 1111", xxNumbers.slice(8, Size(3))),
    expectValue[Size](4)(strs.byRef.distinct.force.size),
    expectValue[Size](3)(strs.byEquals.distinct.force.size),
    expectValue[Size](2)(strs.byToString.distinct.force.size)
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
