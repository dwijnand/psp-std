package psp
package tests

import api._, std._, all._, StdShow._

class IntViewTests {
  val ints: IntRange = 1 to 10
  val ints3          = ints take 3
  val xints          = view(304, 106, 25)

  def same[A : Eq : Show](expr: A, expected: A): Unit = {
    assert(expr === expected, pp"""
      |Expected: $expected
      |  Actual: $expr
      """)
  }

  @Test
  def noTypeClassNeededTests(): Unit = {
    same(ints count (_ < 3), 2)
    same(ints exists (_ < 10), true)
    same(ints exists (_ > 10), false)
    same(ints find (_ > 5), some(6))
    same(ints findOr(_ > 15, 20), 20)
    same(ints findOr(_ > 5, 20), 6)
    same(ints forall (_ < 10), false)
    same(ints forall (_ < 11), true)
    same(ints head, 1)
    same(ints indexWhere (_ < 1), NoIndex)
    same(ints indexWhere (_ > 1), Index(1))
    same(ints indexWhere (_ > 1), Nth(2))
    same(ints last, 10)
    same(ints reducel (_ + _), 55)
    same(ints reducer (_ + _), 55)
    same(ints3.foldl("x")((res, x) => pp"($res - $x)"), "(((x - 1) - 2) - 3)")
    same(ints3.foldr("x")((x, res) => pp"($x - $res)"), "(1 - (2 - (3 - x)))")
    same(ints3.foldl(0)(_ - _), -6)
    same(ints3.foldr(0)(_ - _), 2)
    same(ints.max, 10)
    same(ints max order[Int].flip, 1)
    same(ints filter (_ > 5) head, 6)
    same(ints filterNot (_ > 5) last, 5)
    same(ints grep "^[47]$".r head, 4)
    same(ints grep "^[47]$".r last, 7)
    same(ints.tail.head, 2)
    same(ints.init.last, 9)
    same((ints3.tails drop 1).head.head, 2)
    same((ints3.tails drop 2).head.head, 3)
    same(ints applyIndex Index(0), 1)
    same(ints applyIndex Nth(2), 2)
    same(ints sliceIndex Nth(2), view(2))
    same(ints sliceIndex Nth(20), view())
    same(ints sliceWhile (_ < 4, _ < 6), view(4, 5))
    same(ints sort order[Int].flip head, 10)
    same(ints mapIf { case 1 => -1 } size, Size(10))
    same(ints mapIf { case 1 => -1 } head, -1)
    same(ints.m.slice(Index(2), Size(2)), view(3, 4))
    same(ints.m.slice(Nth(2), Size(2)), view(2, 3))
    same(ints.m.slice(indexRange(1, 4)), view(2, 3, 4))
    same(ints.m slice nthInclusive(3, 4), view(3, 4))
    same(ints.m drop 2 take 2, view(3, 4))
    same(1 to 3 map nth, 0 to 2 map newIndex)
    same(nthInclusive(1, 3), indexRange(0, 3))
    same(ints takeToFirst (_ > 2), view(1, 2, 3))
    same(1 to 2 zip (4 to 5) map (_ + _), view(5, 7))
    same(ints.span(_ < 4).collate, view(1, 4, 2, 5, 3, 6))
    same(ints.partition(_ % 2 == 0).cross.force.size, Size(25))
    same(xints.sort, view(25, 106, 304))
    same(xints.sortBy(_.any_s), view(106, 25, 304))
    same(xints.sortBy(_.any_s.reverseBytes.utf8String), view(304, 25, 106))
    same(ints3 splitAround nth(2) join, view(1, 3))
    same(ints3 dropIndex nth(2), view(1, 3))
  }

  @Test
  def emptyNeededTests(): Unit = {
    implicit def emptyInt = Empty[Int](0)

    val pf1: Int ?=> String = { case 5 => "bob" }
    val pf2: Int ?=> String = { case 50 => "bob" }

    same(ints zfirst pf1, "bob")
    same(ints zfirst pf2, "")
    same(ints zhead, 1)
    same(ints zlast, 10)
    same(ints zreducel (_ + _), 55)
    same(ints zreducer (_ + _), 55)
    same(ints3 zfoldl[Int](_ - _), -6)
    same(ints3 zfoldr[Int](_ - _), 2)
  }
}
