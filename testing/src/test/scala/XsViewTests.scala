package psp
package tests

import api._, std._
import all._
import StdShow._, Makes._

class RepViewTests {
  type DView[A] = RepView[Direct[A], A]
  type DVInt    = DView[Int]
  type VInt     = View[Int]
  type VVInt    = View[VInt]
  type VIntInt  = View[Int->Int]

  def mkInts(lo: Int, hi: Int): DVInt = RepView(lo to hi)

  val ints: DVInt  = mkInts(1, 10)
  val ints3: DVInt = ints take 3
  val xints: DVInt = RepView(vec(304, 106, 25))
  val splt         = ints take 6 partition (_ % 2 === 0)
  val even3        = splt.leftView
  val odd3         = splt.rightView
  val zip3         = splt.zip
  val cro9         = splt.cross

  def view[A](xs: A*): DView[A] = RepView(vec(xs: _*))
  def reverseInt = ?[Order[Int]].flip

  @Test
  def collectionTests(): Unit = {
    same(scalaList(1, 1, 2, 2, 3) o (_.to[sciSet]) sorted, scalaList(1, 2, 3))
    sameDoc(scalaList(1, 2, 3), "List(1, 2, 3)")
    sameDoc(scala.Vector(1, 2, 3), "Vector(1, 2, 3)")
    sameDocAsToString(javaSet(1, 2, 3))
    sameDocAsToString(javaList(1, 2, 3))
    sameDocAsToString(javaMap(1 -> 2, 3 -> 4))
    sameDocAsToString(scalaSet(1, 2, 3))
    sameDocAsToString(scalaList(1, 2, 3))
    sameDocAsToString(scalaMap(1 -> 2, 3 -> 4))
    sameDoc(elems(1, 2, 3): Pset[Int], "{1, 2, 3}")
    sameDoc(1 :: 2 :: 3 :: Pnil(), "[ 1, 2, 3 ]")
    // sameDoc(pmap(1 -> 2, 3 -> 4), "{1: 2, 3: 4}")
  }

  @Test
  def zipTests(): Unit = {
    same[VIntInt]((ints take 3).zipTail.pairs, view(1 -> 2, 2 -> 3))// zipViews(1 to 2, 2 to 3)) // "[ 1 -> 2, 2 -> 3 ]")
    sameDoc(mkInts(1, 100000).zipTail drop 100 take 2, "[ 101 -> 102, 102 -> 103 ]")
    same(1 to 2 zip (4 to 5) map (_ + _), view(5, 7))
    same[DVInt](zip3 map (_ - _), view(1, 1, 1))
    same[VIntInt](zip3 mapLeft (_ * 10) pairs, view(20 -> 1, 40 -> 3, 60 -> 5))
    same[VIntInt](zip3 mapRight (_ => 0) pairs, view(2 -> 0, 4 -> 0, 6 -> 0))
    same[DVInt](splt.collate, view(2, 1, 4, 3, 6, 5))
    same[DVInt](splt.join, view(2, 4, 6, 1, 3, 5))
    same(zip3 corresponds (_ > _), true)
    same(zipViews(ints3, ints3) corresponds (_ >= _), true)
    same(zipViews(ints3, ints3 :+ 8) corresponds (_ >= _), false)
  }

  @Test
  def noTypeClassNeededTests(): Unit = {
    same(ints count (_ < 3), 2)
    same(ints exists (_ < 10), true)
    same(ints exists (_ > 10), false)
    same(ints find (_ > 5), some(6))
    same(ints find (_ > 15) or 20, 20)
    same(ints find (_ > 5) or 20, 6)
    same(ints forall (_ < 10), false)
    same(ints forall (_ < 11), true)
    same(ints head, 1)
    same(ints indexWhere (_ < 1), Index.invalid)
    same[Vdex](ints indexWhere (_ > 1), _1)
    same(ints indexWhere (_ > 1), Nth(2))
    same(ints last, 10)
    same(ints reducel (_ + _), 55)
    same(ints reducer (_ + _), 55)
    same(ints3.foldl("x")((res, x) => pp"($res - $x)"), "(((x - 1) - 2) - 3)")
    same(ints3.foldr("x")((x, res) => pp"($x - $res)"), "(1 - (2 - (3 - x)))")
    same(ints3.foldl(0)(_ - _), -6)
    same(ints3.foldr(0)(_ - _), 2)
    same(ints.max, 10)
    same(ints max reverseInt, 1)
    same(ints filter (_ > 5) head, 6)
    same(ints filterNot (_ > 5) last, 5)
    same(ints grep "^[47]$".r head, 4)
    same(ints grep "^[47]$".r last, 7)
    same(ints.tail.head, 2)
    same(ints.init.last, 9)
    same(ints applyIndex _0, 1)
    same(ints applyIndex Nth(2), 2)
    same(ints sliceIndex Nth(2), view(2))
    same(ints sliceIndex Nth(20), view())
    same(ints sliceWhile (_ < 4, _ < 6), view(4, 5))
    same(ints sort reverseInt head, 10)
    same(ints.toVec o (_ mapIf { case 1 => -1 }) size, Size(10))
    same(ints mapIf { case 1 => -1 } head, -1)
    same(ints.slice(Index(2), Size(2)), view(3, 4))
    same(ints.slice(Nth(2), Size(2)), view(2, 3))
    same(1 to 3 map (_.nth), 0 to 2 map (_.index))
    same(1 nthTo 3, 0 indexUntil 3)
    same(cro9.force.size, Size(9))
    same(xints.sort, view(25, 106, 304))
    same(xints.sortBy(_.any_s), view(106, 25, 304))
    same(xints.sortBy(_.any_s.reverseBytes.utf8String), view(304, 25, 106))
    same(xints sortWith ((x, y) => longCmp(y - x)), view(304, 106, 25))

    // Same here after making Split a nested class.
    same[DVInt](ints takeToFirst (_ > 2), view(1, 2, 3))
    same[DVInt](ints.span(_ < 4).collate, view(1, 4, 2, 5, 3, 6))
    same[DVInt](ints3 splitAround Nth(2) join, view(1, 3))
    same[DVInt](ints3 dropIndex Nth(2), view(1, 3))

    // [error] /g/psp-std/testing/src/test/scala/XsViewTests.scala:85:
    //           diverging implicit expansion for type psp.api.Eq[psp.std.RepView[psp.std.Direct[Int],Int]]
    // [error] starting with method enumRelation in trait StdRelation1
    // [error]     same(ints drop 2 take 2, view(3, 4))
    // [error]         ^
    same[DVInt](ints slice (1 indexUntil 4), view(2, 3, 4))
    same[DVInt](ints slice (3 nthTo 4), view(3, 4))
    same[DVInt](ints drop 2 take 2, view(3, 4))
    same[DVInt](5 +: ints3 :+ 5, view(5, 1, 2, 3, 5))

    same[VVInt](ints3.tails, view(view(1, 2, 3), view(2, 3), view(3), view()))
    same[VVInt](ints3.inits, view(view(1, 2, 3), view(1, 2), view(1), view()))
    same[VInt](ints3.tails.flatten, view(1, 2, 3, 2, 3, 3))
  }
}
