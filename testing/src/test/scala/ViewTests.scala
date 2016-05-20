package psp
package tests

import api._, std._, all._, StdShow._

class StringViewTests {
  val ad: String    = ('a' to 'd').m.joinString
  val da: String    = ad.reverseChars
  val adda1: String = view(ad, da).join
  val adda2: String = view(ad, "c", da).join
  val adda3: String = view(ad, "pa", da).join

  def split(s: String)            = s splitAtTake s.length / 2 mapRight (_.reverse)
  def isPalindrome(s: String)     = split(s).zip forall (_ === _)
  def isEvenPalindrome(s: String) = split(s) app (_ === _)

  @Test
  def stringTests(): Unit = {
    same(ad, ad.reverseChars.reverseChars)
    assert(isPalindrome(adda1), adda1)
    assert(isPalindrome(adda2), adda2)
    assert(!isEvenPalindrome(adda2), adda2)
    assert(!isPalindrome(adda3), adda3)
    sameDoc("[a].".r findAll adda3, "[ ab, ad ]")
    sameDoc("abcdefg" stripPrefix "a..", "abcdefg")
    sameDoc("abcdefg" stripPrefix "a..".r, "defg")
  }
}

class IntViewTests {
  val ints: IntRange = 1 to 10
  val ints3          = ints take 3
  val xints          = view(304, 106, 25)

  val splt  = ints take 6 partition (_ % 2 === 0)
  val even3 = splt.leftView
  val odd3  = splt.rightView
  val zip3  = splt.zip
  val cro9  = splt.cross

  def reverseInt = ?[Order[Int]].flip

  @Test
  def zipTests(): Unit = {
    sameDoc(ints take 3 zipTail, "[ 1 -> 2, 2 -> 3 ]")
    sameDoc((1 to 100000).zipTail drop 100 take 2, "[ 101 -> 102, 102 -> 103 ]")
    same(1 to 2 zip (4 to 5) map (_ + _), view(5, 7))
    same(zip3 map (_ - _), view(1, 1, 1))
    same(zip3 mapLeft (_ * 10) pairs, view(20 -> 1, 40 -> 3, 60 -> 5))
    same(zip3 mapRight (_ => 0) pairs, view(2 -> 0, 4 -> 0, 6 -> 0))
    same(splt.collate, view(2, 1, 4, 3, 6, 5))
    same(splt.join, view(2, 4, 6, 1, 3, 5))
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
    same(ints indexWhere (_ < 1), emptyValue[Index])
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
    same(ints max reverseInt, 1)
    same(ints filter (_ > 5) head, 6)
    same(ints filterNot (_ > 5) last, 5)
    same(ints grep "^[47]$".r head, 4)
    same(ints grep "^[47]$".r last, 7)
    same(ints.tail.head, 2)
    same(ints.init.last, 9)
    same(ints3.tails, view(view(1, 2, 3), view(2, 3), view(3), view()))
    same(ints3.inits, view(view(1, 2, 3), view(1, 2), view(1), view()))
    same(ints3.tails.flatten, view(1, 2, 3, 2, 3, 3))
    same(ints applyIndex Index(0), 1)
    same(ints applyIndex Nth(2), 2)
    same(ints sliceIndex Nth(2), view(2))
    same(ints sliceIndex Nth(20), view())
    same(ints sliceWhile (_ < 4, _ < 6), view(4, 5))
    same(ints sort reverseInt head, 10)
    same(ints mapIf { case 1 => -1 } size, Size(10))
    same(ints mapIf { case 1 => -1 } head, -1)
    same[View[Int]](ints.slice(Index(2), Size(2)), view(3, 4))
    same[View[Int]](ints.slice(Nth(2), Size(2)), view(2, 3))
    same[View[Int]](ints slice indexRange(1, 4), view(2, 3, 4))
    same[View[Int]](ints slice nthInclusive(3, 4), view(3, 4))
    same[View[Int]](ints drop 2 take 2, view(3, 4))
    same(1 to 3 map nth, 0 to 2 map index)
    same(nthInclusive(1, 3), indexRange(0, 3))
    same(ints takeToFirst (_ > 2), view(1, 2, 3))
    same(ints.span(_ < 4).collate, view(1, 4, 2, 5, 3, 6))
    same(cro9.force.size, Size(9))
    same(xints.sort, view(25, 106, 304))
    same(xints.sortBy(_.any_s), view(106, 25, 304))
    same(xints.sortBy(_.any_s.reverseBytes.utf8String), view(304, 25, 106))
    same(xints sortWith ((x, y) => longCmp(y - x)), view(304, 106, 25))
    same(ints3 splitAround nth(2) join, view(1, 3))
    same(ints3 dropIndex nth(2), view(1, 3))
    same(5 +: ints3 :+ 5, view(5, 1, 2, 3, 5))
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

  @Test
  def readmeShowTests(): Unit = {
    val xs = 1 to 20 splitAt index(10)
    val ys = zipCross(1 to 3, vec("a", "bb", "ccc"))
    val zs = ys filter (_ === _.length)

    sameDoc(xs, "Split([ 1, 2, 3, ... ], [ 11, 12, 13, ... ])")
    sameDoc(xs mapLeft (_ dropRight 8) join, "[ 1, 2, 11, ... ]")
    sameDoc(xs.zip filterRight (_ % 3 === 0), "[ 2 -> 12, 5 -> 15, 8 -> 18 ]")
    sameDoc(ys, "[ 1 -> a, 1 -> bb, 1 -> ccc, 2 -> a, 2 -> bb, 2 -> ccc, 3 -> a, 3 -> bb, 3 -> ccc ]")
    sameDoc(zs, "[ 1 -> a, 2 -> bb, 3 -> ccc ]")
    sameDoc(zs.rights joinWith '/', "a/bb/ccc")
  }
}
