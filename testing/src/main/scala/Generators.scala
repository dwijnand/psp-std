package psp
package tests

import Gen._
import psp._, std._, all._, api._, StdShow._

package gen {
  class RegexGenerator(val letter: Gen[Char]) {
    def alternative: Gen[Regex]       = (atom, atom) ^^ (_ | _)
    def anchor(c: Char): Gen[String]  = frequency(3 -> "", 1 -> c.toString)
    def apply(): Gen[Regex]           = (concatenate, anchor('^'), anchor('$')) map ((re, s, e) => re.surround(s, e))
    def atom: Gen[Regex]              = oneOf(literal, range, quantified)
    def concatenate: Gen[Regex]       = simple * intUpTo(1, 3) ^^ (_.m reducel (_ | _))
    def group: Gen[Regex]             = atom ^^ (_.capturingGroup)
    def letterPair: Gen[Char -> Char] = (letter, letter) filter (_ <= _)
    def literal: Gen[Regex]           = letter * intUpTo(0, 5) ^^ (_.join_s.r)
    def quantified: Gen[Regex]        = (literal zipWith oneOf("+?*".charSeq))(_ append _.to_s)
    def range: Gen[Regex]             = (oneOf("", "^"), letterPair) map { case (neg, (s, e)) => s"[$neg$s-$e]".r }
    def simple: Gen[Regex]            = oneOf(atom, group, alternative)
  }
  class TextGenerator(val letter: Gen[Char], charsInWord: Gen[Int], wordsInLine: Gen[Int]) {
    def word: Gen[String]                   = letter * charsInWord ^^ (_ join_s)
    def line: Gen[String]                   = word * wordsInLine ^^ (_ mk_s ' ')
    def nLines(n: Int): Gen[Direct[String]] = line * n
  }

  object regex extends RegexGenerator(alphaLowerChar)
  object text extends TextGenerator(alphaNumChar, intUpTo(1, 8), intUpTo(3, 7))
}

package object gen {
  def chooseFrom[A](xs: Direct[Gen[A]]): Gen[A]      = indexFrom(xs.size.indices) >> xs.apply
  def chooseFrom[A](xs: Gen[A]*): Gen[A]             = chooseFrom(xs.toVec)
  def directOfN[A](n: Int, g: Gen[A]): Gen[Vec[A]]   = containerOfN[Vec, A](n, g)(?, _.trav)
  def directOf[A](g: Gen[A]): Gen[Vec[A]]            = containerOf[Vec, A](g)(?, _.trav)
  def eachOfN[A](n: Int, g: Gen[A]): Gen[Each[A]]    = containerOfN[Each, A](n, g)(?, _.trav)
  def eachOf[A](g: Gen[A]): Gen[Each[A]]             = containerOf[Each, A](g)(?, _.trav)
  def pick[A](n: Int, xs: A*): Gen[Direct[A]]        = pick[A](n, xs.toVec)
  def pick[A](n: Int, xs: Direct[A]): Gen[Direct[A]] = Gen.pick(n, xs.seq) map (_.toVec)

  def indexTo(max: Int): Gen[Index] = (0 upTo max) ^^ (n => Index(n))
  def index: Gen[Index]             = frequency(10 -> zeroPlusIndex, 1 -> NoIndex)
  def int: Gen[Int]                 = intUpTo(MinInt, MaxInt)
  def long: Gen[Long]               = MinLong upTo MaxLong
  def posInt: Gen[Int]              = intUpTo(1, MaxInt)
  def posLong: Gen[Long]            = 1L upTo MaxLong
  def zeroPlusIndex: Gen[Index]     = zeroPlusLong map Index
  def zeroPlusInt: Gen[Int]         = intUpTo(0, MaxInt)
  def zeroPlusLong: Gen[Long]       = 0L upTo MaxLong

  def intRange(start: Gen[Int], end: Gen[Int]): Gen[IntRange]     = (start, end) >> all.intRange
  def longRange(start: Gen[Long], end: Gen[Long]): Gen[LongRange] = (start, end) >> (_ to _)
  def letterFrom(s: String): Gen[Char]                            = oneOf(s.charSeq)
  def indexFrom[A](r: ClosedRange[A]): Gen[Vdex]                  = frequency(1 -> NoIndex, 1 -> Index(0), 20 -> oneOf(r.size.indices.seq), 1 -> r.size.lastIndex.next)
  def indexRangeFrom(sMax: Long, eMax: Long): Gen[VdexRange]      = longRange(0 upTo sMax, 0 upTo eMax) ^^ (_ map Index)
  def validIndexFrom(r: IntRange): Gen[Vdex]                      = oneOf(r.size.indices.seq)

  def precise: Gen[Precise] = chooseNum(1, MaxInt / 2) map (x => Size(x))
  def atomic: Gen[Atomic]   = frequency(10 -> precise, 1 -> Size.Zero, 1 -> Infinite)
  def bounded: Gen[Bounded] = precise.flatMap(lo => atomic map (hi => Size.Range(lo, hi))) collect classFilter[Bounded]
  def size: Gen[Size]       = oneOf(atomic, bounded)
}
