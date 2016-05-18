package psp
package tests

import Gen._
import psp._, std._, all._, api._, StdShow._

package gen {
  class TextGenerator(val letter: Gen[Char], charsInWord: Gen[Int], wordsInLine: Gen[Int]) {
    def word: Gen[String]                   = letter * charsInWord ^^ (_ join_s)
    def line: Gen[String]                   = word * wordsInLine ^^ (_ mk_s ' ')
    def nLines(n: Int): Gen[Direct[String]] = line * n
  }
  object text extends TextGenerator(alphaNumChar, intUpTo(1, 8), intUpTo(3, 7))
}

package object gen {
  def directOfN[A](n: Int, g: Gen[A]): Gen[Vec[A]] = containerOfN[Vec, A](n, g)(?, _.trav)
  def directOf[A](g: Gen[A]): Gen[Vec[A]]          = containerOf[Vec, A](g)(?, _.trav)
  def eachOfN[A](n: Int, g: Gen[A]): Gen[Each[A]]  = containerOfN[Each, A](n, g)(?, _.trav)
  def eachOf[A](g: Gen[A]): Gen[Each[A]]           = containerOf[Each, A](g)(?, _.trav)

  def index: Gen[Index]         = frequency(10 -> zeroPlusIndex, 1 -> NoIndex)
  def int: Gen[Int]             = intUpTo(MinInt, MaxInt)
  def zeroPlusIndex: Gen[Index] = 0L upTo MaxLong map Index

  def longRange(start: Gen[Long], end: Gen[Long]): Gen[LongRange] = (start, end) >> (_ to _)
  def indexRangeFrom(sMax: Long, eMax: Long): Gen[VdexRange]      = longRange(0 upTo sMax, 0 upTo eMax) ^^ (_ map Index)

  def precise: Gen[Precise] = chooseNum(1, MaxInt / 2) map (x => Size(x))
  def atomic: Gen[Atomic]   = frequency(10 -> precise, 1 -> Size.Zero, 1 -> Infinite)
  def bounded: Gen[Bounded] = precise.flatMap(lo => atomic map (hi => Size.Range(lo, hi))) collect classFilter[Bounded]
  def size: Gen[Size]       = oneOf(atomic, bounded)
}
