package psp
package tests

import Gen._
import psp._, std._, all._, api._

object gen {
  class TextGenerator(val letter: Gen[Char], charsInWord: Gen[Int], wordsInLine: Gen[Int]) {
    def word: Gen[String]                 = letter * charsInWord ^^ (x => new String(x.toArray))
    def line: Gen[String]                 = word * wordsInLine ^^ (_.m.joinWords)
    def nLines(n: Int): Gen[View[String]] = line * n
  }
  object text extends TextGenerator(alphaNumChar, range(1, 8), range(3, 7))

  def range(lo: Int, hi: Int): Gen[Int]    = Gen.choose(lo, hi)
  def range(lo: Long, hi: Long): Gen[Long] = Gen.choose(lo, hi)


  def index: Gen[Index]         = frequency(10 -> zeroPlusIndex, 1 -> emptyValue[Index])
  def int: Gen[Int]             = range(MinInt, MaxInt)
  def zeroPlusIndex: Gen[Index] = genIndex(0, MaxLong)

  // def genInt(lo: Int, hi: Int): Gen[Int]                       = Gen.choose(lo, hi)
  // def genLong(lo: Long, hi: Long): Gen[Long]                   = Gen.choose(lo, hi)
  def genIndex(lo: Long, hi: Long): Gen[Index]                    = range(lo, hi) map Index
  def longRange(start: Gen[Long], end: Gen[Long]): Gen[LongRange] = (start zipWith end)(_ to _)
  def indexRangeFrom(s: Long, e: Long): Gen[VdexRange]            = (range(0, s) zipWith range(0, e))(indexRange)

  // longRange(range(0, sMax), range(0, eMax)) ^^ (_ map Index)

  import Size._
  def precise: Gen[Precise] = chooseNum(1, MaxInt / 2) map (x => Size(x))
  def atomic: Gen[Atomic]   = frequency(10 -> precise, 1 -> _0, 1 -> Infinite)
  def bounded: Gen[Size]    = ( for (lo <- precise ; hi <- atomic) yield Range(lo, hi) ) collect classFilter[Bounded]
  def size: Gen[Size]       = oneOf(atomic, bounded)
}

/** Bridging the pointless existence of both Arb and Gen.
 */
trait GenTransform[M[X], A] {
  type This = M[A]

  def self: M[A]
  def transform[B](f: Gen[A] => Gen[B]): M[B]

  def ^^^[B](x: B): M[B]          = transform(_ => Gen const x)
  def ^^[B](f: A => B): M[B]      = transform(_ map f)
  def ?(p: ToBool[A]): M[A]       = transform(_ filter p)
  def >>[B](f: A => Gen[B]): M[B] = transform(_ flatMap f)
  def ^?[B](pf: A ?=> B): M[B]    = transform(_ collect pf)
  def *(n: Int): M[View[A]]       = transform(_ container[View] n)
  def *(r: Gen[Int]): M[View[A]]  = transform(r >> _.*)

  def zip[B](h: Gen[B]): M[A -> B]                   = zipWith(h)(_ -> _)
  def zipWith[B, C](h: Gen[B])(f: (A, B) => C): M[C] = transform(_ -> h map f)
  def collect[B](pf: A ?=> B): M[B]                  = transform(_ suchThat pf.contains map pf)
}
