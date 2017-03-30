package psp
package tests

import Gen._
import psp._, std._, all._, Size._

object gen {
  class TextGenerator(val letter: Gen[Char], charsInWord: Gen[Int], wordsInLine: Gen[Int]) {
    def word: Gen[String]                 = letter * charsInWord ^^ (x => new String(x.toArray))
    def line: Gen[String]                 = word * wordsInLine ^^ (_.joinWords)
    def nLines(n: Int): Gen[View[String]] = line * n
  }
  object text extends TextGenerator(alphaNumChar, 1 upTo 8, 3 upTo 7)

  def atomic: Gen[Atomic]   = frequency(50 -> precise, 1 -> Infinite)
  def bounded: Gen[Size]    = (for (lo <- precise; hi <- atomic) yield Range(lo, hi)) collect classFilter[Bounded]
  def index: Gen[Index]     = frequency(10 -> (nats ^^ Index), 1 -> emptyValue[Index])
  def ints: Gen[Int]        = MinInt upTo MaxInt
  def nats: Gen[Long]       = 0L upTo MaxLong
  def precise: Gen[Precise] = frequency(1 -> Zero, 1 -> One, 1 -> Size(MaxInt.toLong * 2), 20 -> (1 upTo 1000 map (_.size)))
  def size: Gen[Size]       = oneOf(atomic, bounded)
}

/** Bridging the pointless existence of both Arb and Gen.
  */
trait GenTransform[M[X], A] {
  type This = M[A]

  def self: M[A]
  def transform[B](f: Gen[A] => Gen[B]): M[B]

  def ^^^[B](x: B): M[B]     = transform(_ => Gen const x)
  def ^^[B](f: A => B): M[B] = transform(_ map f)
  def reduce(f: BinOp[A]): M[A] = transform(_ reduce f)
  def ?(p: ToBool[A]): M[A]     = transform(_ filter p)
  def >>[B](f: A => Gen[B]): M[B] = transform(_ flatMap f)
  def ^?[B](pf: A ?=> B): M[B]   = transform(_ collect pf)
  def *(n: Int): M[View[A]]      = transform(Gen.containerOfN[View, A](n, _))
  def *(r: Gen[Int]): M[View[A]] = transform(r >> _.*)

  def zip[B](h: Gen[B]): M[A -> B]                   = zipWith(h)(_ -> _)
  def zipWith[B, C](h: Gen[B])(f: (A, B) => C): M[C] = transform(_ -> h map f)
  def collect[B](pf: A ?=> B): M[B]                  = transform(_ suchThat pf.contains map pf)
}
