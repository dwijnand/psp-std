package psp
package tests

import std._, all._, StdShow._

class IntervalTests {
  val lo = Interval(10L)
  val lc = Interval(10L, 10.size)

  @Test
  def longTests(): Unit = {
    same(lo << 2, Interval(8L))
    same(lo >> 2, Interval(12L))
    same(lc << 2, Interval(8L, 10.size))
  }
}
