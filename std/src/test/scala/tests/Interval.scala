package psp
package tests

import std._, all._, StdShow._

class IntervalTests {
  val lo = Interval open 10L
  val lc = Interval.to(10L, 20L)

  @Test
  def longTests(): Unit = {
    same(lo << 2, Interval open 8L)
    same(lo >> 2, Interval open 12L)
    same(lc << 2, Interval.to(8L, 18L))
  }
}
