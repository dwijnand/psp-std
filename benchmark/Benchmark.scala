package psp
package bench

import std._, all._, Unsafe.promoteIndex
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

object Values {
  final val range      = 1 to 1000
  final val scalaElems = range.to[sciVector]
  final val pspElems   = range.to[Vec]
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 200, timeUnit = TimeUnit.MILLISECONDS)
class ScalaVectorBenchmarker {
  def elems = Values.scalaElems

  @Benchmark
  def fold_sum(): Long = elems.foldLeft(0L)(_ + _)

  @Benchmark
  def fold_reverse(): sciVector[Int] = elems.foldRight(sciVector[Int]())(_ +: _)

  @Benchmark
  def apply_sum(): Long = {
    val xs = elems
    var total = 0L
    var i = 0
    while (i < xs.length) {
      total += xs(i)
      i += 1
    }
    total
  }

  @Benchmark
  def drop_last(): Int = {
    var xs = elems
    var total = 0
    while (!xs.isEmpty) {
      total += xs.last
      xs = xs dropRight 1
    }
    total
  }

  @Benchmark
  def drop_head(): Int = {
    var xs = elems
    var total = 0
    while (!xs.isEmpty) {
      total += xs.head
      xs = xs drop 1
    }
    total
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 200, timeUnit = TimeUnit.MILLISECONDS)
class PspVectorBenchmarker {
  def elems = Values.pspElems

  @Benchmark
  def fold_sum(): Long = elems.foldl(0L)(_ + _)

  @Benchmark
  def fold_reverse(): Vec[Int] = elems.zfoldr[Vec[Int]](_ +: _)

  @Benchmark
  def apply_sum(): Long = {
    val xs = elems
    var total = 0L
    var i = 0
    while (i < xs.length) {
      total += xs(i)
      i += 1
    }
    total
  }

  @Benchmark
  def drop_last(): Int = {
    var xs = elems
    var total = 0
    while (!xs.isEmpty) {
      total += xs.last
      xs = xs dropRight 1
    }
    total
  }

  @Benchmark
  def drop_head(): Int = {
    var xs = elems
    var total = 0
    while (!xs.isEmpty) {
      total += xs.head
      xs = xs drop 1
    }
    total
  }
}
