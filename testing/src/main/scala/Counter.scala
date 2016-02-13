package psp
package tests

import std._, all._
import java.util.concurrent.atomic.AtomicInteger

trait Counter[+A] {
  def peek: A
  def next(): A
}

class IntCounter(start: Int) extends Counter[Int] {
  private[this] val atomicInt = new AtomicInteger(start)

  def peek: Int   = atomicInt.get
  def next(): Int = atomicInt.getAndIncrement
  override def toString = s"IntCounter(@ $peek)"
}

object Counter {
  def apply(start: Int): IntCounter = new IntCounter(start)

  class Mapped[A, B](counter: Counter[A], f: A => B) extends Counter[B] {
    def peek: B   = f(counter.peek)
    def next(): B = f(counter.next())
    override def toString = s"Counter.Mapped(@ $peek)"
  }
}

class RecorderCounter() {
  private[this] val seen = scmMap[Any, Int]() withDefaultValue 0
  def distinctCalls: Int = seen.size
  def totalCalls         = seen.values.sum
  def record[A](x: A): A = sideEffect(x, seen(x) += 1)
  override def toString  = s"$distinctCalls/$totalCalls"
}
