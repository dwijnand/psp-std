psp view
========

An alternative to the unwieldy scala-library collections. All operations are
lazy. Computation is deferred for as long as possible. It's still under
development. Please don't use it unless you understand what that means.

Sample slightly reformatted test output:

| Ops                 | Linear    | Direct   | Result      |
| :--                 | :--:      | :--:     | :--         |
| dropR 77, x=>(x, x) | 79 <= 100 | 2 <= 100 | [ 1, 1, 2 ] |

A scala collection and a comparable psp collection apply the same operations
to the sequence 1..100 and the executed steps are counted and compared. The
line shown above shows an example: dropRight 77 followed by a flatMap
duplicating each element. In both cases scala gets the "maximum score" by
walking every element in the original collection. In psp the minimum is walked
in each case.

The tests fail if there is any disagreement as to the result sequence among {
psp, scala } x { linear, direct }, or if any psp collection ever requires more
operations than the corresponding scala collection.

**Linear** is a sequential access sequence, similar to scala's List. Due to
that access pattern, if elements must be taken from the right side of the list
it will fare as poorly as an eager collection. This operation requires 79
steps despite taking from the left because it must walk enough of the stream
to establish that the elements are present. (If the list contained 77
elements, then the final result would be empty, but this can't be known
without walking those 77 elements.)

**Direct** is a random access sequence, similar to scala's Vector or an Array.
In this case it doesn't even require three steps to obtain three elements;
zero are required for dropRight because the size is known, and only two must
be traversed to obtain the elements because the first flatMap produces two.

Performance
===========

As dramatic as the results of a test run are, they severely understate the
difference. Since we only count operations against the original collection, we
count all of ours but only a portion of theirs. The difference seen here is a
lower bound on the total difference, which is much higher and includes not
only unnecessary computation but significant levels of unnecessary allocation.

Here's an example of what that can mean in practice.
```
scala> val xs = (1 to 1e7.toInt).toArray
xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, ...)

// Prepare scala's array operations implicit - otherwise not in scope under -Yno-predef
scala> def ops = Predef.intArrayOps _
ops: Array[Int] => scala.collection.mutable.ArrayOps[Int]

// Typically about 300ms to complete.
scala> timed(ops(xs) map (_ + 1) map (_ + 1) map (_ + 1) take 3 mkString ", ")
Elapsed: 307.322 ms
res0: String = 4, 5, 6

// Now with psp. Typically about 3ms to complete.
scala> timed(xs map (_ + 1) map (_ + 1) map (_ + 1) take 3 joinWith ", ")
Elapsed: 3.529 ms
res1: String = 4, 5, 6
```

Of course pervasive laziness offers many other benefits as well. Sometimes we succeed where scala would throw an unnecessary exception:

```
scala> val xs = Array(0, 0, 0, 0, 0, 1)
xs: Array[Int] = Array(0, 0, 0, 0, 0, 1)

// In scala, failure
scala> ops(xs) map (5 / _) takeRight 1 mkString ""
java.lang.ArithmeticException: / by zero
  at $anonfun$1.apply$mcII$sp(<console>:23)

// In psp, success
scala> xs map (5 / _) takeRight 1 joinString
res0: String = 5
```

Other times we succeed where scala opts for non-termination.

```
scala> val xs = Each.unfold(BigInt(1))(_ + 1)
xs: psp.std.Each.Unfold[scala.math.BigInt] = Unfold(1)

// We drop 1000 elements off the right side of infinity, then take the first three.
scala> xs dropRight 1000 take 3 joinWith ", "
res0: String = 1, 2, 3

// Try it with a Stream view (it does terminate with Stream.)
scala> val xs = (Stream from 1).view
xs: scala.collection.immutable.StreamView[Int,scala.collection.immutable.Stream[Int]] = StreamView(...)

// Alas poor StreamView... I knew him well.
scala> xs dropRight 1000 take 3 mkString ", "
[... time passes ...]
java.lang.OutOfMemoryError: Java heap space
  at scala.collection.immutable.Stream$.from(Stream.scala:1142)
  at scala.collection.immutable.Stream$$anonfun$from$1.apply(Stream.scala:1142)
```
