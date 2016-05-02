package psp
package std

import api._, all._
// import java.util.stream.Stream.{ builder => jStreamBuilder }

object Java extends JavaCollections

trait StdJava0 {
  // implicit def viewJavaStream[A, CC[X] <: jStream[X]](xs: CC[A]): AtomicView[A, CC[A]]               = new StreamView(xs)
  implicit def viewJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): AtomicView[A, CC[A]]           = new LinearView(Each java xs)
  implicit def viewJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): AtomicView[K -> V, CC[K, V]] = new LinearView(Each javaMap xs)
}
trait StdJava extends StdJava0 {
  implicit def unbuildJavaIterable[A, CC[X] <: jIterable[X]] : UnbuildsAs[A, CC[A]]        = Unbuilds[A, CC[A]](Each java _)
  // implicit def unbuildJavaMap[K, V, CC[K, V] <: jMap[K, V]] : UnbuildsAs[K -> V, CC[K, V]] = Unbuilds[K -> V, CC[K, V]](Each javaMap _)

  implicit def opsJavaIterator[A](x: jIterator[A]): ops.JavaIteratorOps[A] = new ops.JavaIteratorOps(x)
  // implicit def opsJavaStream[A](x: jStream[A]): ops.JavaStreamOps[A]       = new ops.JavaStreamOps(x)
  // implicit def opsJavaOptional[A](x: jOptional[A]): ops.OptionOps[A]       = new ops.OptionOps[A](cond(x.isPresent, Some(x.get), None)) // XXX

  // implicit def convertToJavaPredicate[A](p: ToBool[A]): jPredicate[A]      = new jPredicate[A] { def test(x: A) = p(x) }
  // implicit def convertToJavaFunction[A, B](f: A => B): jFunction[A, B]     = new jFunction[A, B] { def apply(x: A): B = f(x) }
  // implicit def convertToJavaConsumer[A](f: A => Unit): jConsumer[A]        = new jConsumer[A] { def accept(x: A): Unit = f(x) }
}

trait JavaBuilders0 {
  def genericJavaListBuilder[A, M[A] <: jList[A]](z: M[A]): Builds[A, M[A]]                 = Builds(xs => doto(z)(z => xs foreach (x => z add x)))
  def genericJavaSetBuilder[A, M[A] <: jSet[A]](z: M[A]): Builds[A, M[A]]                   = Builds(xs => doto(z)(z => xs foreach (x => z add x)))
  def genericJavaMapBuilder[K, V, M[K, V] <: jMap[K, V]](z: M[K, V]): Builds[K->V, M[K, V]] = Builds(xs => doto(z)(z => xs foreach (x => z.put(fst(x), snd(x)))))

  implicit def javaSetBuilder[A]: Builds[A, jSet[A]]            = genericJavaSetBuilder(new jHashSet[A])
  implicit def javaListBuilder[A]: Builds[A, jList[A]]          = genericJavaListBuilder(new jArrayList[A])
  implicit def javaMapBuilder[K, V]: Builds[K -> V, jMap[K, V]] = genericJavaMapBuilder(new jHashMap[K, V])
  // implicit def javaStreamBuilder[A]: Builds[A, jStream[A]]      = Builds(xs => doto(jStreamBuilder[A]())(xs foreach _.add).build)
}
trait JavaBuilders extends JavaBuilders0 {
  implicit def javaSortedSetBuilder[A: Order]: Builds[A, jSortedSet[A]]            = genericJavaSetBuilder(new jTreeSet[A](Order.comparator[A]))
  implicit def javaSortedMapBuilder[K: Order, V]: Builds[K -> V, jSortedMap[K, V]] = genericJavaMapBuilder(new jTreeMap[K, V](Order.comparator[K]))
}
trait JavaCollections extends JavaBuilders {
  // def ConcurrentMap[K: Eq, V](xs: (K -> V)*): jConcurrentMap[K, V] = Built(xs)(genericJavaMapBuilder(new jConcurrentHashMap[K, V]))
  // def SortedMap[K: Order, V](xs: (K -> V)*): jSortedMap[K, V]      = Built(xs)
  def List[A](xs: A*): jList[A]                                    = Built(xs)
  def Map[K, V](xs: (K -> V)*): jMap[K, V]                         = Built(xs)
  def Set[A](xs: A*): jSet[A]                                      = Built(xs)
  // def SortedSet[A: Order](xs: A*): jSortedSet[A]                   = Built(xs)
  // def Stream[A](xs: A*): jStream[A]                                = Built(xs)
}

package ops {
  final class JavaIteratorOps[A](it: jIterator[A]) {
    def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
  }

  final class CmpEnumOps(val cmp: Cmp) {
    def |(that: => Cmp): Cmp = if (cmp == Cmp.EQ) that else cmp
  }

  final class JavaStreamOps[A](xs: jStream[A]) {
    // def slice(n: VdexRange): jStream[A] = xs drop n.startInt take n.size.getInt
    // def take(n: Precise): jStream[A]      = xs limit n.getInt
    // def drop(n: Precise): jStream[A]      = xs skip n.getInt
    // def forall(p: ToBool[A]): Bool        = xs allMatch p
    // def exists(p: ToBool[A]): Bool        = xs anyMatch p
    // def foreach(f: A => Unit): Unit       = xs forEach f
    // def zhead(implicit z: Empty[A]): A    = xs.findFirst.zget

    // def forEachOrdered(x$1: java.util.function.Consumer[_ >: A]): Unit = ???
    // def close(): Unit = ???
    // def isParallel(): Boolean = ???
    // def iterator(): java.util.Iterator[A] = ???
    // def onClose(x$1: Runnable): java.util.stream.Stream[A] = ???
    // def parallel(): java.util.stream.Stream[A] = ???
    // def sequential(): java.util.stream.Stream[A] = ???
    // def spliterator(): java.util.Spliterator[A] = ???
    // def unordered(): java.util.stream.Stream[A] = ???
    // def collect[R, A](x$1: java.util.stream.Collector[_ >: A, A, R]): R = ???
    // def collect[R](x$1: java.util.function.Supplier[R],x$2: java.util.function.BiConsumer[R, _ >: A],x$3: java.util.function.BiConsumer[R,R]): R = ???
    // def count(): Long = ???
    // def distinct(): java.util.stream.Stream[A] = ???
    // def filter(x$1: java.util.function.Predicate[_ >: A]): java.util.stream.Stream[A] = ???
    // def findAny(): java.util.Optional[A] = ???
    // def flatMap[R](x$1: java.util.function.Function[_ >: A, _ <: java.util.stream.Stream[_ <: R]]): java.util.stream.Stream[R] = ???
    // def flatMapToDouble(x$1: java.util.function.Function[_ >: A, _ <: java.util.stream.DoubleStream]): java.util.stream.DoubleStream = ???
    // def flatMapToInt(x$1: java.util.function.Function[_ >: A, _ <: java.util.stream.IntStream]): java.util.stream.IntStream = ???
    // def flatMapToLong(x$1: java.util.function.Function[_ >: A, _ <: java.util.stream.LongStream]): java.util.stream.LongStream = ???
    // def map[R](x$1: java.util.function.Function[_ >: A, _ <: R]): java.util.stream.Stream[R] = ???
    // def mapToDouble(x$1: java.util.function.ToDoubleFunction[_ >: A]): java.util.stream.DoubleStream = ???
    // def mapToInt(x$1: java.util.function.ToIntFunction[_ >: A]): java.util.stream.IntStream = ???
    // def mapToLong(x$1: java.util.function.ToLongFunction[_ >: A]): java.util.stream.LongStream = ???
    // def max(x$1: java.util.Comparator[_ >: A]): java.util.Optional[A] = ???
    // def min(x$1: java.util.Comparator[_ >: A]): java.util.Optional[A] = ???
    // def noneMatch(x$1: java.util.function.Predicate[_ >: A]): Boolean = ???
    // def peek(x$1: java.util.function.Consumer[_ >: A]): java.util.stream.Stream[A] = ???
    // def reduce[U](x$1: U,x$2: java.util.function.BiFunction[U, _ >: A, U],x$3: java.util.function.BinaryOperator[U]): U = ???
    // def reduce(x$1: java.util.function.BinaryOperator[A]): java.util.Optional[A] = ???
    // def reduce(x$1: A,x$2: java.util.function.BinaryOperator[A]): A = ???
    // def sorted(x$1: java.util.Comparator[_ >: A]): java.util.stream.Stream[A] = ???
    // def sorted(): java.util.stream.Stream[A] = ???
    // def toArray[A](x$1: java.util.function.IntFunction[Array[A with Object]]): Array[A with Object] = ???
    // def toArray(): Array[Object] = ???

  }
}
