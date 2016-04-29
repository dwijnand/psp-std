package psp
package ext

import scala.{ Any, Long, Nothing, StringContext, Unit }
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import java.{ lang => jl }
import java.{ util => ju }
import java.{ io => jio }
// import java.util.{ function => juf }
import java.nio.{ file => jnf }
// import java.nio.charset.Charset

trait JavaLib {
  // Exceptional factories.
  def assertionError(msg: String): Nothing                = throw new AssertionError(msg)
  def illegalArgumentException(msg: Any): Nothing         = throw new IllegalArgumentException(s"$msg")
  def indexOutOfBoundsException(msg: Any): Nothing        = throw new IndexOutOfBoundsException(s"$msg")
  def noSuchElementException(msg: String): Nothing        = throw new NoSuchElementException(msg)
  def runtimeException(msg: String): Nothing              = throw new RuntimeException(msg)
  // def unsupportedOperationException(msg: String): Nothing = throw new UnsupportedOperationException(msg)

  // A selection of popular static methods from javaland.
  // def contextClassLoader: ClassLoader = currentThread.getContextClassLoader
  // def currentThread: Thread           = jl.Thread.currentThread
  // def defaultCharset: Charset         = Charset.defaultCharset
  // def fileSeparator: String           = jio.File.separator
  // def milliTime: Long                 = jl.System.currentTimeMillis
  def nanoTime: Long                  = jl.System.nanoTime
  // def systemClassLoader: ClassLoader  = jl.ClassLoader.getSystemClassLoader
  // def threadSleep(ms: Long): Unit     = jl.Thread.sleep(ms)
  def threadYield(): Unit             = jl.Thread.`yield`
  // def utf8Charset: Charset            = Charset forName "UTF-8"

  // A selection of creators for commonly required java types.
  def jFile(path: String): jFile = new jFile(path)
  def jPath(path: String): jPath = jnf.Paths get path
  def jUri(x: String): jUri      = java.net.URI create x
  // def jUrl(x: String): jUrl      = jUri(x).toURL

  // A few operations involving time and date.
  // def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  // def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  // def now(): FileTime                                    = jnf.attribute.FileTime fromMillis milliTime

  // Exceptions and Throwables.
  type AssertionError                = jl.AssertionError
  type ClassCastException            = jl.ClassCastException
  type Exception                     = jl.Exception
  type IOException                   = jio.IOException
  type IllegalArgumentException      = jl.IllegalArgumentException
  type IndexOutOfBoundsException     = jl.IndexOutOfBoundsException
  type NoSuchElementException        = ju.NoSuchElementException
  type RuntimeException              = jl.RuntimeException
  type Throwable                     = jl.Throwable
  type UnsupportedOperationException = jl.UnsupportedOperationException

  // java types which I acknowledge as victors in the battle for simple names.
  // type ClassLoader              = jl.ClassLoader
  type Class[A]                 = jl.Class[A]
  type Comparable[A]            = jl.Comparable[A]
  type Comparator[-A]           = ju.Comparator[A @uV]
  // type DirectoryStreamFilter[A] = jnf.DirectoryStream.Filter[A]
  // type DirectoryStream[A]       = jnf.DirectoryStream[A]
  type FileTime                 = jnf.attribute.FileTime
  type InputStream              = jio.InputStream
  type OutputStream             = jio.OutputStream
  // type PrintStream              = jio.PrintStream
  type String                   = jl.String
  type StringBuilder            = jl.StringBuilder
  type Thread                   = jl.Thread

  // java types for which the battle rages on.
  type jArrayList[A]            = ju.ArrayList[A]
  type jCharSequence            = jl.CharSequence
  // type jCharset                 = Charset
  type jClass                   = jl.Class[_]
  type jClassLoader             = jl.ClassLoader
  // type jCloseable               = jio.Closeable
  // type jCollection[A]           = ju.Collection[A]
  // type jConcurrentHashMap[K, V] = ju.concurrent.ConcurrentHashMap[K, V]
  // type jConcurrentMap[K, V]     = ju.concurrent.ConcurrentMap[K, V]
  // type jConsumer[-A]            = juf.Consumer[A @uV]
  // type jDate                    = ju.Date
  type jEnum[E <: jEnum[E]]     = jl.Enum[E]
  type jEnumeration[A]          = ju.Enumeration[A]
  // type jField                   = jl.reflect.Field
  type jFile                    = jio.File
  // type jFunction[-A, +B]        = juf.Function[A @uV, B @uV]
  // type jFuture[A]               = ju.concurrent.Future[A]
  type jHashMap[K, V]           = ju.HashMap[K, V]
  type jHashSet[A]              = ju.HashSet[A]
  type jIterable[+A]            = jl.Iterable[A @uV]
  type jIterator[+A]            = ju.Iterator[A @uV]
  type jList[A]                 = ju.List[A]
  // type jMapEntry[K, V]          = ju.Map.Entry[K, V]
  type jMap[K, V]               = ju.Map[K, V]
  // type jMethod                  = jl.reflect.Method
  // type jObject                  = jl.Object
  type jOptional[+A]            = ju.Optional[A @uV]
  type jPath                    = jnf.Path
  // type jPredicate[-A]           = juf.Predicate[A @uV]
  // type jRunnable                = jl.Runnable
  type jSet[A]                  = ju.Set[A]
  type jSortedMap[K, V]         = ju.SortedMap[K, V]
  type jSortedSet[A]            = ju.SortedSet[A]
  type jStream[A]               = ju.stream.Stream[A]
  type jTreeMap[K, V]           = ju.TreeMap[K, V]
  type jTreeSet[A]              = ju.TreeSet[A]
  type jUri                     = java.net.URI
  // type jUrl                     = java.net.URL

  // Inlinable constants.
  final val MaxInt  = jl.Integer.MAX_VALUE
  final val MaxLong = jl.Long.MAX_VALUE
  final val MinInt  = jl.Integer.MIN_VALUE
  final val MinLong = jl.Long.MIN_VALUE
  final val EOL     = jl.System getProperty "line.separator"
}
