package psp
package api

import scala.{ collection => sc }, sc.{ mutable => scm, immutable => sci }
import java.{ lang => jl }
import java.{ util => ju }
import java.{ io => jio }
import java.nio.{ file => jnf }

/** Building a default namespace consciously rather than accretively.
 *  Primarily these are various "magic" types which cannot be avoided
 *  due to language privilege. An incomplete list:
 *
 *   - all top and bottom types are treated specially
 *   - as are all primitive types and unit
 *   - Seq, Option, and TupleN are hardcoded into extractors
 *   - Seq is hardcoded into varargs
 *   - Arrays are everywhere special
 *   - Lists are specially optimized by the compiler
 *   - StringContext is required for string interpolation
 *   - ClassTags are synthesized by the compiler
 *   - BigDecimal/BigInt are treated specially for equality
 *   - Dynamic has special semantics
 *   - Function1 has special syntax and much special handling
 *   - PartialFunction has all of Function1's and then some
 *   - DelayedInit is very special but it's garbage so omitted
 */
trait ExternalTypes {
  // The top and bottom types.
  type Any     = scala.Any
  type AnyRef  = scala.AnyRef
  type AnyVal  = scala.AnyVal
  type Null    = scala.Null
  type Nothing = scala.Nothing

  // The eight primitive types of the jvm, plus the scala version of void.
  type Boolean = scala.Boolean
  type Byte    = scala.Byte
  type Char    = scala.Char
  type Double  = scala.Double
  type Float   = scala.Float
  type Int     = scala.Int
  type Long    = scala.Long
  type Short   = scala.Short
  type Unit    = scala.Unit

  // scala magic types, mostly not renamed.
  type Array[A]      = scala.Array[A]
  type BigDecimal    = scala.math.BigDecimal
  type BigInt        = scala.math.BigInt
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Dynamic       = scala.Dynamic
  type Failure[+A]   = scala.util.Failure[A]
  type Option[+A]    = scala.Option[A]
  type Some[+A]      = scala.Some[A]
  type StringContext = scala.StringContext
  type Success[+A]   = scala.util.Success[A]
  type Try[+A]       = scala.util.Try[A]

  // scala annotations
  type inline    = scala.inline
  type spec      = scala.specialized
  type switch    = scala.annotation.switch
  type tailrec   = scala.annotation.tailrec
  type transient = scala.transient
  type uV        = scala.annotation.unchecked.uncheckedVariance
  type unchecked = scala.unchecked
  type volatile  = scala.volatile

  // scala collection types, named consistently and distinctly based on package of origin.
  type scIterable[+A]         = sc.Iterable[A]
  type scIterator[+A]         = sc.Iterator[A]
  type scMap[K, +V]           = sc.Map[K, V]
  type scSeq[+A]              = sc.Seq[A]
  type scSet[A]               = sc.Set[A]
  type scTraversable[+A]      = sc.Traversable[A]

  type sciIndexedSeq[+A]      = sci.IndexedSeq[A]
  type sciList[+A]            = sci.List[A]
  type sciMap[K, +V]          = sci.Map[K, V]
  type sciSeq[+A]             = sci.Seq[A]
  type sciSet[A]              = sci.Set[A]
  type sciStream[+A]          = sci.Stream[A]
  type sciVector[+A]          = sci.Vector[A]

  type scmBuilder[-Elem, +To] = scm.Builder[Elem, To]
  type scmMap[K, V]           = scm.Map[K, V]

  // Spire
  type AdditiveMonoid[A]       = spire.algebra.AdditiveMonoid[A]
  type BooleanAlgebra[R]       = spire.algebra.Bool[R]
  type MultiplicativeMonoid[A] = spire.algebra.MultiplicativeMonoid[A]
  type SafeLong                = spire.math.SafeLong
  type UInt                    = spire.math.UInt

  // Types from java.
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

  type Class[A]             = jl.Class[A]
  type Comparable[A]        = jl.Comparable[A]
  type Comparator[-A]       = ju.Comparator[A @uV]
  type InputStream          = jio.InputStream
  type OutputStream         = jio.OutputStream
  type String               = jl.String
  type StringBuilder        = jl.StringBuilder
  type Thread               = jl.Thread
  type jArrayList[A]        = ju.ArrayList[A]
  type jCharSequence        = jl.CharSequence
  type jClass               = jl.Class[_]
  type jClassLoader         = jl.ClassLoader
  type jEnum[E <: jEnum[E]] = jl.Enum[E]
  type jEnumeration[A]      = ju.Enumeration[A]
  type jFile                = jio.File
  type jHashMap[K, V]       = ju.HashMap[K, V]
  type jHashSet[A]          = ju.HashSet[A]
  type jIterable[+A]        = jl.Iterable[A @uV]
  type jIterator[+A]        = ju.Iterator[A @uV]
  type jList[A]             = ju.List[A]
  type jMap[K, V]           = ju.Map[K, V]
  type jOptional[+A]        = ju.Optional[A @uV]
  type jPath                = jnf.Path
  type jSet[A]              = ju.Set[A]
  type jSortedMap[K, V]     = ju.SortedMap[K, V]
  type jSortedSet[A]        = ju.SortedSet[A]
  type jStream[A]           = ju.stream.Stream[A]
  type jTreeMap[K, V]       = ju.TreeMap[K, V]
  type jTreeSet[A]          = ju.TreeSet[A]
  type jUri                 = java.net.URI

  // You can't use string interpolation without a StringContext term in scope.
  def StringContext = scala.StringContext
}
