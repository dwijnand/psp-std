package psp
package std

/**
  *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
  *  around and it lets us build scala collections at the end of a view with no more code.
  *  Scala maps are built with Tuples even though Product2 should suffice; the types
  *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
  *  The rest of the time one should write it K->V.
  */
import all._

trait StdBuilders1 {
  implicit def makesJavaSet[A]: Makes[A, jSet[A]] = Makes.javaSet
}
trait StdBuilders2 extends StdBuilders1 {
  implicit def makesJavaList[A]: Makes[A, jList[A]] = Makes.javaList
}
trait StdBuilders3 extends StdBuilders2 {
  implicit def makesJavaMap[K, V]: Makes[K->V, jMap[K, V]]                                            = Makes.javaMap
  implicit def makesJvmArray[A: CTag]: Makes[A, Array[A]]                                             = Makes.jvmArray
  implicit def makesJvmString: Makes[Char, String]                                                    = Makes.jvmString
  implicit def makesPspList[A]: Makes[A, Plist[A]]                                                    = Makes.pspList
  implicit def makesPspMap[K : Hash : Eq, V]: Makes[K->V, Pmap[K, V]]                                 = Makes.pspMap
  implicit def makesPspSet[A : Hash : Eq]: Makes[A, Pset[A]]                                          = Makes.pspSet
  implicit def makesPspView[A]: Makes[A, View[A]]                                                     = Makes.pspView
  implicit def makesScalaMap[K, V, That](implicit z: CanBuild[Tuple2[K, V], That]): Makes[K->V, That] = Makes.scalaGenericMap
  implicit def makesScala[A, That](implicit z: CanBuild[A, That]): Makes[A, That]                     = Makes.scalaTraversable
  implicit def viewsJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): RepView[CC[A], A]              = xs.m
  implicit def viewsJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): RepView[CC[K, V], K->V]      = xs.m
  implicit def viewsJvmString(xs: String): IdView[Char, String]                                       = xs.m2
  implicit def viewsPspEach[A, CC[X] <: Each[X]](xs: CC[A]): IdView[A, CC[A]]                         = xs.m2
  implicit def viewsScala[A, CC[X] <: sCollection[X]](xs: CC[A]): RepView[CC[A], A]                   = xs.m
  implicit def walksJavaIterable[A, CC[X] <: jIterable[X]]: Walks[A, CC[A]]                           = Walks.javaIterable
  implicit def walksJavaMap[K, V, CC[X, Y] <: jMap[X, Y]]: Walks[K->V, CC[K, V]]                      = Walks.javaMap
  implicit def walksJvmArray[A]: Walks[A, Array[A]]                                                   = Walks.jvmArray
  implicit def walksJvmString: Walks[Char, String]                                                    = Walks.jvmString
  implicit def walksPspEach[A, CC[X] <: Each[X]]: Walks[A, CC[A]]                                     = Walks.pspEach
  implicit def walksPspSet[A, CC[X] <: Pset[X]]: Walks[A, CC[A]]                                      = Walks.pspSet
  implicit def walksPspView[A, CC[X] <: View[X]]: Walks[A, CC[A]]                                     = Walks.pspView
  implicit def walksScalaMap[K, V, CC[X, Y] <: scMap[X, Y]]: Walks[K->V, CC[K, V]]                    = Walks.scalaMap
  implicit def walksScalaOnce[A, CC[X] <: GTOnce[X]]: Walks[A, CC[A]]                                 = Walks.scalaOnce
}
trait StdBuilders extends StdBuilders3 {
  implicit def makesPspDirect[A]: Makes[A, Direct[A]] = Makes.pspDirect[A]
}

/** Having an Empty[A] instance in scope allows for using methods
  * like zfold, zreduce, zhead, whereupon the implicit empty value
  * will be used if the View is indeed empty. One could look at
  * standard semantics as using a default Empty[A] instance for all
  * types, satisfied by throwing an exception. That sort of Empty[A]
  * instance can also be created explicitly.
  */
trait StdEmpty0 {
  implicit def emptyIdView[A, R]: Empty[IdView[A, R]] = Empty(new IdView(vec[A]()))
}
trait StdEmpty extends StdEmpty0 {
  implicit def emptyFromCanBuild[A, R](implicit z: CanBuild[A, R]): Empty[R] = Empty(z().result)
  implicit def emptyFromHeyting[A: Heyting]: Empty[A]                        = Empty(?[Heyting[A]].zero)
  implicit def emptyFromMakes[A, R](implicit z: Makes[A, R]): Empty[R]       = Empty(elems())
  implicit def emptyJavaList[A]: Empty[jList[A]]                             = Empty(new jArrayList[A])
  implicit def emptyJavaMap[A, B]: Empty[jMap[A, B]]                         = Empty(new jHashMap[A, B])
  implicit def emptyJavaSet[A]: Empty[jSet[A]]                               = Empty(new jHashSet[A])
  implicit def emptyOptional[A]: Empty[jOptional[A]]                         = Empty(java.util.Optional.empty[A]())
  implicit def emptyPair[A: Empty, B: Empty]: Empty[(A, B)]                  = Empty(pair(emptyValue[A], emptyValue[B]))
  implicit def emptyRepView[R, A]: Empty[RepView[R, A]]                      = Empty(RepView.empty[R, A])
  implicit def emptyTriple[A: Empty, B: Empty, C: Empty]: Empty[(A, B, C)]   = Empty(triple(emptyValue[A], emptyValue[B], emptyValue[C]))

  implicit lazy val emptyDoc: Empty.Const[Doc]                  = Empty const Doc.empty
  implicit lazy val emptyFile: Empty.Const[jFile]               = Empty const jFile("")
  implicit lazy val emptyIndex: Empty.Const[Index]              = Empty const Index.invalid
  implicit lazy val emptyInterval: Empty.Const[Interval.Closed] = Empty const Interval.empty
  implicit lazy val emptyNth: Empty.Const[Nth]                  = Empty const Nth.invalid
  implicit lazy val emptyOption: Empty.Const[Option[Nothing]]   = Empty const None
  implicit lazy val emptyPath: Empty.Const[jPath]               = Empty const jPath("")
  implicit lazy val emptyPredicate: Empty[ToBool[Any]]          = Empty const ConstantFalse
  implicit lazy val emptySize: Empty.Const[Size]                = Empty const _0
  implicit lazy val emptyString: Empty.Const[String]            = Empty const ""
  implicit lazy val emptyUri: Empty.Const[jUri]                 = Empty const jUri("")
  implicit lazy val emptyVdexRange: Empty.Const[VdexRange]      = Empty const (Interval.empty map Index)
}
