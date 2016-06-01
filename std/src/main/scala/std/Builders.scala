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

  implicit def isFoldedPspSet[A]: IsFolded[Pset[A], A]                       = IsFolded(xs => xs.basis foreach _)
  implicit def isFoldedForeach[A, CC[X] <: Foreach[X]]: IsFolded[CC[A], A]   = IsFolded(xs => xs foreach _)
  implicit def isFoldedJava[A, CC[X] <: jIterable[X]]: IsFolded[CC[A], A]    = IsFolded(xs => xs.iterator foreach _)
  implicit def isFoldedScalaOnce[A, CC[X] <: GTOnce[X]]: IsFolded[CC[A], A]  = IsFolded(xs => xs foreach _)
  implicit def walksIsFolded[A, R](implicit iz: IsFolded[R, A]): Walks[A, R] = Walks(Makes fromFolded _)
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
  implicit def viewsJvmString(xs: String): RView[Char, String]                                        = xs.m2
  implicit def viewsPspEach[A, CC[X] <: Each[X]](xs: CC[A]): RView[A, CC[A]]                          = xs.m2
  implicit def viewsScalaCollection[A, CC[X] <: sCollection[X]](xs: CC[A]): RepView[CC[A], A]         = xs.m

  implicit def walksPspEach[A, CC[X] <: Each[X]]: Walks[A, CC[A]] = Walks.pspEach
  implicit def walksPspView[A, CC[X] <: View[X]]: Walks[A, CC[A]] = Walks.pspView

  implicit def isPairsScalaMap[K, V, M[K, V] <: scMap[K, V]]: IsPairs[M[K, V], K, V] = IsPairs(_.toSeq)
  implicit def isPairsJavaMap[K, V, M[K, V] <: jMap[K, V]]: IsPairs[M[K, V], K, V]   = IsPairs(_.entrySet.m map (_.toPair))
  implicit def intIndexedArray[A]: IsIntIndexed[Array[A], A]                         = IsIntIndexed(_.length, _ apply _)
  implicit def intIndexedScala[A, R <: scIndexedSeq[A]]: IsIntIndexed[R, A]          = IsIntIndexed(_.length, _ apply _)
  implicit def intIndexedString: IsIntIndexed[String, Char]                          = IsIntIndexed(_.length, _ charAt _)

  implicit def walksIntIndexed[A, R](implicit iz: IsIntIndexed[R, A]): Walks[A, R] = Walks(Makes fromIntIndexed _)
  implicit def walksPairs[A, B, R](implicit pz: IsPairs[R, A, B]): Walks[A->B, R]  = Walks(Makes fromPairs _)
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
  implicit def emptyRView[A, R]: Empty[RView[A, R]] = Empty(View(vec[A]()))
  implicit lazy val emptyNth: Empty.Const[Nth]      = Empty const Nth.invalid
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
  implicit lazy val emptyOption: Empty.Const[Option[Nothing]]   = Empty const None
  implicit lazy val emptyPath: Empty.Const[jPath]               = Empty const jPath("")
  implicit lazy val emptyPredicate: Empty[ToBool[Any]]          = Empty const ConstantFalse
  implicit lazy val emptySize: Empty.Const[Size]                = Empty const _0
  implicit lazy val emptyString: Empty.Const[String]            = Empty const ""
  implicit lazy val emptyUri: Empty.Const[jUri]                 = Empty const jUri("")
  implicit lazy val emptyVdexRange: Empty.Const[VdexRange]      = Empty const (Interval.empty map Index)
}
