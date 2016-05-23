package psp
package std

/**
  *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
  *  around and it lets us build scala collections at the end of a view with no more code.
  *  Scala maps are built with Tuples even though Product2 should suffice; the types
  *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
  *  The rest of the time one should write it K->V.
  */
import api._, all._, scala.Tuple2

trait MakesWalks1 {
  implicit def makesJavaSet[A]: Makes[A, jSet[A]] = Makes.javaSet
}
trait MakesWalks2 extends MakesWalks1 {
  implicit def makesJavaList[A]: Makes[A, jList[A]] = Makes.javaList
}
trait MakesWalks3 extends MakesWalks2 {
  implicit def makesJavaMap[K, V]: Makes[K->V, jMap[K, V]]                                            = Makes.javaMap
  implicit def makesJvmArray[A: CTag]: Makes[A, Array[A]]                                             = Makes.jvmArray
  implicit def makesJvmString: Makes[Char, String]                                                    = Makes.jvmString
  implicit def makesPspList[A]: Makes[A, Plist[A]]                                                    = Makes.pspList
  implicit def makesPspMap[K: Eq, V]: Makes[K->V, Pmap[K, V]]                                         = Makes.pspMap
  implicit def makesPspSet[A: Eq]: Makes[A, Pset[A]]                                                  = Makes.pspSet
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
trait MakesWalks extends MakesWalks3 {
  implicit def makesPspDirect[A]: Makes[A, Direct[A]] = Makes.pspDirect[A]
}
