package psp
package std

import api._, exp._

/** Having an Empty[A] instance in scope allows for using methods
  * like zfold, zreduce, zhead, whereupon the implicit empty value
  * will be used if the View is indeed empty. One could look at
  * standard semantics as using a default Empty[A] instance for all
  * types, satisfied by throwing an exception. That sort of Empty[A]
  * instance can also be created explicitly.
  */
object Empty {
  def empty[A]: Empty[A]             = new Throws[A]("empty") // the empty empty
  def apply[A](empty: => A): Impl[A] = new Impl[A](empty)
  def const[A](empty: A): Const[A]   = new Const[A](empty)

  final class Throws[+A](msg: String) extends Empty[A] { def empty: A = abort(msg) }
  final class Impl[+A](expr: => A)    extends Empty[A] { def empty: A = expr }
  final class Const[+A](val empty: A) extends Empty[A] {}
}

trait StdEmpty {
  implicit def emptyBuilds[A, R](implicit z: Builds[A, R]): Empty[R]       = Empty(elems())
  implicit def emptyCanBuild[A, R](implicit z: CanBuild[A, R]): Empty[R]   = Empty(z().result)
  implicit def emptyHeyting[A: Heyting]: Empty[A]                          = Empty(?[Heyting[A]].zero)
  implicit def emptyIdView[A, R]: Empty[IdView[A, R]]                      = Empty(rview())
  implicit def emptyOptional[A]: Empty[jOptional[A]]                       = Empty(java.util.Optional.empty[A]())
  implicit def emptyPair[A: Empty, B: Empty]: Empty[(A, B)]                = Empty(pair(emptyValue[A], emptyValue[B]))
  implicit def emptyTriple[A: Empty, B: Empty, C: Empty]: Empty[(A, B, C)] = Empty(triple(emptyValue[A], emptyValue[B], emptyValue[C]))

  implicit lazy val emptyDoc: Empty.Const[Doc]                  = Empty const Doc.empty
  implicit lazy val emptyFile: Empty.Const[jFile]               = Empty const jFile("")
  implicit lazy val emptyIndex: Empty.Const[Index]              = Empty const Index.invalid
  implicit lazy val emptyInterval: Empty.Const[Interval.Closed] = Empty const Interval.empty
  implicit lazy val emptyNth: Empty.Const[Nth]                  = Empty const Nth.invalid
  implicit lazy val emptyOption: Empty.Const[Option[Nothing]]   = Empty const None
  implicit lazy val emptyPath: Empty.Const[jPath]               = Empty const jPath("")
  implicit lazy val emptyPredicate: Empty[ToBool[Any]]          = Empty const ConstantFalse
  implicit lazy val emptyString: Empty.Const[String]            = Empty const ""
  implicit lazy val emptyUri: Empty.Const[jUri]                 = Empty const jUri("")
  implicit lazy val emptyVdexRange: Empty.Const[VdexRange]      = Empty const indexRange(0, 0)
}
