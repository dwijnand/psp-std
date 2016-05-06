package psp
package std

import api._, exp._

/** Having an Empty[A] instance in scope allows for using methods
 *  like zfold, zreduce, zhead, whereupon the implicit empty value
 *  will be used if the View is indeed empty. One could look at
 *  standard semantics as using a default Empty[A] instance for all
 *  types, satisfied by throwing an exception. That sort of Empty[A]
 *  instance can also be created explicitly.
 */
object Empty {
  def empty[A] : Empty[A]            = new Throws[A]("empty") // the empty empty
  def apply[A](empty: => A): Impl[A] = new Impl[A](empty)
  def const[A](empty: A): Const[A]   = new Const[A](empty)

  final class Throws[A](msg: String) extends Empty[A] { def empty: A = abort(msg) }
  final class Impl[A](expr: => A) extends Empty[A]    { def empty: A = expr       }
  final class Const[A](val empty: A) extends Empty[A] {                           }
}

trait StdEmpty {
  implicit def emptyCanBuild[R](implicit z: CanBuild[_, R]): Empty[R] = Empty(z().result)
  implicit def emptyEach[R](implicit z: Builds[_, R]): Empty[R]       = Empty(z build vec())
  implicit def emptyOption[A] : Empty.Const[Option[A]]                = Empty const None
  implicit def emptyOptional[A] : Empty[jOptional[A]]                 = Empty(java.util.Optional.empty[A]())
  implicit def emptyTuple[A: Empty, B: Empty]: Empty[(A, B)]          = Empty(pair(emptyValue[A], emptyValue[B]))
  implicit def emptyView[A, R] : Empty[AtomicView[A, R]]              = Empty(new LinearView(Pnil))

  implicit lazy val emptyFile: Empty.Const[jFile]          = Empty const NoFile
  implicit lazy val emptyIndex: Empty.Const[Index]         = Empty const Index.invalid
  implicit lazy val emptyNth: Empty.Const[Nth]             = Empty const Nth.invalid
  implicit lazy val emptyPath: Empty.Const[jPath]          = Empty const NoPath
  implicit lazy val emptyString: Empty.Const[String]       = Empty const ""
  implicit lazy val emptyVdexRange: Empty.Const[VdexRange] = Empty const indexRange(0, 0)
}
