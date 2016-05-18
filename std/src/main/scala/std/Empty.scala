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
  implicit def emptyCanBuild[R](implicit z: CanBuild[_, R]): Empty[R]      = Empty(z().result)
  implicit def emptyBuilds[A, R](implicit z: Builds[A, R]): Empty[R]       = Empty(z build vec())
  implicit def emptyOptional[A]: Empty[jOptional[A]]                       = Empty(java.util.Optional.empty[A]())
  implicit def emptyPair[A: Empty, B: Empty]: Empty[(A, B)]                = Empty(pair(emptyValue[A], emptyValue[B]))
  implicit def emptyTriple[A: Empty, B: Empty, C: Empty]: Empty[(A, B, C)] = Empty(triple(emptyValue[A], emptyValue[B], emptyValue[C]))
  implicit def emptyIdView[A, R]: Empty[IdView[A, R]]                      = Empty(new IdView(Pnil))

  implicit lazy val emptyOption: Empty.Const[Option[Nothing]] = Empty const None
  implicit lazy val emptyFile: Empty.Const[jFile]             = Empty const NoFile
  implicit lazy val emptyIndex: Empty.Const[Index]            = Empty const Index.invalid
  implicit lazy val emptyNth: Empty.Const[Nth]                = Empty const Nth.invalid
  implicit lazy val emptyPath: Empty.Const[jPath]             = Empty const NoPath
  implicit lazy val emptyString: Empty.Const[String]          = Empty const ""
  implicit lazy val emptyVdexRange: Empty.Const[VdexRange]    = Empty const indexRange(0, 0)
}

trait StdTypeClasses {
  import all._

  /** Splitter/Joiner type classes for composing and decomposing an R into A -> B.
    *  Somewhat conveniently for us, "cleave" is a word which has among its meanings
    *  "to adhere firmly and closely as though evenly and securely glued" as well
    *  as "to divide into two parts by a cutting blow".
    */
  def splitter[R, A, B](f: R => (A -> B)): Splitter[R, A, B] = new Splitter[R, A, B] { def split(x: R): A -> B = f(x) }
  def joiner[R, A, B](f: (A, B) => R): Joiner[R, A, B]       = new Joiner[R, A, B] { def join(x: A -> B): R    = f(x._1, x._2) }

  def cleaver[R, A, B](f: (A, B) => R, l: R => A, r: R => B): Cleaver[R, A, B] = new Cleaver[R, A, B] {
    def split(x: R): A -> B = l(x) -> r(x)
    def join(x: A -> B): R  = x app f
  }
}
