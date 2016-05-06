package psp
package api

import scala.{ Tuple2, collection => sc }
import java.{ lang => jl }
import java.nio.{ file => jnf }

// Importing from this is necessary to use these aliases within the api package,
// where they aren't otherwise visible because there's no api package object.
private[api] final object Api extends ApiValues

trait ApiTypes extends ExternalTypes {
  // Aliases for internal and external types.
  type ->[+A, +B]        = scala.Product2[A, B]        // A less overconstrained product type.
  type ?=>[-A, +B]       = scala.PartialFunction[A, B] // ?=> associates to the left instead of the right.
  type BinOp[A]          = (A, A) => A                 // binary operation
  type Bool              = Boolean
  type GTOnce[+A]        = sc.GenTraversableOnce[A]    // This is the beautifully named type at the top of scala collections
  type Index             = Vindex[Vindex.Zero.type]
  type Nth               = Vindex[Vindex.One.type]
  type OrderRelation[-A] = (A, A) => Cmp
  type Ref[+A]           = AnyRef with A               // Promotes an A <: Any into an A <: AnyRef.
  type Relation[-A]      = (A, A) => Bool
  type Suspended[+A]     = ToUnit[ToUnit[A]]
  type ToBool[-A]        = A => Bool
  type ToInt[-A]         = A => Int
  type ToSelf[A]         = A => A
  type ToString[-A]      = A => String
  type ToUnit[-A]        = A => Unit
  type Vdex              = Vindex[_]
  type sCollection[+A]   = sc.GenTraversable[A]        // named analogously to jCollection.

  // A few type constructors
  type Id[+X]         = X
  type ConstString[X] = String
  type ConstSize[X]   = Size
}

abstract class ApiValues extends ApiTypes {
  final val MaxInt  = jl.Integer.MAX_VALUE
  final val MaxLong = jl.Long.MAX_VALUE
  final val MinInt  = jl.Integer.MIN_VALUE
  final val MinLong = jl.Long.MIN_VALUE
  final val EOL     = jl.System getProperty "line.separator"

  def ??? : Nothing                                 = throw new scala.NotImplementedError
  def abort(msg: Any): Nothing                      = throw new RuntimeException(s"$msg")
  def assert(assertion: => Bool, msg: => Any): Unit = cond(assertion, (), assertionError(msg))
  def assertionError(msg: Any): Nothing             = throw new AssertionError(s"$msg")
  def illegalArgumentException(msg: Any): Nothing   = throw new IllegalArgumentException(s"$msg")
  def indexOutOfBoundsException(msg: Any): Nothing  = throw new IndexOutOfBoundsException(s"$msg")
  def noSuchElementException(msg: Any): Nothing     = throw new NoSuchElementException(s"$msg")

  def ?[A](implicit value: A): A                    = value
  def castRef[A](value: A): Ref[A]                  = cast(value)
  def cast[A](value: Any): A                        = value.asInstanceOf[A]
  def classOf[A: CTag](): Class[_ <: A]             = cast(classTag[A].runtimeClass)
  def classTag[A: CTag] : CTag[A]                   = ?[CTag[A]]
  def cond[A](p: Bool, thenp: => A, elsep: => A): A = if (p) thenp else elsep
  def doto[A](x: A)(f: A => Unit): A                = sideEffect(x, f(x))
  def emptyValue[A](implicit z: Empty[A]): A        = z.empty
  def fst[A, B](x: A -> B): A                       = x._1
  def identity[A](x: A): A                          = x
  def isInstance[A: CTag](x: Any): Bool             = classOf[A]() isAssignableFrom x.getClass
  def jFile(path: String): jFile                    = new jFile(path)
  def jPath(path: String): jPath                    = jnf.Paths get path
  def jUri(x: String): jUri                         = java.net.URI create x
  def min[A](l: A, r: A)(implicit z: Order[A]): A   = cond(z.cmp(l, r) == Cmp.LT, l, r)
  def max[A](l: A, r: A)(implicit z: Order[A]): A   = cond(z.cmp(l, r) == Cmp.LT, r, l)
  def none[A](): Option[A]                          = scala.None
  def nullAs[A] : A                                 = cast(null)
  def pair[A, B](x: A, y: B): Tuple2[A, B]          = new Tuple2(x, y)
  def show[A](implicit z: Show[A]): Show[A]         = z
  def sideEffect[A](result: A, exprs: Any*): A      = result
  def snd[A, B](x: A -> B): B                       = x._2
  def some[A](x: A): Option[A]                      = scala.Some(x)
  def swap[A, B](x: A, y: B): B -> A                = scala.Tuple2(y, x)
  def tuple[A, B](x: A -> B): ((A, B))              = scala.Tuple2(fst(x), snd(x))
  def zcond[A: Empty](p: Bool, thenp: => A): A      = cond(p, thenp, emptyValue[A])

  /** Safe in the senses that it won't silently truncate values,
   *  and will translate MaxLong to MaxInt instead of -1.
   *  Note that we depend on this.
   */
  def safeLongToInt(value: Long): Int = value match {
    case MaxLong => MaxInt
    case MinLong => MinInt
    case _       => assert(MinInt <= value && value <= MaxInt, s"$value out of range") ; value.toInt
  }

  def stringFormat(s: String, args: Any*): String = {
    def unwrapArg(arg: Any): AnyRef = arg match {
      case x: scala.math.ScalaNumber => x.underlying
      case x                         => castRef(x)
    }
    java.lang.String.format(s, args map unwrapArg: _*)
  }

  def newArray[A: CTag](length: Int): Array[A] = new Array[A](length)

  def copyArray[A: CTag](src: Array[A]): Array[A] =
    doto(newArray[A](src.length))(arraycopy(src, 0, _, 0, src.length))

  def arraycopy[A](src: Array[A], srcPos: Int, dst: Array[A], dstPos: Int, len: Int): Unit =
    java.lang.System.arraycopy(src, srcPos, dst, dstPos, len)

  // You can't use string interpolation without a StringContext term in scope.
  def StringContext = scala.StringContext
}
