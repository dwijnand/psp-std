package psp
package std

import api._, exp._, StdShow._

/** Op and Operable
  *
  *  It's tricky to abstract smoothly over the type constructor of a collection
  *  and its elements simultaneously.
  */
sealed trait Op[-A, +B] extends Any {
  type In >: A
  type Out <: B
}

trait Operable[M[X]] {
  def apply[A, B](xs: M[A])(op: Op[A, B]): M[B]
}

// sealed trait OpType {
//   type In
//   type Out
// }
// trait RangeOp[A]  extends OpType {               type Out = In }
// trait FilterOp[A] extends OpType { type In = A ; type Out = A  }
// trait MapOp[A, B] extends OpType { type In = A ; type Out = B  }

object Op {
  def apply[A](label: String): Identity[A] = Identity[A](label)

  /** We'd like to categorize view operations as is done above for OpType.
    *  This seems to be impossible or far too much trouble.
    *  So instead we give in and add pointless type parameters to the Range ops.
    */
  final case class Take[A](n: Precise)        extends Op[A, A]
  final case class Drop[A](n: Precise)        extends Op[A, A]
  final case class TakeRight[A](n: Precise)   extends Op[A, A]
  final case class DropRight[A](n: Precise)   extends Op[A, A]
  final case class Slice[A](range: VdexRange) extends Op[A, A]
  final case class Reverse[A]()               extends Op[A, A]

  final case class Identity[A](label: String) extends Op[A, A]
  final case class TakeWhile[A](p: ToBool[A]) extends Op[A, A]
  final case class DropWhile[A](p: ToBool[A]) extends Op[A, A]
  final case class Filter[A](p: ToBool[A])    extends Op[A, A]

  final case class Collect[A, B](pf: A ?=> B)                 extends Op[A, B]
  final case class Maps[A, B](f: A => B)                      extends Op[A, B]
  final case class FlatMap[A, B](f: A => View[B])             extends Op[A, B]
  final case class Compose[A, B, C](p: Op[A, B], q: Op[B, C]) extends Op[A, C]
  final case class Append[A](xs: View[A])                     extends Op[A, A]
  final case class Prepend[A](xs: View[A])                    extends Op[A, A]

  implicit class OpOps[A, B](private val op: Op[A, B]) extends AnyVal {
    def ~[C](that: Op[B, C]): Op[A, C] = Compose[A, B, C](op, that)
    def apply[M[X]](xs: M[A])(implicit z: Operable[M]): M[B] = z(xs)(op)
  }
}

object Operable {
  import Op._, all._

  implicit object OperableSize extends Operable[ConstSize] {
    def apply[A, B](in: Size)(op: Op[A, B]): Size = op match {
      case Reverse()       => in
      case Identity(_)     => in
      case Take(n)         => Size.min(in, n)
      case Drop(n)         => in - n
      case TakeRight(n)    => Size.min(in, n)
      case DropRight(n)    => in - n
      case Slice(range)    => in - min(range.startLong, range.size)
      case TakeWhile(_)    => in.atMost
      case DropWhile(_)    => in.atMost
      case Filter(p)       => in.atMost
      case Collect(pf)     => in.atMost
      case Maps(_)         => in
      case Prepend(xs)     => in + xs.size
      case Append(xs)      => in + xs.size
      case FlatMap(f)      => if (in.isZero) in else Size.Unknown
      case Compose(o1, o2) => apply(apply(in)(o1))(o2)
    }
  }
  implicit object OperableDoc extends Operable[ConstDoc] {
    private implicit def fundoc[A, B](f: A => B): Doc = f match {
      case x: ShowDirect => x.to_s
      case _             => "<f>"
    }
    def str(in: Doc, name: Doc, arg: Doc): Doc = fdoc"$in%s $name%7s $arg%-8s"

    def apply[A, B](in: Doc)(op: Op[A, B]): Doc = op match {
      case Identity(label) => label
      case Reverse()       => str(in, ".reverse", "")
      case Take(n)         => str(in, "take", n)
      case Drop(n)         => str(in, "drop", n)
      case TakeRight(n)    => str(in, "takeR", n)
      case DropRight(n)    => str(in, "dropR", n)
      case Slice(r)        => str(in, "slice", r)
      case TakeWhile(p)    => str(in, "takeW", p)
      case DropWhile(p)    => str(in, "dropW", p)
      case Filter(p)       => str(in, "filter", p)
      case Collect(pf)     => str(in, "collect", pf)
      case Maps(f)         => str(in, "map", f)
      case FlatMap(f)      => str(in, "flatMap", f)
      case Prepend(xs)     => str(in, "prepend", "<xs>")
      case Append(xs)      => str(in, "append", "<xs>")
      case Compose(o1, o2) => apply(apply(in)(o1))(o2)
    }
  }

  // No matter what hints we provide, scala won't figure out that
  // for a particular Op[A, B], A and B are the same type. So we
  // always have to cast.
  implicit object OperableView extends Operable[View] {
    def apply[A, B](xs: View[A])(op: Op[A, B]): View[B] = {
      val res: View[_] = op match {
        case Identity(_)     => xs
        case Reverse()       => xs.toVec.reverse
        case Take(n)         => xs take n
        case Drop(n)         => xs drop n
        case TakeRight(n)    => xs takeRight n
        case DropRight(n)    => xs dropRight n
        case Slice(range)    => xs slice range
        case TakeWhile(p)    => xs takeWhile p
        case DropWhile(p)    => xs dropWhile p
        case Filter(p)       => xs filter p
        case Collect(pf)     => xs collect pf
        case Maps(f)         => xs map f
        case FlatMap(f)      => xs flatMap f
        case Prepend(that)   => Each.join(that, xs).m
        case Append(that)    => Each.join(xs, that).m
        case Compose(o1, o2) => apply(apply(xs)(o1))(o2)
      }
      cast(res)
    }
  }
}
