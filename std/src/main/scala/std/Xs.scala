package psp
package std

import api._, all._, StdShow._

/** Reboot of view code.
 */
object Xs {
  trait RepInfo[Rep] {
    type MapTo[X]
    type Elem

    def sizeOf(xs: Rep): Size
    def foreachView[B](xs: Rep, op: Op[Elem, B], f: B => Unit): Unit
  }
  trait RepOf[Rep, CC[X], A] extends RepInfo[Rep] {
    type MapTo[X] = CC[X]
    type Elem     = A
  }

  implicit def sciVectorRepOf[A] : RepOf[sciVector[A], sciVector, A] = new RepOf[sciVector[A], sciVector, A] {
    def sizeOf(xs: sciVector[A]): Precise                                  = xs.length
    def foreachView[B](xs: sciVector[A], op: Op[A, B], f: B => Unit): Unit = op(xs.m) foreach f
  }
  implicit def sciListRepOf[A] : RepOf[sciList[A], sciList, A] = new RepOf[sciList[A], sciList, A] {
    def sizeOf(xs: sciList[A]): Size                                     = cond(xs.isEmpty, Size.Zero, Size.NonEmpty)
    def foreachView[B](xs: sciList[A], op: Op[A, B], f: B => Unit): Unit = op(xs.m) foreach f
  }

  class RepWithOp[Rep, CC[X], Elem, A](val xs: Rep, val op: Op[Elem, A], val info: RepOf[Rep, CC, Elem]) extends ShowSelf {
    type MapTo[B] = RepWithOp[Rep, CC, Elem, B]
    private implicit def applyNext[B](next: Op[A, B]): MapTo[B] = new RepWithOp(xs, op ~ next, info)

    def collect[B](pf: A ?=> B): MapTo[B]        = Op.Collect(pf)
    def drop(n: Precise): MapTo[A]               = Op.Drop[A](n)
    def dropRight(n: Precise): MapTo[A]          = Op.DropRight[A](n)
    def dropWhile(p: ToBool[A]): MapTo[A]        = Op.DropWhile(p)
    def filter(p: ToBool[A]): MapTo[A]           = Op.Filter(p)
    def flatMap[B](f: A => Foreach[B]): MapTo[B] = Op.FlatMap(f)
    def map[B](f: A => B): MapTo[B]              = Op.Maps(f)
    def take(n: Precise): MapTo[A]               = Op.Take[A](n)
    def takeRight(n: Precise): MapTo[A]          = Op.TakeRight[A](n)
    def takeWhile(p: ToBool[A]): MapTo[A]        = Op.TakeWhile(p)
    def withFilter(p: ToBool[A]): MapTo[A]       = Op.Filter(p)

    object force { def apply[That](implicit z: Builds[A, That]): That = z build Each(foreach _) }
    def build(implicit z: Builds[A, Rep]): Rep                        = force[Rep]

    def foreach(f: A => Unit): Unit = info.foreachView(xs, op, f)
    def size: Size                  = op[ConstSize](info sizeOf xs)
    def to_s: String                = render(op)
  }

  def apply[R, CC[X], A](xs: R)(implicit info: RepOf[R, CC, A]): RepWithOp[R, CC, A, A] =
    new RepWithOp(xs, Op.Identity(classNameOf(xs)), info)
}
