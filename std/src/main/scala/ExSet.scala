package psp
package std

import api._, all._

object ExSet {
  def apply[A: Eq](xs: Each[A]): ExSet[A]  = new Impl[A](xs, ?)
  def elems[A: Eq](xs: A*): ExSet[A]       = apply[A](vec(xs: _*))
  def fromJava[A](xs: jSet[A]): ExSet[A]   = new FromJava(xs)
  def fromScala[A](xs: scSet[A]): ExSet[A] = new FromScala(xs.toSet)

  def impl[A](xs: ExSet[A]): Impl[A] = xs match {
    case xs: Impl[A] => xs
    case _           => new Impl[A](xs.toEach, Eq(xs.equiv))
  }

  sealed trait ExSetImpl[A] extends ExSet[A] with ToBool[A] {
    def eqs: Eq[A]
    def equiv(x: A, y: A): Boolean = eqs.eqv(x, y)
  }
  class FromScala[A](xs: sciSet[A]) extends ExSetImpl[A] {
    def size: Precise                 = Size(xs.size)
    @inline def foreach(f: A => Unit) = xs foreach f
    def apply(elem: A)                = xs(elem)
    def eqs                           = Eq.inherit()
  }
  class FromJava[A](xs: jSet[A]) extends ExSetImpl[A] {
    def size: Precise                 = Size(xs.size)
    @inline def foreach(f: A => Unit) = xs.iterator foreach f
    def apply(elem: A)                = xs contains elem
    def eqs                           = Eq.inherit()
  }

  sealed trait Derived[A] extends ExSetImpl[A] {
    protected def underlying: ExSet[A]
    def eqs = underlying.eqs
  }
  final case class Filtered[A](lhs: ExSet[A], p: ToBool[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && p(elem)
    @inline def foreach(f: A => Unit): Unit = lhs foreach (x => if (p(x)) f(x))
    def size                                = lhs.size.atMost
  }
  final case class Intersect[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filter rhs foreach f
    def size                                = lhs.size intersect rhs.size
  }
  final case class Union[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) || rhs(elem)
    @inline def foreach(f: A => Unit): Unit = Each.join(lhs, rhs filterNot lhs) foreach f
    def size                                = lhs.size union rhs.size
  }
  final case class Diff[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && !rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filterNot rhs foreach f
    def size                                = lhs.size diff rhs.size
  }
  final class Impl[A](basis: Each[A], val eqs: Eq[A]) extends ExSetImpl[A] {
    private[this] def wrap(x: A): Wrap    = new Wrap(x)
    private[this] val wrapSet: jSet[Wrap] = basis map wrap toJavaSet
    private class Wrap(val value: A) {
      override def equals(that: Any): Boolean = that match {
        case x: Wrap => eqs.eqv(value, x.value)
        case _       => false
      }
      override def hashCode = value.##
      override def toString = value.any_s
    }

    def \(relation: Relation[A]): Quotient[A] = new Quotient[A](basis)(Eq(relation))
    def +(elem: A): ExSet[A]                  = this union ExSet.elems(elem)(eqs)
    def apply(elem: A)                        = wrapSet contains wrap(elem)
    def size: Precise                         = wrapSet.size
    @inline def foreach(f: A => Unit): Unit   = wrapSet foreach (x => f(x.value))
  }

  final class Quotient[A](basis: Each[A])(implicit val eqs: Eq[A]) extends ExSetImpl[A] {
    private[this] val set = basis.foldl(sciSet[A]())((res, x) => if (res exists (_ === x)) res else res + x)
    def size: Precise               = set.size
    def apply(x: A): Boolean        = set(x)
    def foreach(x: A => Unit): Unit = set foreach x
  }
}
