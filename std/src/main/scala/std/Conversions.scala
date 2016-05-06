package psp
package std

import api._, all._, Java._

final class Conversions[A](val xs: View[A]) extends AnyVal with ConversionsImpl[A]

final class Unbuilder[A, Repr](repr: Repr)(implicit z: UnbuildsAs[A, Repr]) {
  // def xs: View[A]            = m
  def m: AtomicView[A, Repr] = new LinearView(Each each (z unbuild repr))
}
object Unbuilds {
  def apply[A, R](f: R => Foreach[A]): UnbuildsAs[A, R] = new Impl[A, R](f)
  // def impl[A, R](f: UnbuildsAs[A, R]): Impl[A, R]       = new Impl(f unbuild _)

  final class Impl[A, Repr](val f: Repr => Foreach[A]) extends AnyVal with Unbuilds[Repr] {
    type Elem = A
    def unbuild(xs: Repr): Foreach[A] = f(xs)
  }
}

/** Conversions which require the elements to be pairs. Obtaining evidence of that
 *  up front simplifies everything else, because we don't have to mix and match
 *  between arity-1 and arity-2 type constructors.
 */
final class Paired[R, A, B](xs: Each[R])(implicit splitter: Splitter[R, A, B]) {
  // def toEqualsMap: ExMap[A, B]                                       = toExMap(inheritEq)
  def toExMap(implicit z: Eq[A]): ExMap[A, B]                        = toMap[ExMap]
  // def toHashMap(implicit z: Hash[A]): ExMap[A, B]                    = toMap[ExMap]
  // def toJavaMap(): jMap[_ >: A, _ <: B]                              = toMap[jMap]
  def toMap[CC[_,_]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z build (xs map splitter.split)
  // def toScalaMap(): sciMap[A, B]                                     = toMap[sciMap]
}

trait ConversionsImpl[A] extends Any {
  def xs: View[A]
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs

  def byEquals: HasHashOps[A]   = new HasHashOps[A](xs)(inheritEq)
  def byRef: HasHashOps[Ref[A]] = new HasHashOps[Ref[A]](xs.toRefs)(referenceEq)
  def byString: HasHashOps[A]   = new HasHashOps[A](xs)(stringEq)

  def toArray(implicit z: CTag[A]): Array[A]   = to[Array]
  // def toDirect: Direct[A]                      = to[Direct]
  // def toEach: Each[A]                          = to[Each]
  def toExSet(implicit z: Eq[A]): ExSet[A]     = to[ExSet]
  def toHashSet(implicit z: Hash[A]): ExSet[A] = to[ExSet]
  // def toJavaList: jList[A]                     = to[jList]
  def toJavaSet: jSet[A]                       = to[jSet]
  def toPlist: Plist[A]                        = to[Plist]
  // def toRefArray(): Array[Ref[A]]              = Builds.array[Ref[A]] build xs.toRefs
  // def toScalaList: sciList[A]                  = to[sciList]
  // def toScalaSeq: sciSeq[A]                    = to[sciSeq]
  // def toScalaSet: sciSet[A]                    = to[sciSet]
  def toScalaVector: sciVector[A]              = to[sciVector]
  def toVec: Vec[A]                            = to[Vec]

  def iterator: BiIterator[A] = BiIterator(xs)
}

trait Constructions[M[X]] {
  def construct[A](size: Size, mf: Suspended[A]): M[A]

  def const[A](elem: A): M[A]         = pure(mf => while(true) mf(elem))
  def each[A](xs: Foreach[A]): M[A]   = construct(xs.size, xs foreach _)
  def elems[A](xs: A*): M[A]          = scala(xs.seq)
  // def empty[A] : M[A]                 = construct(Size.Zero, vec() foreach _)
  def java[A](xs: jIterable[A]): M[A] = pure(BiIterable(xs) foreach _)
  def pure[A](f: Suspended[A]): M[A]  = construct(Size.Unknown, f)

  def javaMap[A,B](xs: jMap[A, B]): M[A->B]   = construct(xs.size, mf => xs.keySet foreach (k => mf((k, xs get k))))
  def scalaMap[A,B](xs: scMap[A, B]): M[A->B] = construct(xs.size, xs foreach _)

  def scala[A](xs: GTOnce[A]): M[A] = xs match {
    case xs: scTraversable[A] => construct(xs.size, xs foreach _)
    case _                    => scala(xs.to[sciStream])
  }
}
