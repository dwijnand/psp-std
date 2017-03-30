package psp
package tests

import psp.std._, all._, StdShow._

class SliceSpec extends ScalacheckBundle {
  def bundle = "Slice Operations"
  def checkSlice[A: Eq: Show](xs: Direct[A], start: Int, end: Int, expect: Direct[A]): Direct[NamedProp] = vec(
    pp"$xs.slice($start, $end) === $expect"              -> Prop(xs.o(_ slice (start indexUntil end)) === expect),
    pp"$xs drop $start take ($end - $start) === $expect" -> Prop(xs.o(_ drop start take end - start) === expect)
  )

  def props = checkSlice('a' to 'g', 2, 5, 'c' to 'e')
}

class InferenceSpec extends ScalacheckBundle {
  def bundle = "Type Inference, Views"

  val as: Array[Int]     = elems(1, 2, 3)
  val ds: Direct[Int]    = elems(1, 2, 3)
  val fs: Each[Int]      = elems(1, 2, 3)
  val ls: sciList[Int]   = elems(1, 2, 3)
  val ss: String         = "123"
  val vs: sciVector[Int] = elems(1, 2, 3)
  val xs: Pset[Int]      = elems(1, 2, 3)

  val b1 = as o (_ map identity)
  val b2 = ds o (_ map identity)
  val b3 = fs o (_ map identity)
  val b4 = ls o (_ map identity)
  val b5 = ss o (_ map identity)
  val b6 = vs o (_ map identity)
  val b7 = xs mapToSet identity

  def ptBuild = vec[NamedProp](
    expectType[Array[Int]](b1),
    expectType[Direct[Int]](b2),
    expectType[Each[Int]](b3),
    expectType[sciList[Int]](b4),
    expectType[String](b5),
    expectType[sciVector[Int]](b6),
    expectType[Pset[Int]](b7)
  )
  def ptArray = expectTypes[Array[Int]](
    as.m map identity build,
    as.m map identity force,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity force,
    xs.m map identity force,
    as map identity force
  )
  def ptView = expectTypes[View[Int]](
    as.m map identity,
    ds.m map identity,
    fs.m map identity,
    ls.m map identity,
    ss.m map identity map (_.toInt),
    vs.m map identity,
    xs.m map identity,
    as map identity,
    ds map identity,
    fs map identity,
    ls map identity,
    ss map identity map (_.toInt),
    vs map identity
    // xs map identity
  )
  def ptVector = expectTypes[sciVector[Int]](
    as.m map identity force,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity build,
    vs.m map identity force,
    xs.m map identity force
  )

  def props: Vec[NamedProp] = make(
    vec(ptArray, ptView, ptVector) ++ ptBuild ++ vec(
      expectType[Array[Char]](ss.m map identity force),
      expectType[String](ss o (_ map identity)),
      expectType[String](ss o (_ map identity)),
      expectType[String](ss o (_.m map identity)),
      expectType[String](ss map identity build),
      expectType[String](ss.m map identity force),
      expectType[View[Char]](ss.m map identity),
      expectType[Pset[Int]](xs.m map identity force),
      expectType[Each[Int]](fs o (_ map identity)),
      expectType[Each[Int]](fs.m map identity force),
      expectType[Direct[Int]](ds map identity build),
      expectType[Direct[Int]](ds.m map identity force),
      expectType[Direct[Int]](vs.m map identity force),
      expectType[sciList[Int]](ls map identity),
      expectType[sciList[Int]](ls.m map identity force),
      expectType[sciVector[Int]](vs map identity)
    )
  )
}
