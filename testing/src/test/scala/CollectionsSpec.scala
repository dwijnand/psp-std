package psp
package tests

import std._, all._

class CollectionsSpec extends ScalacheckBundle {
  def bundle = "Type Inference, General"
  def props  = pspProps ++ javaProps ++ scalaProps ++ jvmProps

  type A  = String
  type B  = Int
  type AB = String -> Int

  def in  = Array[AB]("a" -> 1, "b" -> 2, "c" -> 3)
  def arr = Array[B](1, 2, 3)

  val smap: sciMap[A, B]  = elems(in: _*)
  val sseq: sciSeq[AB]    = elems(in: _*)
  val svec: sciVector[AB] = elems(in: _*)
  val sset: sciSet[AB]    = elems(in: _*)
  val jseq: jList[AB]     = elems(in: _*)
  val jset: jSet[AB]      = elems(in: _*)
  val jmap: jMap[A, B]    = elems(in: _*)
  val pset: Pset[AB]      = elems(in: _*)
  val pvec: Vec[AB]       = elems(in: _*)

  def paired[A](x: A): A -> Int = x -> x.any_s.length

  import StdShow._

  def jvmProps = vec[NamedProp](
    expectTypes[String](
      remake("abc")(_ map identity),
      remake("abc")(_ map (_.toInt.toChar)),
      remake("abc")(_ map (_.toInt) map (_.toChar)),
      remake("abc")(_ flatMap (_.show * 3)),
      "abc" map identity build,
      "abc" map (_.toInt.toChar) build,
      "abc" map (_.toInt) map (_.toChar) build,
      "abc" flatMap (_.toString * 3) build,
      "abc" flatMap (_.toString * 3) build,
      "abc" map identity flatMap ("" + _) build
    ),
    expectTypes[Array[Int]](
      remake(arr)(_ map identity),
      remake(arr)(_ flatMap (x => vec(x))),
      remake(arr)(_ map (_.toString) map (_.toInt)),
      remake(arr)(_ map (_.toString) flatMap (_.toString) map (_.toInt)),
      arr.inPlace map identity,
      arr.inPlace.reverse,
      arr ++ arr,
      arr.m ++ arr.m force,
      arr.m.build,
      arr flatMap (x => vec(x)) build,
      arr flatMap (x => plist(x)) build,
      arr flatMap (x => view(x)) build,
      arr.m flatMap (x => vec(x)) build,
      arr.m flatMap (x => plist(x)) build,
      arr.m flatMap (x => view(x)) build
    ),
    expectTypes[Array[Long]](
      make[Array[Long]](1L to 10),
      make[Array[Long]](1L to 10 m),
      make[Array[Long]](1L to 10 toVec),
      make[Array[Long]](1L to 10L),
      make[Array[Long]](1L to 10L m),
      make[Array[Long]](1L to 10L toVec),
      arr.map(_.toLong).to[Array].inPlace.reverse
    )
  )

  def scalaProps = vec[NamedProp](
    expectTypes[sciSet[AB]](
      sset map identity,
      sset build,
      sset map identity build,
      sset map fst map paired,
      sset.m build,
      sset.m map identity build,
      sset.m map fst map paired build
    ),
    expectTypes[sciMap[A, B]](
      smap force,
      smap map identity,
      smap map identity force,
      // smap map fst map paired build,
      smap map fst map paired force,
      smap.m build,
      smap.m map identity build,
      smap.m map fst map paired build
    ),
    expectTypes[sciSeq[AB]](
      sseq map identity,
      sseq build,
      sseq map identity build,
      sseq map fst map paired,
      sseq map fst map paired build,
      sseq.m build,
      sseq.m map identity build,
      sseq.m map fst map paired build
    ),
    expectTypes[sciVector[AB]](
      svec map identity,
      svec build,
      svec map identity build,
      svec map fst map paired,
      svec map fst map paired build,
      svec.m build,
      svec.m map identity build,
      svec.m map fst map paired build
    )
  )

  def javaProps = {
    vec[NamedProp](
      expectTypes[jList[AB]](
        jseq build,
        jseq map identity build,
        jseq map fst map paired build,
        jseq.m build,
        jseq.m map identity build,
        jseq.m map fst map paired build
      ),
      expectTypes[jSet[AB]](
        jset build,
        jset map identity build,
        jset map fst map paired build,
        jset.m build,
        jset.m map identity build,
        jset.m map fst map paired build
      ),
      expectTypes[jMap[A, B]](
        jmap build,
        // jmap map identity,
        jmap map identity build,
        jmap map identity force,
        // jmap map fst map paired,
        jmap map fst map paired build,
        jmap.m build,
        // jmap.m map identity,
        jmap.m map identity build,
        jmap.m map identity force,
        jmap.m map fst map paired build,
        jmap.m map fst map paired force
      )
    )
  }

  def pspProps: Vec[NamedProp] = {
    vec(
      expectTypes[Pset[AB]](
        pset build,
        pset mapToSet identity,
        pset mapToSet fst mapToSet paired,
        pset.m build,
        pset.m map identity build,
        pset.m map fst map paired build
      ),
      expectTypes[Pmap[AB, Int]](
        pset map (x => fst(x).length)
      ),
      expectTypes[Vec[AB]](
        pvec build,
        pvec map identity build,
        pvec map fst map paired build,
        pvec.m build,
        pvec.m map identity build,
        pvec.m map fst map paired build
      )
    )
  }
}
