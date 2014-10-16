package psp
package tests

import psp.std._

class Collections extends ScalacheckBundle {
  def bundle = "Type Inference, General"

  val bits = sciBitSet(1, 2, 3)
  val arr  = Array[Int](1, 2, 3)
  val smap = sciMap("a" -> 1, "b" -> 2, "c" -> 3)
  val sseq = scSeq("a" -> 1, "b" -> 2, "c" -> 3)
  val svec = sciVector("a" -> 1, "b" -> 2, "c" -> 3)
  val sset = sciSet("a" -> 1, "b" -> 2, "c" -> 3)

  def paired[A](x: A): (A, Int) = x -> ("" + x).length

  def props: Seq[NamedProp] = policyProps ++ Seq(
    expectTypes[sciBitSet](
      bits map identity,
      bits.m map (_.toString.length) build,
      bits.m map (_.toString) map (_.length) build,
      bits.m map (x => Seq(x)) map (_.size) build,
      bits.m map (x => Seq(x).size) build
    ),
    expectTypes[String](
      "abc" map identity,
      "abc" map (_.toInt.toChar),
      "abc".m flatMap (_.toString * 3 m) build,
      "abc" flatMap (_.toString * 3)
    ),
    expectTypes[Array[Int]](
      arr mapInPlace identity,
      arr.m.build,
      arr.m flatMap (x => fromElems(x)) build,
      arr.flatMap(x => fromElems(x)).force[Array[Int]]
    ),
    expectTypes[sciSet[_]](
      sset map identity,
      sset.m build,
      sset.m map identity build,
      sset.m.map(_._1) map paired build
    ),
    expectTypes[sciMap[_, _]](
      (smap map identity).force[sciMap[_, _]],
      smap.m build,
      smap.m map identity build,
      smap.m map (_._1) map identity map paired build
    ),
    expectTypes[scSeq[_]](
      sseq map identity,
      sseq.m build,
      sseq.m map identity build,
      sseq.m.map(_._1).map(paired).force[scSeq[_]]
    ),
    expectTypes[sciVector[_]](
      svec map identity,
      svec.m.build,
      svec.m map identity build,
      svec.m.map(_._1).map(paired).force[sciVector[_]]
    )
  )

  def policyProps: Seq[NamedProp] = {
    import StdEq._
    // val pmap = newMap("a" -> 1, "b" -> 2, "c" -> 3)
    val pset = newSet("a" -> 1, "b" -> 2, "c" -> 3)
    val pseq = newSeq("a" -> 1, "b" -> 2, "c" -> 3)

    Seq(
      // expectTypes[Object, pMap[_, _]](
      //   pmap map identity,
      //   pmap.m.build,
      //   pmap.m map identity build,
      //   pmap.m.map(_._1).map(paired).force[pMap[_, _]]
      // ),
      expectTypes[pSet[_]](
        pset.m map identity build,
        pset.m.build,
        pset.m map identity build,
        pset.m.map(_._1).map(paired).force[pSet[_]]
      ),
      expectTypes[pSeq[_]](
        pseq map identity,
        pseq.m.build,
        pseq.m map identity build,
        pseq.m.map(_._1).map(paired).force[pSeq[_]]
      )
    )
  }
}
