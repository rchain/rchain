package coop.rchain.models.rholangN

import scala.collection.immutable.TreeSet

/**
  * Ordered collection of 0 or more processes.
  *
  * @param ps The sequence of any Rholang processes
  * @param remainder Remainder of a list elements. This var used in matching (pattern of a head/tail pair)
  */
final class EListN(val ps: Seq[ParN], val remainder: Option[VarN]) extends CollectionN
object EListN {
  def apply(ps: Seq[ParN] = Seq(), r: Option[VarN] = None): EListN = new EListN(ps, r)
  def apply(p: ParN): EListN                                       = apply(Seq(p), None)
}

/**
  * Ordered collection of 1 or more processes.
  * @param ps The non-empty sequence of any Rholang processes
  */
final class ETupleN private (val ps: Seq[ParN]) extends CollectionN
object ETupleN {
  def apply(ps: Seq[ParN]): ETupleN = {
    assert(ps.nonEmpty, "Cannot create ETuple with an empty par sequence")
    new ETupleN(ps)
  }
  def apply(p: ParN): ETupleN = apply(Seq(p))
}

/**
  * A Rholang set is an unordered collection of 0 or more processes.
  * @param ps The sequence of any Rholang processes
  * @param remainder Remainder of a list elements. This var used in matching (pattern of a head/tail pair)
  */
final class ESetN(private val ps: TreeSet[ParN], val remainder: Option[VarN]) extends CollectionN {
  def +(elem: ParN): ESetN = ESetN(ps + elem, remainder)
  def -(elem: ParN): ESetN = ESetN(ps - elem, remainder)

  def contains(elem: ParN): Boolean = ps.contains(elem)

  def union(that: ESetN): ESetN = ESetN(ps.union(that.ps), None)

  def sortedPs: Seq[ParN] = ps.toSeq
}
object ESetN {
  private object ParOrdering extends Ordering[ParN] {
    def compare(a: ParN, b: ParN): Int = a.rhoHash.bytes compare b.rhoHash.bytes
  }
  def apply(ps: Seq[ParN] = Seq(), r: Option[VarN] = None): ESetN =
    new ESetN(TreeSet.from(ps)(ParOrdering), r)

  def apply(p: ParN): ESetN = ESetN(Seq(p), None)

  def empty: ESetN = ESetN()

  private def apply(ps: TreeSet[ParN], remainder: Option[VarN]): ESetN = new ESetN(ps, remainder)
}
