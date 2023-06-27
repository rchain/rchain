package coop.rchain.models.rholangN

/**
  * Ordered collection of 0 or more processes.
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
