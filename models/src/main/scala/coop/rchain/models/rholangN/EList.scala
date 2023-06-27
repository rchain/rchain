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
