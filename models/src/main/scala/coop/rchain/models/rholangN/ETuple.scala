package coop.rchain.models.rholangN

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
