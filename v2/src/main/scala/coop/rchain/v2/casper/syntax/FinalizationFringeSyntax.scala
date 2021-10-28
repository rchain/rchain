package coop.rchain.v2.casper.syntax

import coop.rchain.v2.casper.data.FinalizationFringe

trait FinalizationFringeSyntax {
  implicit final def FinalizationFringeSyntax[M, S](
      v: FinalizationFringe[M, S]
  ): FinalizationFringeOps[M, S] = new FinalizationFringeOps[M, S](v)
}

final class FinalizationFringeOps[M, S](val ff: FinalizationFringe[M, S]) extends AnyVal {
  def diff(that: FinalizationFringe[M, S]): Set[M] = FinalizationFringe.diff(ff, that)
}
