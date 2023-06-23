package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object Sorting {
  def sortPars(ps: Seq[ParN]): Seq[ParN]         = ps.sorted(Ordering.by((p: ParN) => p.rhoHash.bytes))
  def sortStrings(seq: Seq[String]): Seq[String] = seq.sorted
}
