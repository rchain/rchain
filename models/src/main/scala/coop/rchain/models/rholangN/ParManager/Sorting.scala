package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object Sorting {
  def sort(seq: Seq[ParN]): Seq[ParN] = seq.sorted(Ordering.by((p: ParN) => p.rhoHash.bytes))
}
