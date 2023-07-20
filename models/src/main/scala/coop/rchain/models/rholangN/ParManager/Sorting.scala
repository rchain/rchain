package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object Sorting {
  def sortPars(ps: Seq[ParN]): Seq[ParN] = ps.sorted(Ordering.by((p: ParN) => p.rhoHash.bytes))
  def sortBinds(bs: Seq[ReceiveBindN]): Seq[ReceiveBindN] =
    bs.sorted(Ordering.by((b: ReceiveBindN) => b.rhoHash.bytes))
  def sortUris(uris: Seq[String]): Seq[String] = uris.sorted
  def sortInjections(injections: Map[String, ParN]): Seq[(String, ParN)] =
    injections.toSeq.sortBy(_._1)
  def comparePars(p1: ParN, p2: ParN): Int = p1.rhoHash.bytes compare p2.rhoHash.bytes
}
