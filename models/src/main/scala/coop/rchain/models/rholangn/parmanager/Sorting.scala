package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

import java.util
import scala.math.Ordered.orderingToOrdered

private[parmanager] object Sorting {
  implicit val o: Ordering[Array[Byte]] = (a: Array[Byte], b: Array[Byte]) =>
    util.Arrays.compare(a, b)

  def sortPars(ps: Seq[ParN]): Seq[ParN] = ps.sorted(Ordering.by((p: ParN) => p.rhoHash))
  def sortBinds(bs: Seq[ReceiveBindN]): Seq[ReceiveBindN] =
    bs.sorted(Ordering.by((b: ReceiveBindN) => b.rhoHash))
  def sortBindsWithT[T](bs: Seq[(ReceiveBindN, T)]): Seq[(ReceiveBindN, T)] =
    bs.sortBy(_._1.rhoHash)
  def sortUris(uris: Seq[String]): Seq[String] = uris.sorted
  def sortInjections(injections: Map[String, ParN]): Seq[(String, ParN)] =
    injections.toSeq.sortBy(_._1)
  def comparePars(p1: ParN, p2: ParN): Int = p1.rhoHash compare p2.rhoHash
}
