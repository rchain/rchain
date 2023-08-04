package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

import scala.math.Ordered.orderingToOrdered

@SuppressWarnings(Array("org.wartremover.warts.Return", "org.wartremover.warts.Var"))
private[parmanager] object Sorting {
  implicit val o = new math.Ordering[Array[Byte]] {
    def compare(a: Array[Byte], b: Array[Byte]): Int =
      if (a eq null) {
        if (b eq null) 0
        else -1
      } else if (b eq null) 1
      else {
        val L = math.min(a.length, b.length)
        var i = 0
        while (i < L) {
          if (a(i) < b(i)) return -1
          else if (b(i) < a(i)) return 1
          i += 1
        }
        if (L < b.length) -1
        else if (L < a.length) 1
        else 0
      }
  }

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
