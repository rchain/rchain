package coop.rchain.rspace

import coop.rchain.rspace.ReportingRspace.{
  ReportingComm,
  ReportingConsume,
  ReportingEvent,
  ReportingProduce
}

/**
  * The purpose of the reportingTransformer is to create transformer to transform reporting events like
  * `ReportingProduce`, `ReportingConsume` and `ReportingComm`(see coop.rchain.rspace.ReportingRspace) into
  * something else which is more readable or easier to interact.
  */
trait ReportingTransformer[C, P, A, K, E] {
  type RhoReportingComm    = ReportingComm[C, P, A, K]
  type RhoReportingProduce = ReportingProduce[C, A]
  type RhoReportingConsume = ReportingConsume[C, P, K]

  def serializeConsume(rc: RhoReportingConsume): E

  def serializeProduce(rp: RhoReportingProduce): E

  def serializeComm(rcm: RhoReportingComm): E

  def transformEvent(re: ReportingEvent): E =
    re match {
      case comm @ ReportingComm(_, _) => serializeComm(comm.asInstanceOf[RhoReportingComm])
      case cons @ ReportingConsume(_, _, _, _) =>
        serializeConsume(cons.asInstanceOf[RhoReportingConsume])
      case prod @ ReportingProduce(_, _) => serializeProduce(prod.asInstanceOf[RhoReportingProduce])
    }
}

object ReportingTransformer {
  object ReportingRhoStringTransformer {
    sealed trait RhoEvent

    final case class RhoComm(consume: RhoConsume, produces: List[RhoProduce]) extends RhoEvent

    final case class RhoProduce(channel: String, data: String) extends RhoEvent

    final case class RhoConsume(channels: String, patterns: String, continuation: String)
        extends RhoEvent

    class ReportingEventStringTransformer[C, P, A, K](
        serializeC: C => String,
        serializeP: P => String,
        serializeA: A => String,
        serializeK: K => String
    ) extends ReportingTransformer[C, P, A, K, RhoEvent] {
      override def serializeConsume(
          rc: RhoReportingConsume
      ): RhoConsume = {
        val k   = serializeK(rc.continuation)
        val chs = rc.channels.map(serializeC).mkString("[", ";", "]")
        val ps  = rc.patterns.map(serializeP).mkString("[", ";", "]")
        RhoConsume(channels = chs, patterns = ps, continuation = k)
      }

      override def serializeProduce(rp: RhoReportingProduce): RhoProduce = {
        val d  = serializeA(rp.data)
        val ch = serializeC(rp.channel)
        RhoProduce(channel = ch, data = d)
      }

      override def serializeComm(rcm: RhoReportingComm): RhoComm =
        RhoComm(
          consume = serializeConsume(rcm.consume),
          produces = rcm.produces.map(serializeProduce).toList
        )
    }
  }
}
