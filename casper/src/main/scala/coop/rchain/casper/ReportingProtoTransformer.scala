package coop.rchain.casper

import cats.syntax.all._
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rspace.{ReportingRspace, ReportingTransformer}
import coop.rchain.casper.protocol.{
  PeekProto,
  ReportCommProto,
  ReportConsumeProto,
  ReportEventProto,
  ReportProduceProto
}

class ReportingProtoTransformer
    extends ReportingTransformer[
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation,
      ReportEventProto
    ] {
  override def serializeConsume(
      rc: RhoReportingConsume
  ): ReportConsumeProto =
    ReportConsumeProto(
      rc.channels,
      rc.patterns,
      rc.peeks.map(PeekProto(_))
    )

  override def serializeProduce(rp: RhoReportingProduce): ReportProduceProto =
    ReportProduceProto(channel = rp.channel.some, data = rp.data.some)

  override def serializeComm(rcm: RhoReportingComm): ReportCommProto =
    ReportCommProto(
      consume = serializeConsume(rcm.consume).some,
      produces = rcm.produces.map(serializeProduce).toList
    )

  override def transformEvent(re: ReportingRspace.ReportingEvent): ReportEventProto =
    super.transformEvent(re)
}
