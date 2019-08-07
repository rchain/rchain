package coop.rchain.casper.protocol

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import com.google.protobuf.ByteString
import coop.rchain.models.PCost

final case class BlockMessageN(
    blockHash: ByteString,
    header: HeaderN,
    body: BodyN,
    justifications: List[JustificationN],
    sender: ByteString,
    seqNum: Int,
    sig: ByteString,
    sigAlgorithm: String,
    shardId: String,
    extraBytes: ByteString
)

object BlockMessageN {
  def from(bm: BlockMessage): Either[String, BlockMessageN] =
    for {
      header <- bm.header.toRight("Header not available").map(HeaderN.from)
      body   <- bm.body.toRight("Body not available") >>= (BodyN.from)
    } yield BlockMessageN(
      bm.blockHash,
      header,
      body,
      bm.justifications.toList.map(JustificationN.from),
      bm.sender,
      bm.seqNum,
      bm.sig,
      bm.sigAlgorithm,
      bm.shardId,
      bm.extraBytes
    )
}

final case class HeaderN(
    parentsHashList: List[ByteString],
    postStateHash: ByteString,
    deploysHash: ByteString,
    timestamp: Long,
    version: Long,
    deployCount: Int,
    extraBytes: ByteString
)

object HeaderN {
  def from(h: Header): HeaderN = HeaderN(
    h.parentsHashList.toList,
    h.postStateHash,
    h.deploysHash,
    h.timestamp,
    h.version,
    h.deployCount,
    h.extraBytes
  )
}

final case class BodyN(
    state: RChainStateN,
    deploys: List[ProcessedDeployN],
    extraBytes: ByteString
)

object BodyN {
  def from(b: Body): Either[String, BodyN] =
    for {
      state   <- b.state.toRight("RChainState not available").map(RChainStateN.from)
      deploys <- b.deploys.toList.traverse(ProcessedDeployN.from)
    } yield BodyN(state, deploys, b.extraBytes)
}

final case class JustificationN(
    validator: ByteString,
    latestBlockHash: ByteString
)

object JustificationN {
  def from(j: Justification): JustificationN = JustificationN(
    j.validator,
    j.latestBlockHash
  )
}

final case class RChainStateN(
    preStateHash: ByteString,
    postStateHash: ByteString,
    bonds: List[BondN],
    blockNumber: Long
)

object RChainStateN {
  def from(rchs: RChainState): RChainStateN =
    RChainStateN(
      rchs.preStateHash,
      rchs.postStateHash,
      rchs.bonds.toList.map(BondN.from),
      rchs.blockNumber
    )
}

final case class ProcessedDeployN(
    deploy: DeployDataN,
    cost: PCost,
    deployLog: List[EventN],
    paymentLog: List[EventN],
    errored: Boolean
)

object ProcessedDeployN {
  def from(pd: ProcessedDeploy): Either[String, ProcessedDeployN] =
    for {
      ddn        <- pd.deploy.toRight("DeployData not available").map(DeployDataN.from)
      cost       <- pd.cost.toRight("Cost not available")
      deployLog  <- pd.deployLog.toList.traverse(EventN.from)
      paymentLog <- pd.paymentLog.toList.traverse(EventN.from)
    } yield ProcessedDeployN(
      ddn,
      cost,
      deployLog,
      paymentLog,
      pd.errored
    )
}

final case class DeployDataN(
    deployer: ByteString,
    term: String,
    timestamp: Long,
    sig: ByteString,
    sigAlgorithm: String,
    phloPrice: Long,
    phloLimit: Long,
    validAfterBlockNumber: Long
)

object DeployDataN {
  def from(dd: DeployData): DeployDataN = DeployDataN(
    dd.deployer,
    dd.term,
    dd.timestamp,
    dd.sig,
    dd.sigAlgorithm,
    dd.phloPrice,
    dd.phloLimit,
    dd.validAfterBlockNumber
  )
}

sealed trait EventN
final case class ProduceEventN(
    channelsHash: ByteString,
    hash: ByteString,
    persistent: Boolean,
    sequenceNumber: Int
) extends EventN
final case class ConsumeEventN(
    channelsHashes: List[ByteString],
    hash: ByteString,
    persistent: Boolean,
    sequenceNumber: Int
) extends EventN
final case class CommEventN(consume: ConsumeEventN, produces: List[ProduceEventN]) extends EventN

object EventN {
  def from(e: Event): Either[String, EventN] =
    e.eventInstance match {
      case Event.EventInstance.Produce(pe) => fromProduceEvent(pe).asRight[String]
      case Event.EventInstance.Consume(ce) => fromConsumeEvent(ce).asRight[String]
      case Event.EventInstance.Comm(CommEvent(Some(ce), pes)) =>
        CommEventN(fromConsumeEvent(ce), pes.toList.map(fromProduceEvent)).asRight[String]
      case Event.EventInstance.Comm(CommEvent(None, _)) => "CommEvent does not have a consume event in it" 
      case Event.EventInstance.Empty                    => "Received malformed Event: Empty"
    }

  private def fromConsumeEvent(ce: ConsumeEvent): ConsumeEventN =
    ConsumeEventN(ce.channelsHashes.toList, ce.hash, ce.persistent, ce.sequenceNumber)
  private def fromProduceEvent(pe: ProduceEvent): ProduceEventN =
    ProduceEventN(pe.channelsHash, pe.hash, pe.persistent, pe.sequenceNumber)
}

final case class BondN(
    validator: ByteString,
    stake: Long
)

object BondN {
  def from(b: Bond): BondN = BondN(b.validator, b.stake)
}
