package coop.rchain.casper.protocol

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import com.google.protobuf.ByteString
import coop.rchain.models.PCost

sealed trait CasperMessage {
  def toProto: CasperMessageProto
}

object CasperMessage {
  def from(cm: CasperMessageProto): Either[String, CasperMessage] = cm match {
    case bmp: BlockMessageProto           => BlockMessage.from(bmp)
    case p: ApprovedBlockCandidateProto   => ApprovedBlockCandidate.from(p)
    case p: ApprovedBlockProto            => ApprovedBlock.from(p)
    case p: ApprovedBlockRequestProto     => Right(ApprovedBlockRequest.from(p))
    case p: BlockApprovalProto            => BlockApproval.from(p)
    case p: BlockRequestProto             => Right(BlockRequest.from(p))
    case _: ForkChoiceTipRequestProto     => Right(ForkChoiceTipRequest)
    case p: HasBlockProto                 => Right(HasBlock.from(p))
    case p: HasBlockRequestProto          => Right(HasBlockRequest.from(p))
    case p: NoApprovedBlockAvailableProto => Right(NoApprovedBlockAvailable.from(p))
    case p: UnapprovedBlockProto          => UnapprovedBlock.from(p)
  }
}

final case class HasBlockRequest(hash: ByteString) extends CasperMessage {
  def toProto: HasBlockRequestProto = HasBlockRequestProto(hash)
}

object HasBlockRequest {
  def from(hbr: HasBlockRequestProto): HasBlockRequest = HasBlockRequest(hbr.hash)
}

final case class HasBlock(hash: ByteString) extends CasperMessage {
  def toProto: HasBlockProto = HasBlockProto(hash)
}

object HasBlock {
  def from(hbr: HasBlockProto): HasBlock = HasBlock(hbr.hash)
}

final case class BlockRequest(hash: ByteString) extends CasperMessage {
  def toProto: BlockRequestProto = BlockRequestProto(hash)
}

object BlockRequest {
  def from(hbr: BlockRequestProto): BlockRequest = BlockRequest(hbr.hash)
}

final case object ForkChoiceTipRequest extends CasperMessage {
  def toProto: ForkChoiceTipRequestProto = ForkChoiceTipRequestProto()
}

final case class ApprovedBlockCandidate(block: BlockMessage, requiredSigs: Int)
    extends CasperMessage {
  def toProto: ApprovedBlockCandidateProto =
    ApprovedBlockCandidateProto()
      .withBlock(block.toProto)
      .withRequiredSigs(requiredSigs)
}

object ApprovedBlockCandidate {
  def from(abc: ApprovedBlockCandidateProto): Either[String, ApprovedBlockCandidate] =
    for {
      blockProto <- abc.block.toRight("Block not available")
      block      <- BlockMessage.from(blockProto)
    } yield ApprovedBlockCandidate(block, abc.requiredSigs)
}

final case class UnapprovedBlock(candidate: ApprovedBlockCandidate, timestamp: Long, duration: Long)
    extends CasperMessage {
  def toProto: UnapprovedBlockProto =
    UnapprovedBlockProto()
      .withCandidate(candidate.toProto)
      .withTimestamp(timestamp)
      .withDuration(duration)
}

object UnapprovedBlock {
  def from(ub: UnapprovedBlockProto): Either[String, UnapprovedBlock] =
    for {
      candidateProto <- ub.candidate.toRight("Candidate not available")
      candidate      <- ApprovedBlockCandidate.from(candidateProto)
    } yield UnapprovedBlock(candidate, ub.timestamp, ub.duration)
}

final case class BlockApproval(candidate: ApprovedBlockCandidate, sig: Signature)
    extends CasperMessage {
  def toProto: BlockApprovalProto =
    BlockApprovalProto()
      .withCandidate(candidate.toProto)
      .withSig(sig)
}

object BlockApproval {
  def from(ba: BlockApprovalProto): Either[String, BlockApproval] =
    for {
      candidateProto <- ba.candidate.toRight("Candidate not available")
      candidate      <- ApprovedBlockCandidate.from(candidateProto)
      sig            <- ba.sig.toRight("Sig not available")
    } yield BlockApproval(candidate, sig)
}

final case class ApprovedBlock(candidate: ApprovedBlockCandidate, sigs: List[Signature])
    extends CasperMessage {
  def toProto: ApprovedBlockProto =
    ApprovedBlockProto()
      .withCandidate(candidate.toProto)
      .withSigs(sigs)
}

object ApprovedBlock {
  def from(ba: ApprovedBlockProto): Either[String, ApprovedBlock] =
    for {
      candidateProto <- ba.candidate.toRight("Candidate not available")
      candidate      <- ApprovedBlockCandidate.from(candidateProto)
    } yield ApprovedBlock(candidate, ba.sigs.toList)
}

final case class NoApprovedBlockAvailable(identifier: String, nodeIdentifer: String)
    extends CasperMessage {
  def toProto: NoApprovedBlockAvailableProto =
    NoApprovedBlockAvailableProto()
      .withIdentifier(identifier)
      .withNodeIdentifer(nodeIdentifer)
}

object NoApprovedBlockAvailable {
  def from(naba: NoApprovedBlockAvailableProto): NoApprovedBlockAvailable =
    NoApprovedBlockAvailable(naba.identifier, naba.nodeIdentifer)
}

final case class ApprovedBlockRequest(identifier: String) extends CasperMessage {
  def toProto: ApprovedBlockRequestProto = ApprovedBlockRequestProto(identifier)
}

object ApprovedBlockRequest {
  def from(abr: ApprovedBlockRequestProto): ApprovedBlockRequest =
    ApprovedBlockRequest(abr.identifier)
}

final case class BlockMessage(
    blockHash: ByteString,
    header: Header,
    body: Body,
    justifications: List[Justification],
    sender: ByteString,
    seqNum: Int,
    sig: ByteString,
    sigAlgorithm: String,
    shardId: String,
    extraBytes: ByteString = ByteString.EMPTY
) extends CasperMessage {
  def toProto: BlockMessageProto = BlockMessage.toProto(this)
}

object BlockMessage {

  def from(bm: BlockMessageProto): Either[String, BlockMessage] =
    for {
      header <- bm.header.toRight("Header not available").map(Header.from)
      body   <- bm.body.toRight("Body not available") >>= (Body.from)
    } yield BlockMessage(
      bm.blockHash,
      header,
      body,
      bm.justifications.toList.map(Justification.from),
      bm.sender,
      bm.seqNum,
      bm.sig,
      bm.sigAlgorithm,
      bm.shardId,
      bm.extraBytes
    )

  def toProto(bm: BlockMessage): BlockMessageProto =
    BlockMessageProto()
      .withBlockHash(bm.blockHash)
      .withHeader(Header.toProto(bm.header))
      .withBody(Body.toProto(bm.body))
      .withJustifications(bm.justifications.map(Justification.toProto))
      .withSender(bm.sender)
      .withSeqNum(bm.seqNum)
      .withSig(bm.sig)
      .withSigAlgorithm(bm.sigAlgorithm)
      .withShardId(bm.shardId)
      .withExtraBytes(bm.extraBytes)
}

final case class Header(
    parentsHashList: List[ByteString],
    deploysHash: ByteString,
    timestamp: Long,
    version: Long,
    deployCount: Int,
    extraBytes: ByteString = ByteString.EMPTY
) {
  def toProto: HeaderProto = Header.toProto(this)
}

object Header {
  def from(h: HeaderProto): Header = Header(
    h.parentsHashList.toList,
    h.deploysHash,
    h.timestamp,
    h.version,
    h.deployCount,
    h.extraBytes
  )

  def toProto(h: Header): HeaderProto =
    HeaderProto()
      .withParentsHashList(h.parentsHashList)
      .withDeploysHash(h.deploysHash)
      .withTimestamp(h.timestamp)
      .withVersion(h.version)
      .withDeployCount(h.deployCount)
      .withExtraBytes(h.extraBytes)
}

final case class Body(
    state: RChainState,
    deploys: List[ProcessedDeploy],
    extraBytes: ByteString = ByteString.EMPTY
) {
  def toProto: BodyProto = Body.toProto(this)
}

object Body {
  def from(b: BodyProto): Either[String, Body] =
    for {
      state   <- b.state.toRight("RChainState not available").map(RChainState.from)
      deploys <- b.deploys.toList.traverse(ProcessedDeploy.from)
    } yield Body(state, deploys, b.extraBytes)

  def toProto(b: Body): BodyProto =
    BodyProto()
      .withState(RChainState.toProto(b.state))
      .withDeploys(b.deploys.map(ProcessedDeploy.toProto))
      .withExtraBytes(b.extraBytes)

}

final case class Justification(
    validator: ByteString,
    latestBlockHash: ByteString
) {
  def toProto: JustificationProto = Justification.toProto(this)
}

object Justification {
  def from(j: JustificationProto): Justification = Justification(
    j.validator,
    j.latestBlockHash
  )

  def toProto(j: Justification): JustificationProto =
    JustificationProto(j.validator, j.latestBlockHash)
}

final case class RChainState(
    preStateHash: ByteString,
    postStateHash: ByteString,
    bonds: List[Bond],
    blockNumber: Long
) {
  def toProto: RChainStateProto = RChainState.toProto(this)
}

object RChainState {
  def from(rchs: RChainStateProto): RChainState =
    RChainState(
      rchs.preStateHash,
      rchs.postStateHash,
      rchs.bonds.toList.map(Bond.from),
      rchs.blockNumber
    )

  def toProto(rchsp: RChainState): RChainStateProto =
    RChainStateProto()
      .withPreStateHash(rchsp.preStateHash)
      .withPostStateHash(rchsp.postStateHash)
      .withBonds(rchsp.bonds.map(Bond.toProto))
      .withBlockNumber(rchsp.blockNumber)
}

final case class ProcessedDeploy(
    deploy: DeployData,
    cost: PCost,
    deployLog: List[Event],
    paymentLog: List[Event],
    errored: Boolean
) {
  def toProto: ProcessedDeployProto = ProcessedDeploy.toProto(this)
}

object ProcessedDeploy {
  def from(pd: ProcessedDeployProto): Either[String, ProcessedDeploy] =
    for {
      ddn        <- pd.deploy.toRight("DeployData not available").map(DeployData.from)
      cost       <- pd.cost.toRight("Cost not available")
      deployLog  <- pd.deployLog.toList.traverse(Event.from)
      paymentLog <- pd.paymentLog.toList.traverse(Event.from)
    } yield ProcessedDeploy(
      ddn,
      cost,
      deployLog,
      paymentLog,
      pd.errored
    )

  def toProto(pd: ProcessedDeploy): ProcessedDeployProto =
    ProcessedDeployProto()
      .withDeploy(DeployData.toProto(pd.deploy))
      .withCost(pd.cost)
      .withDeployLog(pd.deployLog.map(Event.toProto))
      .withPaymentLog(pd.paymentLog.map(Event.toProto))
      .withErrored(pd.errored)
}

final case class DeployData(
    deployer: ByteString,
    term: String,
    timestamp: Long,
    sig: ByteString,
    sigAlgorithm: String,
    phloPrice: Long,
    phloLimit: Long,
    validAfterBlockNumber: Long
) {

  def toProto: DeployDataProto = DeployData.toProto(this)
}

object DeployData {
  def from(dd: DeployDataProto): DeployData = DeployData(
    dd.deployer,
    dd.term,
    dd.timestamp,
    dd.sig,
    dd.sigAlgorithm,
    dd.phloPrice,
    dd.phloLimit,
    dd.validAfterBlockNumber
  )

  def toProto(dd: DeployData): DeployDataProto =
    DeployDataProto()
      .withDeployer(dd.deployer)
      .withTerm(dd.term)
      .withTimestamp(dd.timestamp)
      .withSig(dd.sig)
      .withSigAlgorithm(dd.sigAlgorithm)
      .withPhloPrice(dd.phloPrice)
      .withPhloLimit(dd.phloLimit)
      .withValidAfterBlockNumber(dd.validAfterBlockNumber)

}

final case class Peek(channelIndex: Int) {
  def toProto: PeekProto = PeekProto(channelIndex)
}

object Peek {
  def from(p: PeekProto): Peek = Peek(p.channelIndex)
}

sealed trait Event {
  def toProto: EventProto = Event.toProto(this)
}
final case class ProduceEvent(
    channelsHash: ByteString,
    hash: ByteString,
    persistent: Boolean,
    sequenceNumber: Int
) extends Event
final case class ConsumeEvent(
    channelsHashes: List[ByteString],
    hash: ByteString,
    persistent: Boolean,
    sequenceNumber: Int
) extends Event
final case class CommEvent(
    consume: ConsumeEvent,
    produces: List[ProduceEvent],
    peeks: List[Peek]
) extends Event

object Event {
  def from(e: EventProto): Either[String, Event] =
    e.eventInstance match {
      case EventProto.EventInstance.Produce(pe) => fromProduceEvent(pe).asRight[String]
      case EventProto.EventInstance.Consume(ce) => fromConsumeEvent(ce).asRight[String]
      case EventProto.EventInstance.Comm(CommEventProto(Some(ce), pes, pks)) =>
        CommEvent(fromConsumeEvent(ce), pes.toList.map(fromProduceEvent), pks.toList.map(Peek.from))
          .asRight[String]
      case EventProto.EventInstance.Comm(CommEventProto(None, _, _)) =>
        "CommEvent does not have a consume event in it".asLeft[Event]
      case EventProto.EventInstance.Empty => "Received malformed Event: Empty".asLeft[Event]
    }

  def toProto(e: Event): EventProto = e match {
    case pe: ProduceEvent => EventProto(EventProto.EventInstance.Produce(toProduceEventProto(pe)))
    case ce: ConsumeEvent => EventProto(EventProto.EventInstance.Consume(toConsumeEventProto(ce)))
    case cme: CommEvent =>
      EventProto(
        EventProto.EventInstance.Comm(
          CommEventProto(
            Some(toConsumeEventProto(cme.consume)),
            cme.produces.map(toProduceEventProto),
            cme.peeks.map(_.toProto)
          )
        )
      )
  }
  private def fromConsumeEvent(ce: ConsumeEventProto): ConsumeEvent =
    ConsumeEvent(ce.channelsHashes.toList, ce.hash, ce.persistent, ce.sequenceNumber)
  private def fromProduceEvent(pe: ProduceEventProto): ProduceEvent =
    ProduceEvent(pe.channelsHash, pe.hash, pe.persistent, pe.sequenceNumber)

  private def toProduceEventProto(pe: ProduceEvent): ProduceEventProto =
    ProduceEventProto(pe.channelsHash, pe.hash, pe.persistent, pe.sequenceNumber)

  private def toConsumeEventProto(ce: ConsumeEvent): ConsumeEventProto =
    ConsumeEventProto(ce.channelsHashes, ce.hash, ce.persistent, ce.sequenceNumber)
}

final case class Bond(
    validator: ByteString,
    stake: Long
)

object Bond {
  def from(b: BondProto): Bond    = Bond(b.validator, b.stake)
  def toProto(b: Bond): BondProto = BondProto(b.validator, b.stake)
}
