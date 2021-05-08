package coop.rchain.casper.protocol

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.crypto.signatures.{SignaturesAlg, Signed}
import coop.rchain.models.PCost
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.crypto.{Blake2b256Hash, PublicKey}
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector

sealed trait CasperMessage {
  def toProto: CasperMessageProto
}

object CasperMessage {
  def from(cm: CasperMessageProto): Either[String, CasperMessage] = cm match {
    case hash: BlockHashMessageProto      => Right(BlockHashMessage.from(hash))
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
    // Last finalized state messages
    case p: StoreItemsMessageRequestProto => Right(StoreItemsMessageRequest.from(p))
    case p: StoreItemsMessageProto        => Right(StoreItemsMessage.from(p))
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

case object ForkChoiceTipRequest extends CasperMessage {
  val toProto: ForkChoiceTipRequestProto = ForkChoiceTipRequestProto()
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

final case class UnapprovedBlock(
    candidate: ApprovedBlockCandidate,
    timestamp: Long,
    duration: Long
) extends CasperMessage {
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

final case class ApprovedBlockRequest(identifier: String, trimState: Boolean = false)
    extends CasperMessage {
  def toProto: ApprovedBlockRequestProto = ApprovedBlockRequestProto(identifier, trimState)
}

object ApprovedBlockRequest {
  def from(abr: ApprovedBlockRequestProto): ApprovedBlockRequest =
    ApprovedBlockRequest(abr.identifier, abr.trimState)
}

final case class BlockHashMessage(blockHash: BlockHash, blockCreator: ByteString)
    extends CasperMessage {
  def toProto: BlockHashMessageProto = BlockHashMessageProto(blockHash, blockCreator)
}

object BlockHashMessage {
  def from(msg: BlockHashMessageProto) = BlockHashMessage(msg.hash, msg.blockCreator)
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

  override def toString: String = PrettyPrinter.buildString(this)
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
    timestamp: Long,
    version: Long,
    extraBytes: ByteString = ByteString.EMPTY
) {
  def toProto: HeaderProto = Header.toProto(this)
}

object Header {
  def from(h: HeaderProto): Header = Header(
    h.parentsHashList.toList,
    h.timestamp,
    h.version,
    h.extraBytes
  )

  def toProto(h: Header): HeaderProto =
    HeaderProto()
      .withParentsHashList(h.parentsHashList)
      .withTimestamp(h.timestamp)
      .withVersion(h.version)
      .withExtraBytes(h.extraBytes)
}

final case class RejectedDeploy(
    sig: ByteString
)

object RejectedDeploy {
  def from(r: RejectedDeployProto): Either[String, RejectedDeploy] =
    Right(RejectedDeploy(r.sig))

  def toProto(r: RejectedDeploy): RejectedDeployProto =
    RejectedDeployProto().withSig(r.sig)
}

final case class Body(
    state: RChainState,
    deploys: List[ProcessedDeploy],
    rejectedDeploys: List[RejectedDeploy],
    systemDeploys: List[ProcessedSystemDeploy],
    extraBytes: ByteString = ByteString.EMPTY
) {
  def toProto: BodyProto = Body.toProto(this)
}

object Body {
  def from(b: BodyProto): Either[String, Body] =
    for {
      state           <- b.state.toRight("RChainState not available").map(RChainState.from)
      deploys         <- b.deploys.toList.traverse(ProcessedDeploy.from)
      systemDeploys   <- b.systemDeploys.toList.traverse(ProcessedSystemDeploy.from)
      rejectedDeploys <- b.rejectedDeploys.toList.traverse(RejectedDeploy.from)
    } yield Body(state, deploys, rejectedDeploys, systemDeploys, b.extraBytes)

  def toProto(b: Body): BodyProto =
    BodyProto()
      .withState(RChainState.toProto(b.state))
      .withDeploys(b.deploys.map(ProcessedDeploy.toProto))
      .withRejectedDeploys(b.rejectedDeploys.map(RejectedDeploy.toProto))
      .withSystemDeploys(b.systemDeploys.map(ProcessedSystemDeploy.toProto))
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
    deploy: Signed[DeployData],
    cost: PCost,
    deployLog: List[Event],
    isFailed: Boolean,
    systemDeployError: Option[String] = None
) {
  def refundAmount                  = (deploy.data.phloLimit - cost.cost).max(0) * deploy.data.phloPrice
  def toProto: ProcessedDeployProto = ProcessedDeploy.toProto(this)
  def toDeployInfo: DeployInfo =
    DeployInfo(
      deployer = PrettyPrinter.buildStringNoLimit(this.deploy.pk.bytes.toArray),
      term = this.deploy.data.term,
      timestamp = this.deploy.data.timestamp,
      sig = PrettyPrinter.buildStringNoLimit(this.deploy.sig),
      sigAlgorithm = this.deploy.sigAlgorithm.name,
      phloPrice = this.deploy.data.phloPrice,
      phloLimit = this.deploy.data.phloLimit,
      validAfterBlockNumber = this.deploy.data.validAfterBlockNumber,
      cost = this.cost.cost,
      errored = this.isFailed,
      systemDeployError = this.systemDeployError.getOrElse("")
    )
}

object ProcessedDeploy {
  def empty(deploy: Signed[DeployData]) =
    ProcessedDeploy(deploy, PCost(), List.empty, isFailed = false)
  def from(pd: ProcessedDeployProto): Either[String, ProcessedDeploy] =
    for {
      ddProto   <- pd.deploy.toRight("DeployData not available")
      dd        <- DeployData.from(ddProto)
      cost      <- pd.cost.toRight("Cost not available")
      deployLog <- pd.deployLog.toList.traverse(Event.from)
    } yield ProcessedDeploy(
      dd,
      cost,
      deployLog,
      pd.errored,
      if (pd.systemDeployError.isEmpty()) None else Some(pd.systemDeployError)
    )

  def toProto(pd: ProcessedDeploy): ProcessedDeployProto = {
    val proto = ProcessedDeployProto()
      .withDeploy(DeployData.toProto(pd.deploy))
      .withCost(pd.cost)
      .withDeployLog(pd.deployLog.map(Event.toProto))
      .withErrored(pd.isFailed)

    pd.systemDeployError.fold(proto)(errorMsg => proto.withSystemDeployError(errorMsg))
  }
}

sealed trait SystemDeployData

final case class SlashSystemDeployData(invalidBlockHash: BlockHash, issuerPublicKey: PublicKey)
    extends SystemDeployData
case object CloseBlockSystemDeployData extends SystemDeployData
case object Empty                      extends SystemDeployData

object SystemDeployData {
  val empty: SystemDeployData = Empty

  def from(invalidBlockHash: BlockHash, issuerPublicKey: PublicKey): SystemDeployData =
    SlashSystemDeployData(invalidBlockHash, issuerPublicKey)

  def from(): SystemDeployData =
    CloseBlockSystemDeployData

  def fromProto(proto: SystemDeployDataProto): SystemDeployData =
    proto.systemDeploy match {
      case SystemDeployDataProto.SystemDeploy.SlashSystemDeploy(sd) =>
        SlashSystemDeployData(sd.invalidBlockHash, PublicKey(sd.issuerPublicKey))
      case SystemDeployDataProto.SystemDeploy.CloseBlockSystemDeploy(_) =>
        CloseBlockSystemDeployData
      case _ => Empty
    }

  def toProto(sdd: SystemDeployData): SystemDeployDataProto =
    sdd match {
      case SlashSystemDeployData(invalidBlockHash, issuerPublicKey) =>
        SystemDeployDataProto().withSlashSystemDeploy(
          SlashSystemDeployDataProto(invalidBlockHash, ByteString.copyFrom(issuerPublicKey.bytes))
        )
      case CloseBlockSystemDeployData =>
        SystemDeployDataProto().withCloseBlockSystemDeploy(
          CloseBlockSystemDeployDataProto()
        )
      case Empty => SystemDeployDataProto()
    }
}

sealed trait ProcessedSystemDeploy {
  val systemDeploy: SystemDeployData
  def eventList: List[Event]
  def failed: Boolean
  def fold[A](ifSucceeded: List[Event] => A, ifFailed: (List[Event], String) => A): A
}

object ProcessedSystemDeploy {

  final case class Succeeded(
      eventList: List[Event],
      systemDeploy: SystemDeployData
  ) extends ProcessedSystemDeploy {
    val failed = false

    override def fold[A](ifSucceeded: List[Event] => A, ifFailed: (List[Event], String) => A): A =
      ifSucceeded(eventList)
  }

  final case class Failed(
      eventList: List[Event],
      errorMsg: String
  ) extends ProcessedSystemDeploy {
    val failed                                  = true
    override val systemDeploy: SystemDeployData = SystemDeployData.empty

    override def fold[A](ifSucceeded: List[Event] => A, ifFailed: (List[Event], String) => A): A =
      ifFailed(eventList, errorMsg)
  }

  def from(psd: ProcessedSystemDeployProto): Either[String, ProcessedSystemDeploy] =
    psd.deployLog.toList
      .traverse(Event.from)
      .map(
        deployLog =>
          if (psd.errorMsg.isEmpty) {
            Succeeded(
              deployLog,
              psd.systemDeploy.map(SystemDeployData.fromProto).getOrElse(SystemDeployData.empty)
            )
          } else Failed(deployLog, psd.errorMsg)
      )

  def toProto(psd: ProcessedSystemDeploy): ProcessedSystemDeployProto = {
    val deployLog = psd.eventList.map(Event.toProto)
    psd match {
      case Succeeded(_, systemDeploy) =>
        ProcessedSystemDeployProto()
          .withDeployLog(deployLog)
          .withSystemDeploy(SystemDeployData.toProto(systemDeploy))
      case Failed(_, errorMsg) =>
        ProcessedSystemDeployProto().withDeployLog(deployLog).withErrorMsg(errorMsg)
    }
  }
}

final case class DeployData(
    term: String,
    timestamp: Long,
    phloPrice: Long,
    phloLimit: Long,
    validAfterBlockNumber: Long
) {
  def totalPhloCharge = phloLimit * phloPrice
}

object DeployData {
  implicit val serialize = new Serialize[DeployData] {
    override def encode(a: DeployData): ByteVector =
      ByteVector(toProto(a).toByteArray)

    override def decode(bytes: ByteVector): Either[Throwable, DeployData] =
      Right(fromProto(DeployDataProto.parseFrom(bytes.toArray)))
  }

  private def fromProto(proto: DeployDataProto): DeployData =
    DeployData(
      proto.term,
      proto.timestamp,
      proto.phloPrice,
      proto.phloLimit,
      proto.validAfterBlockNumber
    )

  def from(dd: DeployDataProto): Either[String, Signed[DeployData]] =
    for {
      algorithm <- SignaturesAlg(dd.sigAlgorithm).toRight("Invalid signing algorithm")
      signed <- Signed
                 .fromSignedData(fromProto(dd), PublicKey(dd.deployer), dd.sig, algorithm)
                 .toRight("Invalid signature")
    } yield (signed)

  private def toProto(dd: DeployData): DeployDataProto =
    DeployDataProto()
      .withTerm(dd.term)
      .withTimestamp(dd.timestamp)
      .withPhloPrice(dd.phloPrice)
      .withPhloLimit(dd.phloLimit)
      .withValidAfterBlockNumber(dd.validAfterBlockNumber)

  def toProto(dd: Signed[DeployData]): DeployDataProto =
    toProto(dd.data)
      .withDeployer(ByteString.copyFrom(dd.pk.bytes))
      .withSig(dd.sig)
      .withSigAlgorithm(dd.sigAlgorithm.name)
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
    timesRepeated: Int
) extends Event
final case class ConsumeEvent(
    channelsHashes: List[ByteString],
    hash: ByteString,
    persistent: Boolean
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
    ConsumeEvent(ce.channelsHashes.toList, ce.hash, ce.persistent)
  private def fromProduceEvent(pe: ProduceEventProto): ProduceEvent =
    ProduceEvent(pe.channelsHash, pe.hash, pe.persistent, pe.timesRepeated)

  private def toProduceEventProto(pe: ProduceEvent): ProduceEventProto =
    ProduceEventProto(pe.channelsHash, pe.hash, pe.persistent, pe.timesRepeated)

  private def toConsumeEventProto(ce: ConsumeEvent): ConsumeEventProto =
    ConsumeEventProto(ce.channelsHashes, ce.hash, ce.persistent)
}

final case class Bond(
    validator: ByteString,
    stake: Long
)

object Bond {
  def from(b: BondProto): Bond    = Bond(b.validator, b.stake)
  def toProto(b: Bond): BondProto = BondProto(b.validator, b.stake)
}

// Last finalized state

object StoreNodeKey {
  // Encoding of non existent index for store node (Skip or Leaf node)
  val noneIndex = 0x100

  def from(s: StoreNodeKeyProto): (Blake2b256Hash, Option[Byte]) = {
    // Key hash
    val hashBytes = Blake2b256Hash.fromByteString(s.hash)
    // Relative branch index / max 8-bit
    val idx = if (s.index == noneIndex) none[Byte] else s.index.toByte.some
    (hashBytes, idx)
  }

  def toProto(s: (Blake2b256Hash, Option[Byte])): StoreNodeKeyProto =
    StoreNodeKeyProto(s._1.toByteString, s._2.map(_.toInt).getOrElse(noneIndex))
}

final case class StoreItemsMessageRequest(
    startPath: Seq[(Blake2b256Hash, Option[Byte])],
    skip: Int,
    take: Int
) extends CasperMessage {
  override def toProto: StoreItemsMessageRequestProto = StoreItemsMessageRequest.toProto(this)
}

object StoreItemsMessageRequest {
  def from(x: StoreItemsMessageRequestProto): StoreItemsMessageRequest =
    StoreItemsMessageRequest(x.startPath.map(StoreNodeKey.from), x.skip, x.take)

  def toProto(x: StoreItemsMessageRequest): StoreItemsMessageRequestProto =
    StoreItemsMessageRequestProto(x.startPath.map(StoreNodeKey.toProto).toList, x.skip, x.take)
}

final case class StoreItemsMessage(
    startPath: Seq[(Blake2b256Hash, Option[Byte])],
    lastPath: Seq[(Blake2b256Hash, Option[Byte])],
    historyItems: Seq[(Blake2b256Hash, ByteString)],
    dataItems: Seq[(Blake2b256Hash, ByteString)]
) extends CasperMessage {
  override def toProto: StoreItemsMessageProto = StoreItemsMessage.toProto(this)

  def pretty: String = {
//    val start       = startPath.map(RSpaceExporter.pathPretty).mkString(" ")
//    val last        = lastPath.map(RSpaceExporter.pathPretty).mkString(" ")
    val historySize = historyItems.size
    val dataSize    = dataItems.size
//    s"StoreItems(history: $historySize, data: $dataSize, start: [$start], last: [$last])"
    s"StoreItems(history: $historySize, data: $dataSize)"
  }
}

object StoreItemsMessage {
  def from(x: StoreItemsMessageProto): StoreItemsMessage =
    StoreItemsMessage(
      x.startPath.map(StoreNodeKey.from),
      x.lastPath.map(StoreNodeKey.from),
      x.historyItems.map(y => (Blake2b256Hash.fromByteString(y.key), y.value)),
      x.dataItems.map(y => (Blake2b256Hash.fromByteString(y.key), y.value))
    )

  def toProto(x: StoreItemsMessage): StoreItemsMessageProto =
    StoreItemsMessageProto(
      x.startPath.map(StoreNodeKey.toProto).toList,
      x.lastPath.map(StoreNodeKey.toProto).toList,
      x.historyItems.map(y => StoreItemProto(y._1.toByteString, y._2)).toList,
      x.dataItems.map(y => StoreItemProto(y._1.toByteString, y._2)).toList
    )
}
