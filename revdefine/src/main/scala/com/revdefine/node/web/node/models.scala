package com.revdefine.node.web.node

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{
  BlockMessage,
  CloseBlockSystemDeployData,
  Empty,
  SlashSystemDeployData,
  CommEvent => OriCommEvent,
  ConsumeEvent => OriConsumeEvent,
  Event => OriEvent,
  ProcessedDeploy => OriProcessedDeploy,
  ProcessedSystemDeploy => OriProcessedSystemDeploy,
  ProduceEvent => OriProduceEvent
}
import coop.rchain.crypto.codec.Base16

object models {
  implicit class ByteStringOps(bytes: ByteString) {
    def toBase16: String = Base16.encode(bytes.toByteArray)
  }
  final case class Peek(channelIndex: Int)
  sealed trait Event
  final case class ProduceEvent(
      channelsHash: String,
      hash: String,
      persistent: Boolean,
      timesRepeated: Int
  ) extends Event
  final case class ConsumeEvent(
      channelsHashes: List[String],
      hash: String,
      persistent: Boolean
  ) extends Event
  final case class CommEvent(
      consume: ConsumeEvent,
      produces: List[ProduceEvent],
      peeks: List[Peek]
  ) extends Event

  final case class Justification(
      validator: String,
      latestBlockHash: String
  )
  final case class Bond(
      validator: String,
      stake: Long
  )

  final case class BlockHeader(
      blockHash: String,
      justifications: List[Justification],
      parents: List[String],
      timestamp: Long,
      version: Long,
      sender: String,
      seqNum: Int,
      sig: String,
      sigAlgorithm: String,
      shardId: String,
      preStateHash: String,
      postStateHash: String,
      blockNumber: Long,
      bonds: List[Bond],
      extraBytes: String,
      headerExtraBytes: String,
      // need calculations,
      // FIXME get rid off it?
      blockSize: Int,
      deployCount: Int
  )

  final case class DeployInfo(
      deployer: String,
      sig: String,
      sigAlgorithm: String,
      term: String,
      timestamp: Long,
      phloPrice: Long,
      phloLimit: Long,
      validAfterBlockNumber: Long
  )

  final case class ProcessedDeploy(
      deploy: DeployInfo,
      cost: Long,
      eventsLog: List[Event],
      isFailed: Boolean,
      systemDeployError: Option[String] = None
  )
  final case class RejectedDeploy(
      sig: String
  )

  final case class ProcessedSystemDeploy(
      deployType: String,
      eventsLog: List[Event],
      failed: Boolean
  )
  final case class BlockBody(
      deploys: List[ProcessedDeploy],
      rejectedDeploys: List[RejectedDeploy],
      systemDeploys: List[ProcessedSystemDeploy],
      extraBytes: String
  )
  final case class Block(
      header: BlockHeader,
      body: BlockBody
  )

  def convertBlockHeader(block: BlockMessage): BlockHeader = BlockHeader(
    blockHash = block.blockHash.toBase16,
    justifications = block.justifications
      .map(j => Justification(j.validator.toBase16, j.latestBlockHash.toBase16)),
    parents = block.header.parentsHashList.map(_.toBase16),
    timestamp = block.header.timestamp,
    version = block.header.version,
    sender = block.sender.toBase16,
    seqNum = block.seqNum,
    sig = block.sig.toBase16,
    sigAlgorithm = block.sigAlgorithm,
    shardId = block.shardId,
    preStateHash = block.body.state.preStateHash.toBase16,
    postStateHash = block.body.state.postStateHash.toBase16,
    blockNumber = block.body.state.blockNumber,
    bonds = block.body.state.bonds.map(b => Bond(b.validator.toBase16, b.stake)),
    extraBytes = block.extraBytes.toBase16,
    headerExtraBytes = block.header.extraBytes.toBase16,
    blockSize = block.toProto.serializedSize,
    deployCount = block.body.deploys.length
  )

  def convertBlockBody(blockMessage: BlockMessage): BlockBody = BlockBody(
    deploys = blockMessage.body.deploys.map(convertProcessDeploy),
    rejectedDeploys = blockMessage.body.rejectedDeploys.map(r => RejectedDeploy(r.sig.toBase16)),
    systemDeploys = blockMessage.body.systemDeploys.map(convertProcessSystemDeploy),
    extraBytes = blockMessage.body.extraBytes.toBase16
  )

  def convertProcessSystemDeploy(sd: OriProcessedSystemDeploy): ProcessedSystemDeploy =
    ProcessedSystemDeploy(
      deployType = sd.systemDeploy match {
        case SlashSystemDeployData(invalidBlockHash, pk) =>
          s"Slashing ${Base16.encode(pk.bytes)} because of ${invalidBlockHash.toBase16}"
        case CloseBlockSystemDeployData => "closeBlock"
        case Empty                      => "empty"
      },
      eventsLog = sd.eventList.map(convertEvent),
      failed = sd.failed
    )

  def convertProcessDeploy(pd: OriProcessedDeploy): ProcessedDeploy = ProcessedDeploy(
    deploy = DeployInfo(
      deployer = Base16.encode(pd.deploy.pk.bytes),
      sig = pd.deploy.sig.toBase16,
      sigAlgorithm = pd.deploy.sigAlgorithm.name,
      term = pd.deploy.data.term,
      timestamp = pd.deploy.data.timestamp,
      phloPrice = pd.deploy.data.phloPrice,
      phloLimit = pd.deploy.data.phloLimit,
      validAfterBlockNumber = pd.deploy.data.validAfterBlockNumber
    ),
    cost = pd.cost.cost,
    eventsLog = pd.deployLog.map(convertEvent),
    isFailed = pd.isFailed,
    systemDeployError = pd.systemDeployError
  )

  def convertEvent(e: OriEvent): Event = e match {
    case OriProduceEvent(c, h, p, t) => ProduceEvent(c.toBase16, h.toBase16, p, t)
    case OriConsumeEvent(chs, h, p)  => ConsumeEvent(chs.map(_.toBase16), h.toBase16, p)
    case OriCommEvent(c, p, peek) =>
      CommEvent(
        convertEvent(c).asInstanceOf[ConsumeEvent],
        p.map(convertEvent(_).asInstanceOf[ProduceEvent]),
        peek.map(p => Peek(p.channelIndex))
      )
  }

  def convertBlock(blockMessage: BlockMessage) = Block(
    header = convertBlockHeader(blockMessage),
    body = convertBlockBody(blockMessage)
  )
}
