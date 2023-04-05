package coop.rchain.casper.helper

import cats.Applicative
import cats.effect.{Concurrent, IO, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper.merging.ParentsMergedState
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.InterpreterUtil.computeDeploysCheckpoint
import coop.rchain.casper.rholang.types.SystemDeploy
import coop.rchain.casper.rholang.{BlockRandomSeed, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource, Time}

object BlockGenerator {
  private[this] val GenerateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "generate-block")

  implicit val timeEff              = new LogicalTime[IO]
  implicit val logSource: LogSource = LogSource(this.getClass)

  // Dummy empty Casper snapshot
  val dummyParentsPreState = ParentsMergedState(
    justifications = Set.empty,
    maxBlockNum = 0L,
    maxSeqNums = Map.empty,
    fringe = Set(),
    fringeState = RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
    fringeBondsMap = Map.empty,
    fringeRejectedDeploys = Set(),
    // Pre-state is the same as fringe state
    preStateHash = RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
    rejectedDeploys = Set()
  )

  def step[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage
  ): F[Unit] =
    for {
      computeBlockCheckpointResult              <- computeBlockCheckpoint(block, dummyParentsPreState)
      (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
      result                                    <- injectPostStateHash[F](block, postB1StateHash, postB1ProcessedDeploys)
    } yield result

  private def computeBlockCheckpoint[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage,
      preState: ParentsMergedState
  ): F[(StateHash, Seq[ProcessedDeploy])] = Span[F].trace(GenerateBlockMetricsSource) {
    val deploys      = block.state.deploys.map(_.deploy)
    val preStateHash = preState.preStateHash.toByteString
    val rand         = BlockRandomSeed.randomGenerator(block)
    for {
      result <- computeDeploysCheckpoint[F](
                 deploys,
                 List.empty[SystemDeploy],
                 rand,
                 BlockData.fromBlock(block),
                 preStateHash
               )
      (postStateHash, processedDeploys, _) = result
    } yield (postStateHash, processedDeploys)
  }

  private def injectPostStateHash[F[_]: Sync: BlockStore: BlockDagStorage](
      b: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): F[Unit] = {
    val updatedBlockBody =
      b.state.copy(deploys = processedDeploys.toList)
    val updatedBlock = b.copy(postStateHash = postGenStateHash, state = updatedBlockBody)
    BlockStore[F].put(b.blockHash, updatedBlock) >>
      BlockDagStorage[F].insertLegacy(updatedBlock, invalid = false).void
  }
}

trait BlockGenerator {
  def buildBlock[F[_]: Applicative](
      creator: Validator = ByteString.EMPTY,
      bonds: Map[Validator, Long] = Map.empty,
      justifications: Seq[BlockHash] = Seq.empty[BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      postStateHash: ByteString = ByteString.EMPTY,
      shardId: String = "root",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Long = 0L
  ): F[BlockMessage] =
    getRandomBlock(
      setValidator = creator.some,
      setBonds = bonds.some,
      setJustifications = justifications.some,
      setDeploys = deploys.some,
      setPostStateHash = postStateHash.some,
      setShardId = shardId.some,
      setPreStateHash = preStateHash.some,
      setSeqNumber = seqNum.some
    ).pure[F]

  def createGenesis[F[_]: Sync: BlockStore: BlockDagStorage](
      creator: Validator = BlockUtil.generateValidator("Validator genesis"),
      bonds: Map[Validator, Long] = Map.empty,
      justifications: Seq[BlockHash] = Seq.empty[BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "root",
      preStateHash: ByteString = RuntimeManager.emptyStateHashFixed,
      seqNum: Long = 0
  ): F[BlockMessage] =
    for {
      genesis <- buildBlock[F](
                  creator,
                  bonds,
                  justifications,
                  deploys,
                  tsHash,
                  shardId,
                  preStateHash,
                  seqNum
                )
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insertGenesis(genesis)
    } yield genesis

  def createBlock[F[_]: Sync: BlockStore: BlockDagStorage](
      creator: Validator = BlockUtil.generateValidator("Validator"),
      bonds: Map[Validator, Long] = Map.empty,
      justifications: Seq[BlockHash] = Seq.empty[BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      postStateHash: ByteString = ByteString.EMPTY,
      shardId: String = "root",
      preStateHash: ByteString = RuntimeManager.emptyStateHashFixed,
      seqNum: Long = 0,
      invalid: Boolean = false
  ): F[BlockMessage] =
    for {
      block <- buildBlock[F](
                creator,
                bonds,
                justifications,
                deploys,
                postStateHash,
                shardId,
                preStateHash,
                seqNum
              )
      dag <- BlockDagStorage[F].getRepresentation
      // TODO: format of if..then expressions in for comprehensions is awful
      nextCreatorSeqNum = if (block.seqNum == 0L) getLatestSeqNum(block.sender, dag) + 1L
      else block.seqNum
      nextId <- justifications.toList
                 .traverse(BlockStore[F].getUnsafe(_).map(_.blockNumber))
                 .map(_.maximumOption.getOrElse(0L) + 1L)
      modifiedBlock = block
        .copy(
          blockNumber = nextId,
          seqNum = nextCreatorSeqNum
        )
      _ <- BlockStore[F].put(block.blockHash, modifiedBlock)
      blockMeta = BlockMetadata
        .fromBlock(modifiedBlock)
        .copy(validated = true, validationFailed = invalid, fringeStateHash = preStateHash)
      _ <- BlockDagStorage[F].insert(blockMeta, modifiedBlock)
    } yield modifiedBlock

  def getLatestSeqNum(sender: Validator, dag: DagRepresentation): Long = {
    val sendersLatest = dag.dagMessageState.latestMsgs.filter(_.sender == sender)
    sendersLatest.map(_.senderSeq).toList.maximumOption.getOrElse(-1L)
  }

  def createValidatorBlock[F[_]: Sync: Time: BlockStore: BlockDagStorage](
      justifications: Seq[BlockMessage],
      validator: Validator,
      bonds: Map[Validator, Long],
      seqNum: Long = 0,
      invalid: Boolean = false,
      shardId: String
  ): F[BlockMessage] =
    for {
      deploy <- ConstructDeploy.basicProcessedDeploy[F](0, shardId)
      result <- createBlock[F](
                 creator = validator,
                 bonds = bonds,
                 deploys = Seq(deploy),
                 justifications = justifications.map(_.blockHash),
                 seqNum = seqNum,
                 invalid = invalid
               )
    } yield result
}
