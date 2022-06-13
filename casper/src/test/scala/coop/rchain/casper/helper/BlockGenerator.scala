package coop.rchain.casper.helper

import cats._
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.InterpreterUtil.{
  computeDeploysCheckpoint,
  computeParentsPostState
}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.rholang.types.SystemDeploy
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.{CasperMetricsSource, CasperShardConf, CasperSnapshot, OnChainCasperState}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task

// TODO squash this with block generator in blockimplicits
object BlockGenerator {
  implicit val timeEff = new LogicalTime[Task]
  private[this] val GenerateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "generate-block")

  implicit val logSource: LogSource = LogSource(this.getClass)
  def mkCasperSnapshot[F[_]](dag: DagRepresentation) =
    CasperSnapshot(
      dag,
      Seq(),
      ByteString.EMPTY,
      IndexedSeq.empty,
      Set.empty,
      Set.empty,
      0,
      Map.empty,
      OnChainCasperState(
        CasperShardConf(0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Map.empty,
        Seq.empty
      )
    )
  def step[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage,
      genesis: BlockMessage
  ): F[Unit] =
    for {
      dag                                       <- BlockDagStorage[F].getRepresentation
      computeBlockCheckpointResult              <- computeBlockCheckpoint(block, mkCasperSnapshot(dag))
      (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
      result                                    <- injectPostStateHash[F](block, postB1StateHash, postB1ProcessedDeploys)
    } yield result

  private def computeBlockCheckpoint[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      b: BlockMessage,
      s: CasperSnapshot
  ): F[(StateHash, Seq[ProcessedDeploy])] = Span[F].trace(GenerateBlockMetricsSource) {
    val deploys = b.state.deploys.map(_.deploy)
    for {
      computedParentsInfo <- computeParentsPostState(b.justifications, s)
      result <- computeDeploysCheckpoint[F](
                 deploys,
                 List.empty[SystemDeploy],
                 BlockData.fromBlock(b),
                 computedParentsInfo
               ).attempt
      Right((preStateHash, postStateHash, processedDeploys, rejectedDeploys, _)) = result
    } yield (postStateHash, processedDeploys)
  }

  private def injectPostStateHash[F[_]: Monad: BlockStore: BlockDagStorage](
      b: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): F[Unit] = {
    val updatedBlockBody =
      b.state.copy(deploys = processedDeploys.toList)
    val updatedBlock = b.copy(postStateHash = postGenStateHash, state = updatedBlockBody)
    BlockStore[F].put(b.blockHash, updatedBlock) >>
      BlockDagStorage[F].insert(updatedBlock, invalid = false).void
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

  def createGenesis[F[_]: Monad: BlockStore: BlockDagStorage](
      creator: Validator = BlockUtil.generateValidator("Validator genesis"),
      bonds: Map[Validator, Long] = Map.empty,
      justifications: Seq[BlockHash] = Seq.empty[BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "root",
      preStateHash: ByteString = ByteString.EMPTY,
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
      _ <- BlockDagStorage[F].insert(genesis, false, false)
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
    } yield genesis

  def createBlock[F[_]: Sync: BlockStore: BlockDagStorage](
      creator: Validator = BlockUtil.generateValidator("Validator"),
      bonds: Map[Validator, Long] = Map.empty,
      justifications: Seq[BlockHash] = Seq.empty[BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      postStateHash: ByteString = ByteString.EMPTY,
      shardId: String = "root",
      preStateHash: ByteString = ByteString.EMPTY,
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
      nextCreatorSeqNum <- if (block.seqNum == 0L)
                            dag.latestMessage(block.sender).map(_.fold(-1L)(_.seqNum) + 1L)
                          else block.seqNum.pure[F]
      nextId <- justifications.toList
                 .traverse(
                   BlockStore[F].getUnsafe(_).map(_.blockNumber)
                 )
                 .map(_.maximumOption.getOrElse(0L) + 1L)
      modifiedBlock = block
        .copy(
          blockNumber = nextId,
          seqNum = nextCreatorSeqNum
        )
      _ <- BlockDagStorage[F].insert(modifiedBlock, invalid, false)
      _ <- BlockStore[F].put(block.blockHash, modifiedBlock)
    } yield modifiedBlock

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
