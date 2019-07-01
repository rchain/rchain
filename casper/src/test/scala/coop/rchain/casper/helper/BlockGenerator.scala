package coop.rchain.casper.helper

import cats._
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.computeDeploysCheckpoint
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{InternalProcessedDeploy, RuntimeManager}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Time
import monix.eval.Task

import scala.collection.immutable.HashMap
import scala.language.higherKinds
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rholang.interpreter.Runtime.BlockData
import scala.concurrent.duration.`package`.span

object BlockGenerator {
  implicit val timeEff = new LogicalTime[Task]

  private[this] val GenerateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "generate-block")

  def updateChainWithBlockStateUpdate[F[_]: Sync: BlockStore: IndexedBlockDagStorage: Time: Metrics: Span](
      id: Int,
      genesis: BlockMessage,
      runtimeManager: RuntimeManager[F]
  ): F[BlockMessage] =
    for {
      b   <- IndexedBlockDagStorage[F].lookupByIdUnsafe(id)
      dag <- IndexedBlockDagStorage[F].getRepresentation
      computeBlockCheckpointResult <- computeBlockCheckpoint[F](
                                       b,
                                       genesis,
                                       dag,
                                       runtimeManager
                                     )
      (postStateHash, processedDeploys) = computeBlockCheckpointResult
      _                                 <- injectPostStateHash[F](id, b, genesis, postStateHash, processedDeploys)
    } yield b

  def computeBlockCheckpoint[F[_]: Sync: BlockStore: Time: Metrics: Span](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[(StateHash, Seq[ProcessedDeploy])] = Span[F].child(GenerateBlockMetricsSource) { span =>
    for {
      result                                                 <- computeBlockCheckpointFromDeploys[F](b, genesis, dag, runtimeManager, span)
      Right((preStateHash, postStateHash, processedDeploys)) = result
    } yield (postStateHash, processedDeploys.map(_.toProcessedDeploy))
  }

  def injectPostStateHash[F[_]: Monad: BlockStore: IndexedBlockDagStorage](
      id: Int,
      b: BlockMessage,
      genesis: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): F[Unit] = {
    val updatedBlockPostState = b.getBody.getState.withPostStateHash(postGenStateHash)
    val updatedBlockBody =
      b.getBody.withState(updatedBlockPostState).withDeploys(processedDeploys)
    val updatedBlock = b.withBody(updatedBlockBody)
    BlockStore[F].put(b.blockHash, updatedBlock) *>
      IndexedBlockDagStorage[F].inject(id, updatedBlock, genesis, invalid = false)
  }

  private def computeBlockCheckpointFromDeploys[F[_]: Sync: BlockStore: Time](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      span: Span[F]
  ): F[Either[Throwable, (StateHash, StateHash, Seq[InternalProcessedDeploy])]] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      deploys = ProtoUtil.deploys(b).flatMap(_.deploy)

      _ = assert(
        parents.nonEmpty || (parents.isEmpty && b == genesis),
        "Received a different genesis block."
      )

      now <- Time[F].currentMillis
      result <- computeDeploysCheckpoint[F](
                 parents,
                 deploys,
                 dag,
                 runtimeManager,
                 BlockData(now, b.body.get.state.get.blockNumber),
                 span,
                 Map.empty[BlockHash, Validator]
               )
    } yield result
}

trait BlockGenerator {
  private def buildBlock[F[_]: Monad: Time](
      parentsHashList: Seq[BlockHash],
      creator: Validator,
      bonds: Seq[Bond],
      justifications: collection.Map[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy],
      tsHash: ByteString,
      shardId: String,
      preStateHash: ByteString,
      seqNum: Int
  ): F[BlockMessage] =
    for {
      now <- Time[F].currentMillis
      postState = RChainState()
        .withPreStateHash(preStateHash)
        .withPostStateHash(tsHash)
        .withBonds(bonds)
      postStateHash = Blake2b256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
        .withDeploysHash(ProtoUtil.protoSeqHash(deploys))
        .withTimestamp(now)
      blockHash = Blake2b256.hash(header.toByteArray)
      body      = Body().withState(postState).withDeploys(deploys)
      serializedJustifications = justifications.toList.map {
        case (cr: Validator, latestBlockHash: BlockHash) =>
          Justification(cr, latestBlockHash)
      }
      serializedBlockHash = ByteString.copyFrom(blockHash)
    } yield BlockMessage(
      serializedBlockHash,
      Some(header),
      Some(body),
      serializedJustifications,
      creator,
      shardId = shardId,
      seqNum = seqNum
    )

  def createGenesis[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): F[BlockMessage] =
    for {
      genesis <- buildBlock[F](
                  Seq.empty,
                  creator,
                  bonds,
                  justifications,
                  deploys,
                  tsHash,
                  shardId,
                  preStateHash,
                  seqNum
                )
      modifiedBlock <- IndexedBlockDagStorage[F].insertIndexed(genesis, genesis, false)
      _             <- BlockStore[F].put(genesis.blockHash, modifiedBlock)
    } yield modifiedBlock

  def createBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      parentsHashList: Seq[BlockHash],
      genesis: BlockMessage,
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): F[BlockMessage] =
    for {
      block <- buildBlock[F](
                parentsHashList,
                creator,
                bonds,
                justifications,
                deploys,
                tsHash,
                shardId,
                preStateHash,
                seqNum
              )
      modifiedBlock <- IndexedBlockDagStorage[F].insertIndexed(block, genesis, false)
      _             <- BlockStore[F].put(block.blockHash, modifiedBlock)
    } yield modifiedBlock
}
