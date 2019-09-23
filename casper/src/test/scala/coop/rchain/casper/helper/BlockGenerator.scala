package coop.rchain.casper.helper

import cats._
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.computeDeploysCheckpoint
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.shared.Time
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Task

import scala.collection.immutable.HashMap
import scala.language.higherKinds

object BlockGenerator {
  implicit val timeEff = new LogicalTime[Task]
  private[this] val GenerateBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "generate-block")

  implicit val logSource: LogSource = LogSource(this.getClass)

  def step[F[_]: BlockDagStorage: BlockStore: Time: Metrics: Log: Span: Sync](
      runtimeManager: RuntimeManager[F]
  )(block: BlockMessage, genesis: BlockMessage): F[Unit] =
    for {
      dag                                       <- BlockDagStorage[F].getRepresentation
      computeBlockCheckpointResult              <- computeBlockCheckpoint(block, genesis, dag, runtimeManager)
      (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
      result <- injectPostStateHash[F](
                 block,
                 genesis,
                 postB1StateHash,
                 postB1ProcessedDeploys
               )
    } yield result

  private def computeBlockCheckpoint[F[_]: Sync: Log: BlockStore: Time: Metrics: Span](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[(StateHash, Seq[ProcessedDeploy])] = Span[F].trace(GenerateBlockMetricsSource) {
    for {
      parents <- ProtoUtil.getParents[F](b)
      deploys = ProtoUtil.deploys(b).map(_.deploy)
      now     <- Time[F].currentMillis
      result <- computeDeploysCheckpoint[F](
                 parents,
                 deploys,
                 dag,
                 runtimeManager,
                 BlockData(now, b.body.state.blockNumber),
                 Map.empty[BlockHash, Validator]
               )
      Right((preStateHash, postStateHash, processedDeploys)) = result
    } yield (postStateHash, processedDeploys.map(_.toProcessedDeploy))
  }

  private def injectPostStateHash[F[_]: Monad: BlockStore: BlockDagStorage](
      b: BlockMessage,
      genesis: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): F[Unit] = {
    val updatedBlockPostState = b.body.state.copy(postStateHash = postGenStateHash)
    val updatedBlockBody =
      b.body.copy(state = updatedBlockPostState, deploys = processedDeploys.toList)
    val updatedBlock = b.copy(body = updatedBlockBody)
    BlockStore[F].put(b.blockHash, updatedBlock) >>
      BlockDagStorage[F].insert(updatedBlock, genesis, invalid = false).void
  }
}

trait BlockGenerator {
  def buildBlock[F[_]: Monad: Time](
      parentsHashList: Seq[BlockHash],
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): F[BlockMessage] =
    Time[F].currentMillis.map(now => {
      val postState: RChainState = Dummies.createRChainState(
        preStateHash = preStateHash,
        postStateHash = tsHash,
        bonds = bonds.toList
      )
      val header = Dummies.createHeader(
        parentHashes = parentsHashList.toList,
        deploysHash = ProtoUtil.protoSeqHash(deploys.map(_.toProto)),
        timestamp = now
      )
      val blockHash = Blake2b256.hash(header.toProto.toByteArray)
      val body      = Dummies.createBody(state = postState, deploys = deploys.toList)
      val serializedJustifications = justifications.toList.map {
        case (cr: Validator, latestBlockHash: BlockHash) =>
          Justification(cr, latestBlockHash)
      }
      val serializedBlockHash = ByteString.copyFrom(blockHash)

      Dummies.createBlockMessage(
        blockHash = serializedBlockHash,
        header = header,
        body = body,
        justifications = serializedJustifications,
        sender = creator,
        shardId = shardId,
        seqNum = seqNum
      )

    })

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
