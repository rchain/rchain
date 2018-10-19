package coop.rchain.casper.helper

import cats._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, ProcessedDeployUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Time
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap
import scala.language.higherKinds

object BlockGenerator {
  implicit val timeEff

  def updateChainWithBlockStateUpdate(
      id: Int,
      genesis: BlockMessage,
      runtimeManager: RuntimeManager,
      chain: IndexedBlockDag
  )(implicit blockStore: BlockStore[Id]): (BlockMessage, IndexedBlockDag) = {
    val b = chain.idToBlocks(id)
    val (postStateHash, processedDeploys) =
      computeBlockCheckpoint(
        b,
        genesis,
        chain,
        runtimeManager
      )
    val updatedChain = injectPostStateHash(chain, id, b, postStateHash, processedDeploys)
    (b, updatedChain)
  }

  def computeBlockCheckpoint(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      runtimeManager: RuntimeManager
  )(implicit blockStore: BlockStore[Id]): (StateHash, Seq[ProcessedDeploy]) = {
    val Right((preStateHash, postStateHash, processedDeploys)) =
      InterpreterUtil
        .computeBlockCheckpointFromDeploys[Id](b, genesis, dag, runtimeManager)

    (postStateHash, processedDeploys.map(ProcessedDeployUtil.fromInternal))
  }

  def injectPostStateHash(
      chain: IndexedBlockDag,
      id: Int,
      b: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  )(implicit blockStore: BlockStore[Id]): IndexedBlockDag = {
    val updatedBlockPostState = b.getBody.getState.withPostStateHash(postGenStateHash)
    val updatedBlockBody =
      b.getBody.withState(updatedBlockPostState).withDeploys(processedDeploys)
    val updatedBlock = b.withBody(updatedBlockBody)
    blockStore.put(b.blockHash, updatedBlock)
    chain.copy(idToBlocks = chain.idToBlocks.updated(id, updatedBlock))
  }
}

trait BlockGenerator {
  def createBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      parentsHashList: Seq[BlockHash],
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY
  ): F[BlockMessage] =
    for {
      now           <- Time[F].currentMillis
      postState     = RChainState().withPreStateHash(preStateHash)
        .withPostStateHash(tsHash).withBonds(bonds)
      postStateHash = Blake2b256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
        .withDeploysHash(ProtoUtil.protoSeqHash(deploys))
        .withTimestamp(now)
      blockHash = Blake2b256.hash(header.toByteArray)
      body      = Body().withState(postState).withDeploys(deploys)
      serializedJustifications = justifications.toList.map {
        case (creator: Validator, latestBlockHash: BlockHash) =>
          Justification(creator, latestBlockHash)
      }
      serializedBlockHash = ByteString.copyFrom(blockHash)
      block = BlockMessage(
        serializedBlockHash,
        Some(header),
        Some(body),
        serializedJustifications,
        creator,
        shardId = shardId
      )
      modifiedBlock <- IndexedBlockDagStorage[F].insertIndexed(block)
      _             <- BlockStore[F].put(serializedBlockHash, modifiedBlock)
    } yield modifiedBlock
}
