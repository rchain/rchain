package coop.rchain.casper.merging
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.protocol.{BlockMessage, DeployChain}
import coop.rchain.casper.syntax.casperSyntaxRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax.modelsSyntaxByteString
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.MergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{ChannelChange, StateChange, StateChangeMerger}
import coop.rchain.rspace.syntax.rspaceSyntaxHistoryReader
import coop.rchain.shared.Stopwatch
import scodec.bits.ByteVector

import scala.collection.concurrent.TrieMap

object DeployChainMerger {

  // Todo purge cache.
  val indexCache    = TrieMap.empty[DeployChain, DeployChainIndex]
  val blocksIndexed = TrieMap.empty[BlockHash, Unit]

  def getDeployChainIndex[F[_]: Sync](v: DeployChain): F[DeployChainIndex] = {
    val errMsg = s"No merging index available. Is block indexing enabled on block replay?"
    indexCache.get(v).liftTo(new Exception(errMsg))
  }

  def indexBlock[F[_]: Concurrent: RuntimeManager](b: BlockMessage): F[Unit] = {
    val blockPreState  = b.body.state.preStateHash
    val blockPostState = b.body.state.postStateHash
    val blockSender    = b.sender.toByteArray
    for {
      mergeableChs <- RuntimeManager[F].loadMergeableChannels(blockPostState, blockSender, b.seqNum)
      index <- BlockIndexer(
                b.blockHash,
                b.body.deploys,
                b.body.systemDeploys,
                blockPreState.toBlake2b256Hash,
                blockPostState.toBlake2b256Hash,
                RuntimeManager[F].getHistoryRepo,
                mergeableChs
              )
      _ = index.map { case (k, v) => DeployChainMerger.indexCache.putIfAbsent(k, v) }
      _ = DeployChainMerger.blocksIndexed.putIfAbsent(b.blockHash, ())
    } yield ()
  }

  /**
    * Create new state which is merge of merge set into base state.
    * @param baseState Base state.
    * @param mergeSet Set of deploy chains to merge.
    * @param runtimeManager Accessor to Rholang runtime.
    * @return Hash of the new state + diagnostics data Todo remove.
    */
  def merge[F[_]: Sync](baseState: Blake2b256Hash, mergeSet: Set[DeployChain])(
      runtimeManager: RuntimeManager[F]
  ): F[(Blake2b256Hash, Int, String, String)] =
    for {
      indices     <- mergeSet.toList.traverse(getDeployChainIndex[F])
      baseReader  = runtimeManager.getHistoryRepo.getHistoryReader(baseState).readerBinary
      baseGetData = (k: Blake2b256Hash) => baseReader.getData(k).map(_.map(_.decoded))
      // TODO: Negative or overflow should be rejected before!
      allMergeableChannels = indices.map(_.eventLogIndex.numberChannelsData).combineAll
      overrideTrieAction = (
          hash: Blake2b256Hash,
          changes: ChannelChange[ByteVector],
          numberChs: NumberChannelsDiff
      ) =>
        numberChs.get(hash).traverse {
          RholangMergingLogic.calculateNumberChannelMerge(hash, changes, baseGetData)
        }
      computeTrieActions = (changes: StateChange, mergeableChs: NumberChannelsDiff) => {
        StateChangeMerger.computeTrieActions(
          changes,
          baseReader,
          mergeableChs,
          overrideTrieAction
        )
      }
      changes                        = indices.map(_.stateChanges).combineAll
      r                              <- Stopwatch.duration(computeTrieActions(changes, allMergeableChannels))
      (trieActions, trieActionsTime) = r
      r <- Stopwatch.duration(
            runtimeManager.getHistoryRepo
              .reset(baseState)
              .flatMap(_.doCheckpoint(trieActions).map(_.root))
          )
      (finalState, postStateTime) = r
    } yield (finalState, trieActions.size, trieActionsTime, postStateTime)
}
