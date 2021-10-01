package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.merging.DeployChainMerger.indexBlock
import coop.rchain.casper.merging.{DeployChainIndex, DeployChainMerger}
import coop.rchain.casper.protocol.DeployChain
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.v2.core.Casper.{
  ConflictScope,
  FinalizationFringe,
  LatestMessages,
  MessageScope
}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator

import scala.collection.concurrent.TrieMap

/**
  * Defines the complete state that is sufficient to process all future messages.
  *
  * The [[NetworkSnapshot]] computed for the most recent self proposed message
  * defines a Pruned state for clients to bootstrap from (or to truncate local database to).
  *
  * @param scope                The scope that matters, everything outside is not needed and can be pruned.
  * @param finalizedStateRoot   Root hash of the latest finalized state in the Merkle tree.
  *                             This state is a merge of states of messages of [[finalizationFringe]].
  * @param mergingIndices       Merging indices for all blocks in conflict scope.
  * @param invalidMessages
  * @param deploysInScope
  */
final case class NetworkSnapshot(
    latestMessages: LatestMessages[Validator, BlockMetadata],
    conflictScope: ConflictScope[BlockMetadata],
    finalizationFringe: FinalizationFringe[BlockMetadata],
    finalizedStateRoot: StateHash,
    mergingIndices: Map[DeployChain, DeployChainIndex],
    invalidMessages: Map[Validator, BlockHash],
    deploysInScope: Set[DeployId]
) {
  private val indices     = mergingIndices.keys.toSet
  private val conflictSet = conflictScope.conflictSet(_.stateMetadata.proposed)
  require(
    conflictSet.forall(indices.contains),
    "Network snapshot does not contain indices for all messages in conflict scope."
  )
}

object NetworkSnapshot {

  /**
    * @param latestMessages   Latest messages
    * @param messageScope     Message scope, defined by latest messages
    * @param finalizedState   Finalize state of the RSpace
    * @param invalidMessages  Invalid messages
    * @return Network snapshot
    */
  def apply[F[_]: Concurrent: RuntimeManager: BlockStore](
      messageScope: MessageScope[Validator, BlockMetadata],
      finalizedState: FinalizationFringe[BlockMetadata] => F[StateHash],
      invalidMessages: Map[Validator, BlockHash]
  ) = {
    import messageScope._
    // Merging indices that are required but missing in cache
    val indicesMissing =
      conflictScope.messages
        .map(_.blockHash)
        .filter(!DeployChainMerger.blocksIndexed.keySet.contains(_))
    // deploys in the scope Todo double check 50 below
    val deploysInScope = conflictScope.messages.flatMap(_.stateMetadata.proposed.flatMap(_.deploys))

    for {
      _ <- indicesMissing.toList.traverse(
            BlockStore[F].getUnsafe(_).flatMap(indexBlock)
          )
      mergingIndices = DeployChainMerger.indexCache.filterKeys(
        k => (k.deploys.toSet intersect conflictScope.messages.map(_.blockHash)).nonEmpty
      )
      finalizedState <- finalizedState(finalizationFringe)

    } yield NetworkSnapshot(
      latestMessages,
      finalizationFringe,
      finalizedState,
      conflictScope,
      mergingIndices,
      invalidMessages,
      deploysInScope
    )
  }

  val finalizedStateCache = TrieMap.empty[Set[BlockHash], StateHash]

  /** @return (optional) Root hash of an RSpace state on which the network is acquiescent. */
  def acquiescentState(networkState: NetworkSnapshot): Option[StateHash] = {
    val postStates = networkState.latestMessages.flatMap { case (_, m) => m.map(_.postStateHash) }.toSet
    // all latest messages should bear the same post state which should be finalized
    (postStates.size == 1 && postStates.head == networkState.finalizedStateRoot)
      .guard[Option]
      .as(postStates.head)
  }

  def isAcquiescent(networkState: NetworkSnapshot): Boolean =
    acquiescentState(networkState).nonEmpty

  // Validators that should be slashed because of invalid block, but still active in this state
  def bondedOffenders[F[_]: Sync](
      networkState: NetworkSnapshot,
      activeValidators: Set[Validator]
  ): Iterator[(Validator, BlockHash)] =
    networkState.invalidMessages.toIterator.filter {
      case (validator, _) => activeValidators.contains(validator)
    }

//  def mkSnapshot[F[_]: Concurrent: BlockStore: RuntimeManager: Log: Metrics](
//      networkState: NetworkSnapshot,
//      dag: BlockDagRepresentation[F],
//      finalizationState: BlockDagFinalizationState,
//      conflictsResolver: ConflictsResolver[F, DeployChain]
//  ): F[CasperSnapshot] = {
//    val fringeMergeCache = OptionT.fromOption[F](
//      finalizedStateCache.get(networkState.finalizationFringe.v.map(_.blockHash))
//    )
//    val mergeFringe = mergeFinalizationFringe(
//      networkState.finalizationFringe.v,
//      dag,
//      finalizationState.rejected,
//      hashes =>
//        hashes.traverse { h =>
//          BlockStore[F]
//            .getUnsafe(h)
//            .flatMap(indexBlock)
//            .unlessA(DeployChainMerger.blocksIndexed.contains(h))
//        }.void
//    )(RuntimeManager[F])
//
//    for {
//      finalizedState <- fringeMergeCache.getOrElseF(mergeFringe)
//      conflictSet    = networkState.conflictScope.v.flatMap(_.stateMetadata.proposed)
//      conflictResolution <- DeployChainSetConflictResolver[F](
//                             DeployChainMerger.getDeployChainIndex[F]
//                           ).resolve(conflictSet)
//      r <- Stopwatch.duration(
//            DeployChainMerger.merge(finalizedState, conflictResolution.acceptedSet)(
//              RuntimeManager[F]
//            )
//          )
//      ((finalState, trieActionsNum, trieActionsTime, postStateTime), mergeStateT) = r
//      _ <- Log[F].info(
//            s"Message scope (${networkState.conflictScope.v.size} messages) computed in $messageScopeT, " +
//              s"merged in $mergeStateT ($trieActionsNum trie actions computed in $trieActionsTime, applied in $postStateTime)."
//          )
//      // Todo read these from state
//      deployLifespan = 50
//      casperVersion  = 0L
//    } yield ()
//  }
}
