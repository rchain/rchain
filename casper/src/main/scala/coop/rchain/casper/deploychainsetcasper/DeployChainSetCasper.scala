package coop.rchain.casper.deploychainsetcasper

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.merging.DeployChainMerger
import coop.rchain.casper.protocol.DeployChain
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.v2.core.Casper.{
  ConflictScope,
  FinalizationFringe,
  MessageScope,
  NoFinalizationFringe
}
import coop.rchain.casper.v2.core.syntax.all._
import coop.rchain.casper.v2.core.{Casper, DependencyGraph, SafetyOracle}
import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.casper.v2.stcasper.StateMessage
import coop.rchain.casper.v2.stcasper.syntax.all._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.syntax._
import coop.rchain.models.Validator.Validator
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, Stopwatch}
import fs2.Stream

object DeployChainSetCasper {

  /** Representation of BlockMetadata as a state message*/
  def metaAsState(meta: BlockMetadata): StateMessage[DeployChain] = new StateMessage[DeployChain] {
    override def proposed = meta.stateMetadata.proposed.toSet
    override def merged: ConflictResolution[DeployChain] =
      ConflictResolution(
        meta.stateMetadata.acceptedSet.toSet,
        meta.stateMetadata.rejectedSet.toSet
      )
  }

  implicit val metaOrd: Ordering[BlockMetadata] = Ordering.by[BlockMetadata, BlockHash](_.blockHash)

  /** Implementations of Casper traits for BlockDagRepresentation */
  final case class BlockMetadataDag[F[_]: Sync](
      dag: BlockDagRepresentation[F]
  ) extends DependencyGraph[F, BlockMetadata, Validator] {
    override def justifications(message: BlockMetadata): F[List[BlockMetadata]] =
      message.justifications.map(_.latestBlockHash).traverse(dag.lookupUnsafe(_))
    override def sender(message: BlockMetadata): Validator = message.sender
    override def seqNum(message: BlockMetadata): Int       = message.seqNum
  }

  final case class BlockMetadataSafetyOracle[F[_]: Sync]()
      extends SafetyOracle[F, BlockMetadata, Validator] {
    override def compatible(source: BlockMetadata, target: BlockMetadata): Boolean =
      !metaAsState(source).conflicts(metaAsState(target))
    override def bondsMap(message: BlockMetadata): Map[Validator, Long] = message.weightMap
  }

  final case class BlockMetadataCasper[F[_]: Sync](
      override val faultToleranceThreshold: Float,
      override val maxDepth: Long
  ) extends Casper[F, BlockMetadata, Validator]

  /** Compute message scope given BlockDagRepresentation and latest messages. */
  def computeMessageScope[F[_]: Sync](
      latestMessages: Set[BlockMetadata],
      dag: BlockDagRepresentation[F]
  ): F[MessageScope[BlockMetadata]] = {
    val casperMaxDepth = 100L
    val metaDag        = BlockMetadataDag(dag)
    val safetyOracle   = BlockMetadataSafetyOracle()
    val casper         = BlockMetadataCasper(faultToleranceThreshold = -1, maxDepth = casperMaxDepth)

    Sync[F].handleErrorWith(casper.messageScope(latestMessages, metaDag, safetyOracle)) {
      case NoFinalizationFringe =>
        for {
          maxHeight <- dag.getPureState.heightMap.lastOption
                        .liftTo(new Exception(s"Empty height map when computing message scope."))
          // This is equivalent of genesis ceremony.
          // If no finalized fringe is found, this means that approvals of genesis done by genesis validators
          // (which are just messages on top), are not finalized, therefore uncertain.
          // Genesis ceremony is constrained by casper complexity. When genesis is out of reach of the very top
          // message and there is still no finalization fringe available, failure of the ceremony can be declared.
          genesisInReach = maxHeight._1 < casperMaxDepth
          gsFailed = s"Genesis is out of reach of the highest message (Casper depth is $casperMaxDepth) " +
            s"and still not finalized. Genesis ceremony failed."
          _ <- new Exception(gsFailed).raiseError.whenA(!genesisInReach)
          genesis = dag.getPureState.heightMap.head
            .ensuring(_._1 == 0, "Genesis has height != 0 ")
            .ensuring(_._2.size == 1, "Multiple messages with height 0")
            ._2
            .head
          conflictScope <- Stream
                            .emits(latestMessages.toList.map(metaDag.selfJustificationChain))
                            .covary[F]
                            .flatten
                            .compile
                            .to(Set)
          base <- dag.lookupUnsafe(genesis)
        } yield MessageScope(FinalizationFringe(Set(base)), ConflictScope(conflictScope - base))
    }
  }

  /** Merge finalization fringe into single finalized state.  */
  def mergeFinalizationFringe[F[_]: Concurrent: BlockStore: Log: Metrics](
      fringe: Set[BlockMetadata],
      dag: BlockDagRepresentation[F],
      finalizedRejections: Set[DeployChain],
      fillMergingIndex: List[BlockHash] => F[Unit]
  )(runtimeManager: RuntimeManager[F]): F[Blake2b256Hash] =
    for {
      r                <- BlockMetadataDag(dag).mergeScope(fringe)
      (base, mergeSet) = r
      _                <- fillMergingIndex(mergeSet.map(_.blockHash).toList)
      acceptedSet      = mergeSet.flatMap(_.stateMetadata.proposed) diff finalizedRejections
      // compute approved state
      r <- Stopwatch.duration(
            DeployChainMerger.merge(
              Blake2b256Hash.fromByteString(base.message.postStateHash),
              acceptedSet
            )(runtimeManager)
          )
      (
        (
          finalizedState,
          actionsNum,
          trieActionComputeTime,
          stateComputedTime
        ),
        mergeTime
      ) = r
      _ <- Log[F].info(
            s"Finalization fringe merged in $mergeTime: ${acceptedSet.size} DC merged, " +
              s"${actionsNum} trie actions computed in $trieActionComputeTime, applied in $stateComputedTime."
          )
    } yield finalizedState
}
