package coop.rchain.blockstorage.dag

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.DeployChain
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
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, Stopwatch}
import fs2.Stream

object DeployChainSetCasper {

  implicit val metaOrd: Ordering[BlockMetadata] = Ordering.by[BlockMetadata, BlockHash](_.blockHash)

  /** Implementations of Casper traits for BlockDagRepresentation */
  final case class BlockMetadataDag[F[_]: Sync](
      dag: BlockDagRepresentation[F]
  ) extends DependencyGraph[F, BlockMetadata, Validator] {
    override def justifications(message: BlockMetadata): F[List[BlockMetadata]] =
      message.justifications.map(_.latestBlockHash).traverse(dag.lookupUnsafe(_))
    override def sender(message: BlockMetadata): Validator = message.sender
    override def seqNum(message: BlockMetadata): Int       = message.seqNum
    override def parents(
        message: BlockMetadata
    ): F[List[BlockMetadata]] = message.parents.traverse(dag.lookupUnsafe)
    override def children(
        message: BlockMetadata
    ): F[List[BlockMetadata]] =
      dag.children(message.blockHash).flatMap(_.getOrElse(Set()).toList.traverse(dag.lookupUnsafe))
  }

  final case class BlockMetadataSafetyOracle[F[_]: Sync]()
      extends SafetyOracle[F, BlockMetadata, Validator] {
    override def compatible(source: BlockMetadata, target: BlockMetadata): Boolean = true
    override def bondsMap(message: BlockMetadata): Map[Validator, Long]            = message.weightMap
  }

  final case class BlockMetadataCasper[F[_]: Sync](
      override val faultToleranceThreshold: Float,
      override val maxDepth: Long
  ) extends Casper[F, BlockMetadata, Validator]

  /** Compute message scope given BlockDagRepresentation and latest messages. */
//  def computeMessageScope[F[_]: Sync](
//      latestMessages: Set[BlockMetadata],
//      dag: BlockDagRepresentation[F]
//  ): F[MessageScope[Validtor, BlockMetadata]] = {
//    val casperMaxDepth = 100L
//    val metaDag        = BlockMetadataDag(dag)
//    val safetyOracle   = BlockMetadataSafetyOracle()
//    val casper         = BlockMetadataCasper(faultToleranceThreshold = -1, maxDepth = casperMaxDepth)
//
//    Sync[F].handleErrorWith(casper.messageScope(latestMessages, metaDag, safetyOracle)) {
//      case NoFinalizationFringe =>
//        for {
//          maxHeight <- dag.getPureState.heightMap.lastOption
//                        .liftTo(new Exception(s"Empty height map when computing message scope."))
//          // This is equivalent of genesis ceremony.
//          // If no finalized fringe is found, this means that approvals of genesis done by genesis validators
//          // (which are just messages on top), are not finalized, therefore uncertain.
//          // Genesis ceremony is constrained by casper complexity. When genesis is out of reach of the very top
//          // message and there is still no finalization fringe available, failure of the ceremony can be declared.
//          genesisInReach = maxHeight._1 < casperMaxDepth
//          gsFailed = s"Genesis is out of reach of the highest message (Casper depth is $casperMaxDepth) " +
//            s"and still not finalized. Genesis ceremony failed."
//          _ <- new Exception(gsFailed).raiseError.whenA(!genesisInReach)
//          genesis = dag.getPureState.heightMap.head
//            .ensuring(_._1 == 0, "Genesis has height != 0 ")
//            .ensuring(_._2.size == 1, "Multiple messages with height 0")
//            ._2
//            .head
//          conflictScope <- Stream
//                            .emits(
//                              latestMessages.toList
//                                .map(lm => Stream(lm) ++ metaDag.selfJustificationChain(lm))
//                            )
//                            .covary[F]
//                            .flatten
//                            .compile
//                            .to(Set)
//          base <- dag.lookupUnsafe(genesis)
//        } yield MessageScope(FinalizationFringe(Set(base)), ConflictScope(conflictScope - base))
//    }
//  }
}
