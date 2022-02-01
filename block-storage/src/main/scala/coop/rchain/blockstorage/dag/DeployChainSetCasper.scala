package coop.rchain.blockstorage.dag

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.DeployChain
import coop.rchain.blockstorage.casper.Casper.{
  ConflictScope,
  FinalizationFringe,
  MessageScope,
  NoFinalizationFringe
}
import coop.rchain.blockstorage.casper.syntax.all._
import coop.rchain.blockstorage.casper.{Casper, DependencyGraph, SafetyOracle2}
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
    override def seqNum(message: BlockMetadata): Long      = message.seqNum
    override def parents(
        message: BlockMetadata
    ): F[List[BlockMetadata]] = message.parents.traverse(dag.lookupUnsafe)
    override def children(
        message: BlockMetadata
    ): F[List[BlockMetadata]] =
      dag
        .closestChildren(message.blockHash)
        .flatMap(_.getOrElse(Set()).toList.traverse(dag.lookupUnsafe))
    override def witnesses(
        message: BlockMetadata
    ): F[List[BlockMetadata]] =
      dag
        .witnesses(message.blockHash)
        .flatMap(_.getOrElse(Set()).toList.traverse(dag.lookupUnsafe))
  }

  final case class BlockMetadataSafetyOracle[F[_]: Sync]()
      extends SafetyOracle2[F, BlockMetadata, Validator] {
    override def compatible(source: BlockMetadata, target: BlockMetadata): Boolean = true
    override def bondsMap(message: BlockMetadata): Map[Validator, Long]            = message.weightMap
  }

  final case class BlockMetadataCasper[F[_]: Sync](
      override val faultToleranceThreshold: Float,
      override val maxDepth: Long
  ) extends Casper[F, BlockMetadata, Validator]
}
