package coop.rchain.casper.blocks.merger

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogIndex
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.store.{
  KeyValueCache,
  KeyValueTypedStore,
  LazyAdHocKeyValueCache,
  LazyKeyValueCache
}
import coop.rchain.shared.syntax._

import scala.collection.Seq
import scala.collection.immutable.Set

final case class BlockIndex private (
    chanToDeployMap: Map[Blake2b256Hash, Set[ProcessedDeploy]],
    deployToChanMap: Map[ProcessedDeploy, Set[Blake2b256Hash]],
    eventLogIndex: EventLogIndex,
    deploys: Set[ProcessedDeploy]
) {
  def +(that: BlockIndex) =
    BlockIndex(
      this.chanToDeployMap ++ that.chanToDeployMap,
      this.deployToChanMap ++ that.deployToChanMap,
      this.eventLogIndex + that.eventLogIndex,
      this.deploys ++ that.deploys
    )
}

final case class BranchIndex(
    chanToDeployMap: Map[Blake2b256Hash, Set[ProcessedDeploy]],
    deployToChanMap: Map[ProcessedDeploy, Set[Blake2b256Hash]],
    eventLogIndex: EventLogIndex,
    deploys: Set[ProcessedDeploy]
) {
  def calculateRejectedDeploys(conflicts: Set[Blake2b256Hash]): Set[ProcessedDeploy] =
    conflicts.flatMap(c => chanToDeployMap.getOrElse(c, Set.empty[ProcessedDeploy]))
}

object BranchIndex {
  def apply(blocksIndexes: Seq[BlockIndex]): BranchIndex = {
    val r = blocksIndexes.reduce((a, b) => a + b)
    BranchIndex(r.chanToDeployMap, r.deployToChanMap, r.eventLogIndex, r.deploys)
  }
}

object Indexer {
  def createBlockIndex[F[_]: Concurrent](
      vertex: MergingVertex
  ): F[BlockIndex] =
    for {
      chanToDeployMapRef <- Ref.of[F, Map[Blake2b256Hash, Set[ProcessedDeploy]]](Map.empty)
      deployToChanMapRef <- Ref.of[F, Map[ProcessedDeploy, Set[Blake2b256Hash]]](Map.empty)

      producesLinearRef     <- Ref.of[F, Set[Produce]](Set.empty)
      producesPersistentRef <- Ref.of[F, Set[Produce]](Set.empty)

      consumesLinearRef     <- Ref.of[F, Set[Consume]](Set.empty)
      consumesPersistentRef <- Ref.of[F, Set[Consume]](Set.empty)

      producesDestroyedRef <- Ref.of[F, Set[Produce]](Set.empty)
      consumesDestroyedRef <- Ref.of[F, Set[Consume]](Set.empty)

      producesPeekedRef <- Ref.of[F, Set[Produce]](Set.empty)

      deploys = vertex.processedDeploys
      events = deploys.toParArray.flatMap(
        d => d.deployLog.par.map(e => (EventConverter.toRspaceEvent(e), d))
      )

      ps = events.toList.map {
        case (e, deploy) =>
          fs2.Stream.eval(e match {
            case p: Produce =>
              for {
                _ <- chanToDeployMapRef.update(
                      s =>
                        s.updated(
                          p.channelsHash,
                          s.getOrElse(p.channelsHash, Set.empty) + deploy
                        )
                    )
                _ <- deployToChanMapRef.update(
                      s => s.updated(deploy, s.getOrElse(deploy, Set.empty) + p.channelsHash)
                    )
                _ <- producesLinearRef
                      .update(s => s + p)
                      .unlessA(p.persistent)
                _ <- producesPersistentRef
                      .update(s => s + p)
                      .whenA(p.persistent)
              } yield ()
            case c: Consume =>
              for {
                _ <- chanToDeployMapRef.update(
                      s =>
                        c.channelsHashes.foldLeft(s)(
                          (acc, ch) => acc.updated(ch, s.getOrElse(ch, Set.empty) + deploy)
                        )
                    )
                _ <- deployToChanMapRef.update(
                      s => s.updated(deploy, s.getOrElse(deploy, Set.empty) ++ c.channelsHashes)
                    )
                _ <- consumesLinearRef
                      .update(s => s + c)
                      .unlessA(c.persistent)
                _ <- consumesPersistentRef
                      .update(s => s + c)
                      .whenA(c.persistent)
              } yield ()
            case c: COMM => {
              for {
                _ <- producesDestroyedRef
                      .update(s => s ++ c.produces)
                      .whenA(c.peeks.isEmpty)
                _ <- producesPeekedRef
                      .update(s => s ++ c.produces)
                      .whenA(c.peeks.nonEmpty)
                _ <- consumesDestroyedRef
                      .update(s => s + c.consume)
              } yield ()
            }
          })
      }
      _ <- fs2.Stream.emits(ps).parJoinProcBounded.compile.drain

      chanToDeployMap <- chanToDeployMapRef.get
      deployToChanMap <- deployToChanMapRef.get

      // NOTE: this produces linear ideally should contain only produces created inside branch.
      // but because peek is not atomic and creates the same produce that it consumed, when invoke, this is not true
      // So produce potentially can be from base state.
      producesLinear         <- producesLinearRef.get
      producesPersistent     <- producesPersistentRef.get
      producesDestroyed      <- producesDestroyedRef.get
      producesPeeked         <- producesPeekedRef.get
      consumesLinearAndPeeks <- consumesLinearRef.get
      consumesPersistent     <- consumesPersistentRef.get
      consumesDestroyed      <- consumesDestroyedRef.get

    } yield BlockIndex(
      chanToDeployMap = chanToDeployMap,
      eventLogIndex = EventLogIndex(
        producesLinear = producesLinear,
        producesPersistent = producesPersistent,
        producesDestroyed = producesDestroyed,
        producesPeeked = producesPeeked,
        consumesLinearAndPeeks = consumesLinearAndPeeks,
        consumesPersistent = consumesPersistent,
        consumesDestroyed = consumesDestroyed
      ),
      deploys = deploys,
      deployToChanMap = deployToChanMap
    )

  def createBranchIndex[F[_]: Concurrent](
      vertices: Seq[MergingVertex]
  )(implicit indexCache: LazyKeyValueCache[F, MergingVertex, BlockIndex]): F[BranchIndex] =
    for {
      indexes <- fs2.Stream
                  .emits(vertices)
                  .parEvalMapProcBounded(indexCache.get)
                  .compile
                  .toList
    } yield BranchIndex(indexes)
}
