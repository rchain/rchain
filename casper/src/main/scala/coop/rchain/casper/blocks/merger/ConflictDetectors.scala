package coop.rchain.casper.blocks.merger

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.models.{ListParWithRandom, Par}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.shared.Log
import coop.rchain.store.LazyKeyValueCache

import scala.collection.Seq
import scala.collection.immutable.HashSet

final case class MergingBranchIndex[F[_]] private (
    vertices: Set[MergingVertex],
    chanToDeployMap: Map[Blake2b256Hash, Set[ProcessedDeploy]],
    deployToChanMap: Map[ProcessedDeploy, Set[Blake2b256Hash]],
    producesLinear: HashSet[Produce],
    producesPersistent: HashSet[Produce],
    producesDestroyed: HashSet[Produce],
    producesPeeked: HashSet[Produce],
    consumesLinearAndPeeks: HashSet[Consume],
    consumesPersistent: HashSet[Consume],
    consumesDestroyed: HashSet[Consume],
    deploys: Set[ProcessedDeploy]
) {
  def +(that: MergingBranchIndex[F]) =
    new MergingBranchIndex[F](
      this.vertices ++ that.vertices,
      this.chanToDeployMap ++ that.chanToDeployMap,
      this.deployToChanMap ++ that.deployToChanMap,
      this.producesLinear ++ that.producesLinear,
      this.producesPersistent ++ that.producesPersistent,
      this.producesDestroyed ++ that.producesDestroyed,
      this.producesPeeked ++ that.producesPeeked,
      this.consumesLinearAndPeeks ++ that.consumesLinearAndPeeks,
      this.consumesPersistent ++ that.consumesPersistent,
      this.consumesDestroyed ++ that.consumesDestroyed,
      this.deploys ++ that.deploys
    )

  def calculateRejected(conflicts: Set[Blake2b256Hash]): Set[ProcessedDeploy] =
    conflicts.flatMap(c => chanToDeployMap.getOrElse(c, Set.empty[ProcessedDeploy]))
}

object MergingBranchIndex {

  def create[F[_]: Concurrent](
      vertices: Seq[MergingVertex]
  ): F[MergingBranchIndex[F]] =
    for {
      chanToDeployMapRef <- Ref.of[F, Map[Blake2b256Hash, Set[ProcessedDeploy]]](Map.empty)
      deployToChanMapRef <- Ref.of[F, Map[ProcessedDeploy, Set[Blake2b256Hash]]](Map.empty)

      producesLinearRef     <- Ref.of[F, HashSet[Produce]](HashSet.empty)
      producesPersistentRef <- Ref.of[F, HashSet[Produce]](HashSet.empty)

      consumesLinearRef     <- Ref.of[F, HashSet[Consume]](HashSet.empty)
      consumesPersistentRef <- Ref.of[F, HashSet[Consume]](HashSet.empty)

      producesDestroyedRef <- Ref.of[F, HashSet[Produce]](HashSet.empty)
      consumesDestroyedRef <- Ref.of[F, HashSet[Consume]](HashSet.empty)

      producesPeekedRef <- Ref.of[F, HashSet[Produce]](HashSet.empty)

      deploys = vertices
        .flatMap(_.processedDeploys)
        .toSet

      events = deploys
        .flatMap(d => d.deployLog.map(e => (EventConverter.toRspaceEvent(e), d)))

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
      _ <- fs2.Stream.emits(ps).parJoinUnbounded.compile.toList

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

    } yield new MergingBranchIndex(
      vertices = vertices.toSet,
      chanToDeployMap = chanToDeployMap,
      producesLinear = producesLinear,
      producesPersistent = producesPersistent,
      producesDestroyed = producesDestroyed,
      producesPeeked = producesPeeked,
      consumesLinearAndPeeks = consumesLinearAndPeeks,
      consumesPersistent = consumesPersistent,
      consumesDestroyed = consumesDestroyed,
      deploys = deploys,
      deployToChanMap = deployToChanMap
    )

}

object ConflictDetectors {

  /**
    * There are 2 cases when the channel is considered conflicting.
    *
    *   1. If the same produce or consume is destroyed in COMM in both branches, this might be a race.
    *   All events created in event logs are unique, this match can be identified by comparing case classes.
    *
    *   Produce is considered destroyed in COMM if it is not persistent and been consumed without peek.
    *   Consume is considered destroyed in COMM when it is not persistent.
    *
    *   2. Events that are created inside branch and has not been destroyed in branch's COMMs
    *   can lead to potential COMM during merge.
    *   NOTE: this case requires special care about peek, as active event can be inherited from base state because
    *   of peek issue.
    *
    * @return set of channels that are conflicting.
    */
  def findConflicts[F[_]: Concurrent: Log](
      main: MergingBranchIndex[F],
      merge: MergingBranchIndex[F],
      // TODO remove baseDataReader, required only for peek workaround
      baseDataReader: LazyKeyValueCache[F, Blake2b256Hash, Seq[Datum[ListParWithRandom]]],
      baseJoinReader: LazyKeyValueCache[F, Blake2b256Hash, Seq[Seq[Par]]]
  ): F[Set[Blake2b256Hash]] = {

    // Check #1
    val biCommedConsume = merge.consumesDestroyed
      .intersect(main.consumesDestroyed)
    val biCommedProduce = merge.producesDestroyed
      .intersect(main.producesDestroyed)

    val racesForSameConsume =
      biCommedConsume.filterNot(_.persistent).flatMap(_.channelsHashes)
    val racesForSameProduce =
      biCommedProduce.filterNot(_.persistent).map(_.channelsHash)

    // Check #2
    val commOnMergeCheck = {
      // Check is performed by examining active produces of both branches against consumes of other branch
      def check(left: MergingBranchIndex[F], right: MergingBranchIndex[F]) = {
        val leftProdActive  = left.producesLinear.diff(left.producesDestroyed) ++ left.producesPersistent
        val rightConsActive = right.consumesLinearAndPeeks.diff(right.consumesDestroyed) ++ right.consumesPersistent

        leftProdActive.toList.map(
          p =>
            fs2.Stream
              .eval({
                // Find consume that waits for data on this channel in opposite branch.
                // Consume found can actually not match if it is a join, but for now we treat all such cases as conflicting.
                // TODO analyze joins to make less conflicts
                val matchingConsume =
                  rightConsActive.find(_.channelsHashes.contains(p.channelsHash))

                // As peek is not atomic, when Produce is peaked - exactly the same produce is created in event log.
                // Because of this event log cannot reliably tell whether produce is originated from base state on top
                // of which this event is created, of copied via peek.
                // We can do some tricks to check whether produce is created by a peek, but this logic is not clear yet.
                // So its safer to just read from base for now.
                val produceFromBase =
                  baseDataReader.get(p.channelsHash).map(_.exists(_.source == p))

                val commBetweenBranches = matchingConsume.nonEmpty.pure[F] &&^ produceFromBase.not

                // In addition, it has to be checked if active produces match some join that is inherited from base state
                val touchingBaseJoin = baseJoinReader.get(p.channelsHash).map(_.exists(_.size > 1))

                for {
                  conflict <- commBetweenBranches ||^ touchingBaseJoin
                } yield (p.channelsHash, conflict)
              })
              .filter {
                case (_, conflict) => conflict
              }
              .map(_._1)
        )
      }

      check(main, merge) ++ check(merge, main)
    }

    for {
      commsBetweenBranches <- fs2.Stream
                               .emits(commOnMergeCheck)
                               .parJoinUnbounded
                               .compile
                               .toList

      racesForSameEvent = racesForSameProduce ++ racesForSameConsume

      _ <- Log[F]
            .info(s"${racesForSameEvent.size} conflicts with base found")
            .whenA(racesForSameEvent.nonEmpty)
      _ <- Log[F]
            .info(s"${commsBetweenBranches.size} potential COMMs between branches found")
            .whenA(commsBetweenBranches.nonEmpty)
    } yield (commsBetweenBranches ++ racesForSameEvent).toSet
  }
}
