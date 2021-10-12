package coop.rchain.casper.deploychainsetcasper

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.deploychainsetcasper.DeployChainSetConflictResolver.DeployChainWithIndex
import coop.rchain.casper.merging.DeployChainIndex
import coop.rchain.casper.protocol.DeployChain
import coop.rchain.v2.casper.stcasper.ConflictsResolver
import coop.rchain.v2.casper.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.rspace.merger.MergingLogic
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.shared.{Log, Stopwatch}

final case class DeployChainSetConflictResolver[F[_]: Sync](
    index: DeployChain => F[DeployChainIndex]
) extends ConflictsResolver[F, DeployChain] {

  val minCostRejectionF: Set[DeployChainWithIndex] => Long = _.map(_.index).map(_.cost).sum

  implicit val ordBytes                          = Ordering.by((_: ByteString).toByteArray.toIterable).reverse
  implicit val o: Ordering[DeployChainWithIndex] = Ordering.by(_.deployChain.deploys.head)

  /** Conflict resolution */
  private def areConflicting(
      as: Set[DeployChainWithIndex],
      bs: Set[DeployChainWithIndex]
  ): Boolean =
    (as.flatMap(_.deployChain.deploys) intersect bs.flatMap(_.deployChain.deploys)).nonEmpty ||
      MergingLogic.areConflicting(
        as.map(_.index.eventLogIndex).toList.combineAll,
        bs.map(_.index.eventLogIndex).toList.combineAll
      )

  /** compute optimal rejection configuration */
  private def optimalRejection(
      options: Set[Set[Set[DeployChainWithIndex]]],
      targetF: Set[DeployChainWithIndex] => Long
  ): Set[Set[DeployChainWithIndex]] = {
    require(
      options.map(_.map(_.head)).size == options.size,
      "Same rejection unit is found in two rejection options. Please report this to code maintainer."
    )
    options.toList
    // reject set with min sum of target function output,
    // if equal value - min size of a branch,
    // if equal size - sorted by head of rejection set option
      .sortBy(b => (b.map(targetF).sum, b.size, b.head.head))
      .headOption
      .getOrElse(Set.empty[Set[DeployChainWithIndex]])
  }

  def resolve(conflictSet: Set[DeployChain]): F[ConflictResolution[DeployChain]] = {
    implicit val log = Log.log
    for {
      withIndex <- conflictSet.toList.traverse(cs => index(cs).map(DeployChainWithIndex(cs, _)))

      // split merging set into branches without cross dependencies
      // TODO make dependencies directional, maintain dependency graph. Now if l depends on r or otherwise - it does not matter.*/
      (branches, branchesTime) = Stopwatch.profile(
        computeRelatedSets[DeployChainWithIndex](
          withIndex.toSet,
          (l, r) => depends(l.index.eventLogIndex, r.index.eventLogIndex)
        )
      )

      // map of conflicting branches */
      (conflictMap, conflictsMapTime) = Stopwatch.profile(
        computeRelationMap[Set[DeployChainWithIndex]](
          branches,
          (l, r) => areConflicting(l, r)
        )
      )

      // TODO reject only units that are conflicting + dependent, its not necessary to reject the whole branch
      // rejection options that leave only non conflicting branches */
      (rejectionOptions, rejOptionsTime) = Stopwatch.profile(
        computeRejectionOptions(conflictMap)
      )
      rejection = optimalRejection(rejectionOptions, minCostRejectionF)
      _ <- Log[F].info(
            s"Conflicts resolved (conflict set size ${conflictSet.size}). branches computed in $branchesTime, " +
              s"conflictsMap in $conflictsMapTime, rejection options in $rejOptionsTime."
          )
    } yield ConflictResolution(
      conflictSet -- rejection.flatMap(_.map(_.deployChain)),
      rejection.flatMap(_.map(_.deployChain))
    )
  }
}

object DeployChainSetConflictResolver {
  final case class DeployChainWithIndex(deployChain: DeployChain, index: DeployChainIndex)
}
