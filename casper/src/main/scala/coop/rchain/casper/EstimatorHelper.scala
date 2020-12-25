package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.protocol.Event
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{DagOperations, EventConverter}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace.{COMM, Consume, EventGroup, Produce, Event => RSpaceEvent}
import coop.rchain.rspace.trace.Event.{
  allChannels,
  containConflictingEvents,
  extractJoinedChannels,
  extractRSpaceEventGroup,
  Conflict,
  NonConflict
}
import coop.rchain.shared.{Log, Serialize}

import scala.collection.BitSet

object EstimatorHelper {

  private[this] val ChooseNonConflictingMetricsSource =
    Metrics.Source(CasperMetricsSource, "choose-non-conflicting")

  def chooseNonConflicting[F[_]: Sync: Log: BlockStore](
      blockHashes: Seq[BlockHash],
      dag: BlockDagRepresentation[F]
  )(implicit spanF: Span[F]): F[Seq[BlockMetadata]] =
    spanF.trace(ChooseNonConflictingMetricsSource) {
      def nonConflicting(b: BlockMetadata): BlockMetadata => F[Boolean] =
        conflicts[F](_, b, dag).map(b => !b)

      for {
        blocks <- blockHashes.toList.traverse(dag.lookupUnsafe)
        result <- blocks
                   .foldM(List.empty[BlockMetadata]) {
                     case (acc, b) =>
                       acc
                         .forallM(nonConflicting(b))
                         .ifM(
                           (b :: acc).pure[F],
                           acc.pure[F]
                         )
                   }
                   .map(_.reverse)
      } yield result
    }

  private[casper] def conflicts[F[_]: Sync: Log: BlockStore](
      b1: BlockMetadata,
      b2: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    for {
      uncommonAncestorsMap             <- DagOperations.uncommonAncestors[F](Vector(b1, b2), dag)
      (b1AncestorsMap, b2AncestorsMap) = uncommonAncestorsMap.partition(_._2 == BitSet(0))
      b1Events                         <- extractBlockEvents[F](b1AncestorsMap.keys.toList)
      b2Events                         <- extractBlockEvents[F](b2AncestorsMap.keys.toList)
      conflictsBecauseOfJoins = extractJoinedChannels(b1Events)
        .intersect(allChannels(b2Events))
        .nonEmpty || extractJoinedChannels(b2Events).intersect(allChannels(b1Events)).nonEmpty
      conflicts = conflictsBecauseOfJoins || containConflictingEvents(b1Events, b2Events).nonEmpty
      _ <- if (conflicts) {
            Log[F].info(
              s"Blocks ${PrettyPrinter.buildString(b1.blockHash)} and ${PrettyPrinter
                .buildString(b2.blockHash)} conflict."
            )
          } else {
            Log[F].info(
              s"Blocks ${PrettyPrinter
                .buildString(b1.blockHash)} and ${PrettyPrinter
                .buildString(b2.blockHash)} don't conflict."
            )
          }
    } yield conflicts

  final case class MergeChanges(
      validDeploys: List[ProcessedDeploy],
      rejectedDeploys: List[ProcessedDeploy],
      validEventLogs: Seq[RSpaceEvent]
  )

  /**
    * When merging two blocks in casper, you can suppose the minor case like below.
    *
    *       mergedBlock
    *       /        \
    * mainBlock    mergingBlock
    *       \       /
    *       baseBlock
    *
    *  The `baseState` should be the `postState` of the baseBlock which the outcome of baseBlock and the state should
    *  be the start of mainBlock and mergingBlock.
    *
    *  The `mainDeploys` are all the deploys in the `mainBlock` and the mergingDeploys are all the
    *  deploys in `mergingBlock`.
    *
    *  The minor case can be extended like below.
    *
    *          mergedBlock
    *       /             \
    *     b1              mergingBlock
    *     |               |
    *     b2              |
    *       \            /
    *          baseBlock
    *
    *  The `mainDeploys` should be all the deploys in `b1` and `b2`.
    *
    *  The function would return conflict detection on these deploys between main side and merging side.
    *
    * @param historyRepo [[coop.rchain.rspace.history.HistoryRepository]] which is used for compute mergeChanges
    * @param baseState
    * @param mainDeploys
    * @param mergingDeploys
    * @param sc
    * @return
    */
  def computeMergeChanges[F[_]: Concurrent, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K],
      baseState: Blake2b256Hash,
      mainDeploys: List[ProcessedDeploy],
      mergingDeploys: List[ProcessedDeploy]
  ): F[MergeChanges] = {

    // errored deploy is always non-conflict
    val successDeploysMain  = mainDeploys.filterNot(_.isFailed)
    val successDeploysMerge = mergingDeploys.filterNot(_.isFailed)

    // free IOEvents are produces and consumes that are not part of any COMM event, or they are persistent
    def getFreeIOEvents(d: ProcessedDeploy) = {
      val (prod, cons, comms) = d.deployLog
        .map(EventConverter.toRspaceEvent)
        .foldLeft((Seq.empty[Produce], Seq.empty[Consume], Seq.empty[COMM])) { (op, event) =>
          event match {
            case p: Produce => (op._1 :+ p, op._2, op._3)
            case c: Consume => (op._1, op._2 :+ c, op._3)
            case c: COMM    => (op._1, op._2, op._3 :+ c)
          }
        }
      val freeProd = prod.filter(p => p.persistent || !comms.exists(_.produces.contains(p)))
      val freeCons = cons.filter(c => c.persistent || !comms.exists(_.consume == c))
      ((freeProd ++ freeCons))
    }

    val freeIOEventsMain  = successDeploysMain.map(getFreeIOEvents).reduce(_ ++ _)
    val freeIOEventsMerge = successDeploysMerge.map(getFreeIOEvents).reduce(_ ++ _)

    historyRepo
      .simpleConflictDetector(
        baseState,
        freeIOEventsMain,
        freeIOEventsMerge
      )
      .map {
        case Seq() =>
          MergeChanges(
            mergingDeploys,
            List.empty[ProcessedDeploy],
            extractEventGroup(mergingDeploys.flatMap(_.deployLog)).events
          )
        case conflicts => {
          val (conflictDeploys, nonConflictDeploys) = mergingDeploys.partition(
            d =>
              d.deployLog.forall(
                e =>
                  EventConverter.toRspaceEvent(e) match {
                    case Produce(channelsHash, _, _)   => conflicts.contains(channelsHash)
                    case Consume(channelsHashes, _, _) => channelsHashes.exists(conflicts.contains)
                    case COMM(_, _, _, _)              => true
                  }
              )
          )
          MergeChanges(
            nonConflictDeploys,
            conflictDeploys,
            extractEventGroup(nonConflictDeploys.flatMap(_.deployLog)).events
          )
        }
      }
  }

  private[this] def extractBlockEvents[F[_]: Sync: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[EventGroup] =
    for {
      ancestors <- blockAncestorsMeta.traverse(
                    blockAncestorMeta => BlockStore[F].getUnsafe(blockAncestorMeta.blockHash)
                  )
      events = ancestors
        .flatMap(_.body.deploys.flatMap(_.deployLog))
      eventGroup = extractEventGroup(events)
    } yield eventGroup

  /**
    * @return EventGroup contains all the events which is not volatile in the scope
    */
  private[this] def extractEventGroup(events: List[Event]): EventGroup = {
    val rspaceEvents = events
      .map(EventConverter.toRspaceEvent)
    extractRSpaceEventGroup(rspaceEvents)
  }

}
