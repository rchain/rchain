package coop.rchain.casper.blocks.merger

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.{MergingMetricsSource, PrettyPrinter}
import coop.rchain.dag._
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.rspace.merger.instances.EventsIndexConflictDetectors
import coop.rchain.rspace.syntax._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.store.LazyKeyValueCache

import scala.collection.Seq

/**
  * Merger optimizing the process using blocks relation from DAG.
  * DAG is split into non intersecting branches, which are merged afterwards.
  */
object CasperDagMerger {
  val indexBranchesLabel: Source      = Metrics.Source(MergingMetricsSource, "index-branches")
  val calculateConflictsLabel: Source = Metrics.Source(MergingMetricsSource, "calculate-conflicts")
  val mergeBranchesLabel: Source      = Metrics.Source(MergingMetricsSource, "merge-branches")

  /** branch having start state, end state and index for performant merging */
  final case class IndexedBranch(
      startState: ByteString,
      endState: ByteString,
      index: BranchIndex
  )

  /** conflict resolution for a sequence of branches */
  final case class ConflictResolution(
      mainBranch: IndexedBranch,
      mergingBranches: Seq[IndexedBranch],
      channelsConflicting: Set[Blake2b256Hash],
      deploysToReject: Set[ProcessedDeploy]
  )

  /**
    * Merge DAG.
    * @param tips - top blocks of the DAG
    * @param base - bottom blocks of the DAG, all tips should be connected with base
    * @param dag - connection graph
    * @return tuple space state hash of the merged state and list of conflicting deploys to reject
    */
  def merge[F[_]: Concurrent: RuntimeManager: Log: Span: Metrics](
      tips: Seq[MergingVertex],
      base: MergingVertex,
      dag: DagReader[F, MergingVertex],
      blockIndexCache: LazyKeyValueCache[F, MergingVertex, BlockIndex]
  ): F[(StateHash, Seq[ProcessedDeploy])] = {

    // ordering is required to be able to replay computing branches procedure
    // there is no ordering for Seq[Byte] in standard lib, so conversion toSeq with implicits below is required
    import scala.math.Ordered.orderingToOrdered
    import scala.math.Ordering.Implicits.seqDerivedOrdering
    implicit val ord: Ordering[MergingVertex] = (x: MergingVertex, y: MergingVertex) =>
      x.postStateHash.toByteArray.toSeq.compare(y.postStateHash.toByteArray.toSeq)

    val historyRepo = RuntimeManager[F].getHistoryRepo

    for {
      // compute branches that can be merged
      branchesSorted <- DagOps.computeSortedDistinctBranches(tips, base, dag)
      // compute branch indexes, preserving order
      _ <- Log[F].debug(s"Attempting to merge ${branchesSorted.size} branches. Indexing...")
      branchesIndexed <- Span[F].trace(indexBranchesLabel)(
                          Stopwatch.time(Log[F].info(_))(s"Indexing done.")(
                            fs2.Stream
                              .emits(branchesSorted)
                              .parEvalMapProcBounded { b =>
                                implicit val c = blockIndexCache
                                Indexer.createBranchIndex(b).map { i =>
                                  IndexedBranch(b.last.preStateHash, b.head.postStateHash, i)
                                }
                              }
                              .compile
                              .toList
                          )
                        )

      // reader for base state
      baseReader = historyRepo.getHistoryReader(Blake2b256Hash.fromByteString(base.postStateHash))
      // caching readers for data in base state
      baseDataReader <- LazyKeyValueCache(
                         (ch: Blake2b256Hash) => baseReader.getDataFromChannelHash(ch)(historyRepo)
                       )
      baseJoinsReader <- LazyKeyValueCache(
                          (ch: Blake2b256Hash) =>
                            baseReader.getJoinsFromChannelHash(ch)(historyRepo)
                        )

      _ <- Log[F].debug(s"Calculating conflicts for branches size of (events): ${branchesIndexed
            .map(_.index.deploys.map(_.deployLog).size)
            .mkString(";")}")

      // Calculate conflicting channels
      // 1. Biggest branch is set fixed and remaining are checked against it, accumulating deploys that conflict
      // 2. Biggest branch ejected into results
      // 3. Repeat
      // TODO Now deploys are not removed immediately, so this can cause unnecessary conflicts.
      // TODO More complex logic can be applied to make less conflicts
      r <- Span[F].trace(calculateConflictsLabel)(
            Stopwatch.time(Log[F].info(_))(s"Calculating conflicts done.")(
              branchesIndexed
                .foldLeftM(
                  (branchesIndexed, Set.empty[(Set[Blake2b256Hash], Set[ProcessedDeploy])])
                ) {
                  case ((biggestBranch :: remainder, result), _) =>
                    val mainEventsIndex = biggestBranch.index.eventLogIndex

                    // streams for calculating conflicts between biggest branch and remainder
                    val conflictDetectors = remainder.map {
                      case IndexedBranch(_, _, mergingIndex) =>
                        val mergingEventsIndex = mergingIndex.eventLogIndex
                        fs2.Stream.eval(
                          for {
                            conflictingChannels <- EventsIndexConflictDetectors
                                                    .findConflicts(
                                                      mainEventsIndex,
                                                      mergingEventsIndex,
                                                      baseDataReader,
                                                      baseJoinsReader
                                                    )
                            deploysToReject = mergingIndex.calculateRejectedDeploys(
                              conflictingChannels
                            )
                          } yield (
                            conflictingChannels,
                            deploysToReject
                          )
                        )
                    }

                    for {
                      // execute streams, find conflicts
                      conflicting <- fs2.Stream
                                      .emits(conflictDetectors)
                                      .parJoinProcBounded
                                      .compile
                                      .toList
                                      .map(_.toSet)
                    } yield (remainder, result ++ conflicting)
                }
                .map(
                  v =>
                    ConflictResolution(
                      branchesIndexed.head,
                      branchesIndexed.tail,
                      v._2.flatMap(_._1),
                      v._2.flatMap(_._2)
                    )
                )
            )
          )

      // main amd merging branch
      main     = r.mainBranch
      mergings = r.mergingBranches
      // deploys to reject
      toReject = r.deploysToReject

      _ <- Log[F].debug(s"Rejecting ${toReject.size} deploys.").whenA(toReject.nonEmpty)

      // event chains to merge, with events from conflicting deploys removed
      toMerge <- mergings.toList.traverse {
                  case IndexedBranch(startState, endState, eventLog) =>
                    for {
                      startHistory <- historyRepo
                                       .reset(Blake2b256Hash.fromByteString(startState))
                                       .map(_.history)
                      endHistory <- historyRepo
                                     .reset(Blake2b256Hash.fromByteString(endState))
                                     .map(_.history)
                    } yield EventChain(
                      startHistory,
                      endHistory,
                      // TODO possible to use EventLogIndex here to make less compute
                      eventLog.deploys
                        .filterNot(d => toReject.exists(_.deploy.sig == d.deploy.sig))
                        .flatMap(_.deployLog)
                        .toVector
                        .map(EventConverter.toRspaceEvent)
                    )
                }

      _ <- Log[F].debug(s"Merging {${toMerge
            .map(m => PrettyPrinter.buildString(m.endState.root.toByteString) + s" (${m.events.size})")
            .mkString("; ")}} into ${PrettyPrinter.buildString(main.endState)} (${main.index.deploys
            .flatMap(_.deployLog)
            .size}), base ${PrettyPrinter.buildString(base.postStateHash)}")

      // execute merge
      mainHistory <- historyRepo.reset(Blake2b256Hash.fromByteString(main.endState)).map(_.history)
      resultHash <- Span[F].trace(mergeBranchesLabel)(
                     Stopwatch.time(Log[F].info(_))(s"Merging done.")(
                       historyRepo.stateMerger.merge(mainHistory, toMerge)
                     )
                   )

    } yield (resultHash.root.toByteString, toReject.toVector)
  }
}

// detect conflicts across all branches with map reduce
//      toReject <- Stopwatch.time(Log[F].info(_))("CALCULATION DEPLOYS TO REJECT")(
//                   (branchesIndexed.map(_._2), Set.empty[ProcessedDeploy])
//                     .tailRecM[F, Set[ProcessedDeploy]] {
//                       case (_ :: Nil, r) =>
//                         r.asRight[(List[MergingBranchIndex[F]], Set[ProcessedDeploy])].pure[F]
//                       // intermediate result, on each go N branches turn into N/2 (plus leftover option,
//                       // if num of branches is odd)
//                       case (branches, acc) => {
//                         val pairsSize = (branches.size / 2)
//                         val chunks    = branches.sliding(pairsSize, pairsSize).toList
//                         val (pairs, leftover) =
//                           (chunks.head zip chunks(1), chunks.get(3).map(_.head))
//
//                         val workers = pairs.map {
//                           case (main, merging) =>
//                             fs2.Stream.eval(for {
//                               conflictingChans <- ConflictDetectors.findConflics(
//                                                    main,
//                                                    merging,
//                                                    baseDataReader,
//                                                    baseContReader
//                                                  )
//                               rejectedDeploys = merging.calculateRejected(conflictingChans)
//                             } yield (main + merging, rejectedDeploys))
//                         }
//
//                         for {
//                           results <- fs2.Stream.emits(workers).parJoinUnbounded.compile.toList
//                         } yield (
//                           (
//                             results.map(_._1) ++ leftover,
//                             acc ++ results.flatMap(_._2)
//                           )
//                         ).asLeft[Set[ProcessedDeploy]]
//                       }
//                     }
