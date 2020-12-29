package coop.rchain.casper.blocks.merger

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.codec.Base16
import coop.rchain.dag._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.store.LazyKeyValueCache

import scala.collection.Seq

object CasperDagMerger {

  /**
    * Merge DAG.
    * @param tips - top blocks of the DAG
    * @param base - bottom blocks of the DAG, all tips should be connected with base
    * @param dag - connection graph
    * @return tuplespace state hash of the merged state and list of conflicting deploys to reject
    */
  def merge[F[_]: Concurrent: RuntimeManager: Log](
      tips: Seq[MergingVertex],
      base: MergingVertex,
      dag: DagReader[F, MergingVertex]
  ): F[(StateHash, Seq[ProcessedDeploy])] = {

    // ordering is required for computing branches procedure to be replayable
    implicit val ord: Ordering[MergingVertex] = (x: MergingVertex, y: MergingVertex) =>
      Base16.encode(x.postStateHash.toByteArray).compare(Base16.encode(y.postStateHash.toByteArray))

    for {
      baseStateReader <- RuntimeManager[F].getHistoryRepo
                          .reset(Blake2b256Hash.fromByteString(base.postStateHash))
      branchesSorted <- DagOps.computeSortedDistinctBranches(tips, base, dag)
      // TODO extract syntax for streams to compute in parallel but preserve order
      branchesIndexed <- Stopwatch.time(Log[F].info(_))(
                          s"INDEXING BRANCHES: branches: ${branchesSorted.size}"
                        )(
                          fs2.Stream
                            .emits(
                              branchesSorted.zipWithIndex.map(
                                b =>
                                  fs2.Stream.eval(
                                    MergingBranchIndex
                                      .create(b._1)
                                      .map { i =>
                                        (b._1.head.postStateHash, b._1.last.preStateHash, i, b._2)
                                      }
                                  )
                              )
                            )
                            .parJoinUnbounded
                            .compile
                            .toList
                            .map(_.sortBy(_._4).map(v => (v._1, v._2, v._3)))
                        )

      // Caching readers for data in base state
      baseDataReader <- LazyKeyValueCache(
                         (ch: Blake2b256Hash) =>
                           baseStateReader
                             .getDataFromChannelHash(ch)
                       )
      baseJoinsReader <- LazyKeyValueCache(
                          (ch: Blake2b256Hash) =>
                            baseStateReader
                              .getJoinsFromChannelHash(ch)
                        )

      _ <- Log[F].info(
            s"trying to merge branches size of:  ${branchesIndexed.map(_._3.deploys.map(_.deployLog).size).mkString(";")}"
          )
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
      v <- branchesIndexed.foldLeftM(
            (branchesIndexed, Set.empty[(Set[Blake2b256Hash], Set[ProcessedDeploy])])
          ) {
            case ((biggestBranch :: remainder, result), _) =>
              for {
                conflicting <- fs2.Stream
                                .emits(remainder.map { b =>
                                  fs2.Stream.eval(
                                    for {
                                      conflictingChans <- ConflictDetectors.findConflicts(
                                                           biggestBranch._3,
                                                           b._3,
                                                           baseDataReader,
                                                           baseJoinsReader
                                                         )
                                    } yield (
                                      conflictingChans,
                                      b._3.calculateRejected(conflictingChans)
                                    )
                                  )
                                })
                                .parJoinUnbounded
                                .compile
                                .toList
                                .map(_.toSet)

              } yield (remainder, result ++ conflicting)
          }
      toReject    = v._2.flatMap(_._2)
      conflicting = v._2.flatMap(_._1)

      _ <- Log[F].info(s"rejecting ${toReject.size} deploys").whenA(toReject.nonEmpty)

      // main branch
      main = branchesIndexed.head

      // merging branches, rejecting conflicting deploys
      toMerge = branchesIndexed.tail.map(
        m =>
          (
            EventChain(
              startState = Blake2b256Hash.fromByteString(m._2),
              endState = Blake2b256Hash.fromByteString(m._1),
              events = m._3.deploys
                .filterNot(d => toReject.exists(_.deploy.sig == d.deploy.sig))
                .flatMap(_.deployLog)
                .toVector
                .map(EventConverter.toRspaceEvent)
            )
          )
      )

      _ <- Log[F].info(
            s"Merging {${toMerge
              .map(m => PrettyPrinter.buildString(m.endState.toByteString) + s" (${m.events.size})")
              .mkString("; ")}} into ${PrettyPrinter.buildString(main._1)} (${main._3.vertices
              .flatMap(_.processedDeploys.flatMap(_.deployLog))
              .size}), base ${PrettyPrinter
              .buildString(base.postStateHash)}"
          )

      resultHash <- Stopwatch.time(Log[F].info(_))(
                     s"MERGE ALL BRANCHES"
                   )(
                     RuntimeManager[F].getHistoryRepo.stateMerger.flatMap(
                       _.merge(
                         Blake2b256Hash.fromByteString(main._1),
                         toMerge
                       )
                     )
                   )
    } yield (resultHash.toByteString, toReject.toVector)
  }
}
