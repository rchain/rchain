package coop.rchain.dag

import cats.effect.Concurrent
import cats.syntax.all._

import scala.collection.Seq

trait Conflictual

abstract class Mergeable[F[_], C <: Conflictual, T <: Mergeable[F, C, T]] {
  def removeConflicts(conflicts: Set[C]): F[T]
}

object DagMerger {

//  // Merge branches folding one by one.
//  // Conflict detection is performed for the next branch after each merge
//  def mergeInSeq[F[_]: Concurrent, C <: Conflictual, M <: Mergeable[F, C, M]](
//      branches: Seq[M],
//      conflictDetector: (M, M) => F[Set[C]],
//      merger: (M, M) => F[(M, Set[C])]
//  ): F[(M, Seq[C])] =
//    for {
//      r <- branches.tail.toList
//            .foldM((branches.head, Set.empty[C])) {
//              case ((main, conflictSet), merging) =>
//                for {
//                  conflicts <- conflictDetector(main, merging)
//                  _         <- merging.removeConflicts(conflicts)
//                  merged    <- merger(main, merging)
//                } yield (merged, conflictSet ++ conflicts)
//            }
//    } yield r
//
//  // First detect all conflicts across all branches, then merge all in one go
//  def mergeInBatch[F[_]: Concurrent, C <: Conflictual, M <: Mergeable[F, C, M]](
//      branches: Seq[M],
//      conflictDetector: (M, M) => F[Set[C]],
//      merger: (M, Seq[M]) => F[M]
//  ): F[(M, Seq[C])] =
//    for {
//      // detect conflicts across all branches
//      v <- branches.toList.foldLeftM(
//            (branches, Seq.empty[C])
//          ) {
//            case ((biggestBranch :: remainder, result), _) =>
//              for {
//                // remove from remainder branches blocks that are also in the biggestBranch
//                conflicting <- fs2.Stream
//                                .emits(remainder.map { b =>
//                                  fs2.Stream.eval(for {
//                                    conflictingChans <- conflictDetector(
//                                                         biggestBranch,
//                                                         b
//                                                       )
//                                  } yield conflictingChans)
//                                })
//                                .parJoinUnbounded
//                                .compile
//                                .toList
//                                .map(_.reduce(_ ++ _))
//              } yield (remainder, result :+ conflicting)
//          }
//      conflictSet = v._2.toSet
//      // remove all conflicts found
//      cleared <- branches.tail.toList.traverse(_.removeConflicts(conflictSet))
//      // merge all branches into one
//      r <- merger(branches.head, cleared)
//    } yield (r, conflictSet)
//
//  // TODO Map reduce merge
//  def mergeMapReduce: Unit = {
//    //      r <- branches.tailRecM[F, MergingBranch] {
//    //            // recursion in the end outputs a single branch, which is a merge result
//    //            case result :: Nil => result.asRight[Seq[MergingBranch]].pure[F]
//    //            // intermediate result, on each go N branches turn into N/2 (plus leftover option, of num of branches is odd)
//    //            case b => {
//    //              val mergingPairsSize = (b.size / 2)
//    //              val chunks           = b.sliding(mergingPairsSize, mergingPairsSize).toList
//    //              val (mergePairs, leftover) =
//    //                (chunks.head zip chunks(1), chunks.get(3).map(_.head))
//    //
//    //              val mergeProcesses = mergePairs.map {
//    //                case (main, merging) => fs2.Stream.eval(merge(main, merging, base))
//    //              }
//    //              for {
//    //                results <- fs2.Stream.emits(mergeProcesses).parJoinProcBounded.compile.toList
//    //              } yield (results ++ leftover).asLeft[MergingBranch]
//    //            }
//    //          }
//  }
}
