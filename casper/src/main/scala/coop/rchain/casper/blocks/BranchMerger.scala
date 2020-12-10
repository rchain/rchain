package coop.rchain.casper.blocks

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.EstimatorHelper
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.state.DAGReader
import coop.rchain.shared.syntax._
import coop.rchain.state.DAGReader._

import scala.collection.Seq

object BranchMerger {

  final case class MergingVertex(
      blockHash: BlockHash = ByteString.EMPTY,
      stateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  )

  final case class MergingBranch(
      finalStateHash: StateHash,
      vertices: Seq[MergingVertex],
      deploysToReject: Seq[ProcessedDeploy] = Seq.empty
  )

  final class MergingDagReader[F[_]: Concurrent: BlockStore](hashDAGReader: DAGReader[F, BlockHash])
      extends DAGReader[F, MergingVertex] {
    override def children(vertex: MergingVertex): F[Option[Set[MergingVertex]]] =
      hashDAGReader.children(vertex.blockHash).flatMap {
        case None => none[Set[MergingVertex]].pure[F]
        case Some(children) =>
          BlockStore[F]
            .getUnsafe(children.toSeq)
            .compile
            .toList
            .map(
              _.map(b => MergingVertex(b.blockHash, b.body.state.postStateHash, b.body.deploys)).toSet.some
            )
      }

    override def parents(vertex: MergingVertex): F[Option[Set[MergingVertex]]] =
      hashDAGReader.parents(vertex.blockHash).flatMap {
        case None => none[Set[MergingVertex]].pure[F]
        case Some(children) =>
          BlockStore[F]
            .getUnsafe(children.toSeq)
            .compile
            .toList
            .map(
              _.map(b => MergingVertex(b.blockHash, b.body.state.postStateHash, b.body.deploys)).toSet.some
            )
      }
  }

  /**
    * Merge two branches
    */
  def merge[F[_]: Concurrent: Log](
      mainBranch: MergingBranch,
      mergingBranch: MergingBranch,
      base: MergingVertex
  )(implicit runtimeManager: RuntimeManager[F]) = {
    import coop.rchain.rholang.interpreter.storage._
    for {
      changes <- Stopwatch.time(Log[F].info(_))(
                  s"COMPUTE MERGE CHANGES: main deploys ${mainBranch.vertices
                    .flatMap(_.processedDeploys)
                    .size}, merging deploys ${mergingBranch.vertices.flatMap(_.processedDeploys).size}"
                )(
                  EstimatorHelper.computeMergeChanges(
                    runtimeManager.getHistoryRepo,
                    Blake2b256Hash.fromByteString(base.stateHash),
                    mainBranch.vertices.flatMap(_.processedDeploys).toList,
                    mergingBranch.vertices.flatMap(_.processedDeploys).toList
                  )
                )
      mergeStateHash <- Stopwatch.time(Log[F].info(_))(
                         s"MERGE TWO BRANCHES, events: ${changes.validEventLogs.size}"
                       )(
                         runtimeManager.getHistoryRepo.stateMerger.flatMap(
                           _.merge(
                             Blake2b256Hash.fromByteString(base.stateHash),
                             Blake2b256Hash.fromByteString(mainBranch.finalStateHash),
                             Blake2b256Hash.fromByteString(mergingBranch.finalStateHash),
                             changes.validEventLogs.toList
                           )
                         )
                       )
      r = MergingBranch(
        mergeStateHash.toByteString,
        mainBranch.vertices ++ mergingBranch.vertices,
        mainBranch.deploysToReject ++ changes.rejectedDeploys
      )
    } yield r
  }

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
      dag: DAGReader[F, MergingVertex]
  ): F[(StateHash, Seq[ProcessedDeploy])] = {
    val computeBranches: F[Seq[MergingBranch]] =
      for {
        // compute branches in parallel
        branches <- fs2.Stream
                     .emits(tips.map { top =>
                       fs2.Stream.eval(computeBranch(top, base, dag))
                     })
                     .parJoinUnbounded
                     .compile
                     .toList
                     .map(_.map(b => MergingBranch(b._1.stateHash, b._2)))
        // sort branches by number of blocks contained
        sorted = branches.sortBy(
          b => (b.vertices.size, Base16.encode(b.finalStateHash.toByteArray))
        )
        // deduplicate branches content
        // check the biggest branch and remove from remaining ones blocks that the biggest branch contains.
        // go to the second biggest, repeat.
        (_, nonIntersectingBranches) = sorted.foldLeft(
          // initial list of branches and result list of deduplicated branches
          (sorted, Seq.empty[MergingBranch])
        ) {
          case ((biggestBranch :: remainder, result), _) => {
            // remove from remainder branches blocks that are also in the biggestBranch
            val filteredRemainder = remainder.map { b =>
              MergingBranch(b.finalStateHash, b.vertices.filterNot(biggestBranch.vertices.contains))
            }
            (filteredRemainder, result :+ biggestBranch)
          }
        }
      } yield nonIntersectingBranches

    for {
      branches <- computeBranches
      r        <- branches.tail.toList.foldM(branches.head)((main, merging) => merge(main, merging, base))
//      r <- branches.tailRecM[F, MergingBranch] {
//            // recursion in the end outputs a single branch, which is a merge result
//            case result :: Nil => result.asRight[Seq[MergingBranch]].pure[F]
//            // intermediate result, on each go N branches turn into N/2 (plus leftover option, of num of branches is odd)
//            case b => {
//              val mergingPairsSize = (b.size / 2)
//              val chunks           = b.sliding(mergingPairsSize, mergingPairsSize).toList
//              val (mergePairs, leftover) =
//                (chunks.head zip chunks(1), chunks.get(3).map(_.head))
//
//              val mergeProcesses = mergePairs.map {
//                case (main, merging) => fs2.Stream.eval(merge(main, merging, base))
//              }
//              for {
//                results <- fs2.Stream.emits(mergeProcesses).parJoinProcBounded.compile.toList
//              } yield (results ++ leftover).asLeft[MergingBranch]
//            }
//          }
    } yield (r.finalStateHash, r.deploysToReject)
  }
}
