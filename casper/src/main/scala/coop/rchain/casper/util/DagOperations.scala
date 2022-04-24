package coop.rchain.casper.util

import cats.Monad
import cats.implicits._
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.models.BlockMetadata
import coop.rchain.blockstorage.syntax._

import scala.collection.immutable.{BitSet, HashSet, SortedSet}
import scala.collection.mutable

object DagOperations {

  /**
    * Determines the ancestors to a set of blocks which are not common to all
    * blocks in the set. Each starting block is assigned an index (hence the
    * usage of IndexedSeq) and this is used to refer to that block in the result.
    * A block B is an ancestor of a starting block with index i if the BitSet for
    * B contains i.
    * @param blocks indexed sequence of blocks to determine uncommon ancestors of
    * @param dag the DAG
    * @return A map from uncommon ancestor blocks to BitSets, where a block B is
    *         and ancestor of starting block with index i if B's BitSet contains i.
    */
  def uncommonAncestors[F[_]: Monad: BlockDagStorage](
      blocks: IndexedSeq[BlockMetadata],
      dag: DagRepresentation
  ): F[Map[BlockMetadata, BitSet]] = {
    val commonSet = BitSet(0 until blocks.length: _*)
    def parents(b: BlockMetadata): F[List[BlockMetadata]] =
      b.parents.traverse(b => dag.lookup(b).map(_.get))
    def isCommon(set: BitSet): Boolean = set == commonSet

    val initMap = blocks.zipWithIndex.map { case (b, i) => b -> BitSet(i) }.toMap
    val q       = new mutable.PriorityQueue[BlockMetadata]()(BlockMetadata.orderingByNum)
    q.enqueue(blocks: _*)

    def loop(
        currMap: Map[BlockMetadata, BitSet],
        enqueued: HashSet[BlockMetadata],
        uncommonEnqueued: Set[BlockMetadata]
    ): F[Map[BlockMetadata, BitSet]] =
      if (uncommonEnqueued.isEmpty) currMap.pure[F]
      else {
        val currBlock = q.dequeue()
        //Note: The orElse case should never occur because we traverse in
        //      reverse topological order (i.e. down parent links)
        val currSet = currMap.getOrElse(currBlock, BitSet.empty)
        for {
          currParents <- parents(currBlock)
          (newMap, newEnqueued, newUncommon) = currParents.foldLeft(
            (currMap, enqueued - currBlock, uncommonEnqueued - currBlock)
          ) {
            case ((map, enq, unc), p) =>
              if (!enq(p)) q.enqueue(p)
              val pSet = map.getOrElse(p, BitSet.empty) | currSet
              val newUnc =
                if (isCommon(pSet)) unc - p
                else unc + p
              (map.updated(p, pSet), enq + p, newUnc)
          }
          result <- if (isCommon(currSet)) loop(newMap - currBlock, newEnqueued, newUncommon)
                   else loop(newMap, newEnqueued, newUncommon)
        } yield result
      }

    loop(initMap, HashSet.empty[BlockMetadata], blocks.toSet).map(_.filter {
      case (_, set) => !isCommon(set)
    })
  }

  /**
    * Conceptually, the LUCA is the lowest point at which the histories of b1 and b2 diverge.
    * We compute by finding the first block that is the "lowest" (has highest blocknum) block common
    * for both blocks' ancestors.
    */
  def lowestUniversalCommonAncestorF[F[_]: Monad: BlockDagStorage](
      b1: BlockMetadata,
      b2: BlockMetadata,
      dag: DagRepresentation
  ): F[BlockMetadata] = {
    def getParents(p: BlockMetadata): F[Set[BlockMetadata]] =
      p.parents.traverse(dag.lookup(_)).map(_.toSet.flatten)

    def extractParentsFromHighestNumBlock(
        blocks: SortedSet[BlockMetadata]
    ): F[SortedSet[BlockMetadata]] = {
      val (head, tail) = (blocks.head, blocks.tail)
      getParents(head).map(newBlocks => tail ++ newBlocks)
    }

    if (b1 == b2) {
      b1.pure[F]
    } else {
      val start = SortedSet.empty[BlockMetadata](BlockMetadata.orderingByNum.reverse) + b1 + b2
      Monad[F]
        .iterateWhileM(start)(extractParentsFromHighestNumBlock)(
          _.size != 1
        )
        .map(_.head)
    }
  }
}
