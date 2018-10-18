package coop.rchain.casper.util

import cats.{ApplicativeError, Eval, Monad}
import cats.implicits._
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.BlockDag
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.util.MapHelper.updatedWith
import coop.rchain.catscontrib.ListContrib
import coop.rchain.shared.StreamT

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, HashSet, Queue}
import scala.collection.mutable

object DagOperations {

  def bfTraverseF[F[_]: Monad, A](start: List[A])(neighbours: A => F[List[A]]): StreamT[F, A] = {
    def build(q: Queue[A], prevVisited: HashSet[A]): F[StreamT[F, A]] =
      if (q.isEmpty) StreamT.empty[F, A].pure[F]
      else {
        val (curr, rest) = q.dequeue
        if (prevVisited(curr)) build(rest, prevVisited)
        else
          for {
            ns      <- neighbours(curr)
            visited = prevVisited + curr
            newQ    = rest.enqueue[A](ns.filterNot(visited))
          } yield StreamT.cons(curr, Eval.always(build(newQ, visited)))
      }

    StreamT.delay(Eval.now(build(Queue.empty[A].enqueue[A](start), HashSet.empty[A])))
  }

  /**
    * Determines the ancestors to a set of blocks which are not common to all
    * blocks in the set. Each starting block is assigned an index (hence the
    * usage of IndexedSeq) and this is used to refer to that block in the result.
    * A block B is an ancestor of a starting block with index i if the BitSet for
    * B contains i.
    * @param blocks indexed sequence of blocks to determine uncommon ancestors of
    * @param lookup association of block hashes to data about the block (for DAG
    *               traversal)
    * @param topoSort topological sort of the DAG, ensures ancestor computation is
    *                 done correctly
    * @return A map from uncommon ancestor blocks to BitSets, where a block B is
    *         and ancestor of starting block with index i if B's BitSet contains i.
    */
  def uncommonAncestors(blocks: IndexedSeq[BlockMetadata], lookup: BlockMetadata.Lookup)(
      implicit topoSort: Ordering[BlockMetadata]
  ): Map[BlockMetadata, BitSet] = {
    val commonSet                                      = BitSet(0 until blocks.length: _*)
    def parents(b: BlockMetadata): List[BlockMetadata] = b.parents.map(lookup)
    def isCommon(set: BitSet): Boolean                 = set == commonSet

    val initMap = blocks.zipWithIndex.map { case (b, i) => b -> BitSet(i) }.toMap
    val q       = new mutable.PriorityQueue[BlockMetadata]()
    q.enqueue(blocks: _*)

    @tailrec
    def loop(
        currMap: Map[BlockMetadata, BitSet],
        enqueued: HashSet[BlockMetadata],
        uncommonEnqueued: Set[BlockMetadata]
    ): Map[BlockMetadata, BitSet] =
      if (uncommonEnqueued.isEmpty) currMap
      else {
        val currBlock = q.dequeue()
        //Note: The orElse case should never occur because we traverse in
        //      reverse topological order (i.e. down parent links)
        val currSet = currMap.getOrElse(currBlock, BitSet.empty)
        val (newMap, newEnqueued, newUncommon) = parents(currBlock).foldLeft(
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

        if (isCommon(currSet)) loop(newMap - currBlock, newEnqueued, newUncommon)
        else loop(newMap, newEnqueued, newUncommon)
      }

    loop(initMap, HashSet.empty[BlockMetadata], blocks.toSet).filter {
      case (_, set) => !isCommon(set)
    }
  }

  //Conceptually, the GCA is the first point at which the histories of b1 and b2 diverge.
  //Based on that, we compute by finding the first block from genesis for which there
  //exists a child of that block which is an ancestor of b1 or b2 but not both.
  def greatestCommonAncestorF[F[_]: Monad: BlockStore](
      b1: BlockMessage,
      b2: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[BlockMessage] =
    if (b1 == b2) {
      b1.pure[F]
    } else {
      def commonAncestorChild(
          b: BlockMessage,
          commonAncestors: Set[BlockMessage]
      ): F[List[BlockMessage]] = {
        val childrenHashes = dag.childMap.getOrElse(b.blockHash, HashSet.empty[BlockHash])
        for {
          children               <- childrenHashes.toList.traverse(ProtoUtil.unsafeGetBlock[F])
          commonAncestorChildren = children.filter(commonAncestors)
        } yield commonAncestorChildren
      }

      for {
        b1Ancestors     <- bfTraverseF[F, BlockMessage](List(b1))(ProtoUtil.unsafeGetParents[F]).toSet
        b2Ancestors     <- bfTraverseF[F, BlockMessage](List(b2))(ProtoUtil.unsafeGetParents[F]).toSet
        commonAncestors = b1Ancestors.intersect(b2Ancestors)
        gca <- bfTraverseF[F, BlockMessage](List(genesis))(commonAncestorChild(_, commonAncestors))
                .findF(
                  b =>
                    dag.childMap
                      .getOrElse(b.blockHash, HashSet.empty[BlockHash])
                      .toList
                      .existsM(
                        hash =>
                          for {
                            c <- ProtoUtil.unsafeGetBlock[F](hash)
                          } yield b1Ancestors(c) ^ b2Ancestors(c)
                      )
                )
      } yield gca.get
    }
}
