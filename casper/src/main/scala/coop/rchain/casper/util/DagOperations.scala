package coop.rchain.casper.util

import cats.{Eval, Monad}
import cats.implicits._
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.util.MapHelper.updatedWith
import coop.rchain.catscontrib.ListContrib
import coop.rchain.models.BlockMetadata
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
    * @param dag the DAG
    * @param topoSort topological sort of the DAG, ensures ancestor computation is
    *                 done correctly
    * @return A map from uncommon ancestor blocks to BitSets, where a block B is
    *         and ancestor of starting block with index i if B's BitSet contains i.
    */
  def uncommonAncestors[F[_]: Monad](
      blocks: IndexedSeq[BlockMetadata],
      dag: BlockDagRepresentation[F]
  )(
      implicit topoSort: Ordering[BlockMetadata]
  ): F[Map[BlockMetadata, BitSet]] = {
    val commonSet = BitSet(0 until blocks.length: _*)
    def parents(b: BlockMetadata): F[List[BlockMetadata]] =
      b.parents.traverse(b => dag.lookup(b).map(_.get))
    def isCommon(set: BitSet): Boolean = set == commonSet

    val initMap = blocks.zipWithIndex.map { case (b, i) => b -> BitSet(i) }.toMap
    val q       = new mutable.PriorityQueue[BlockMetadata]()
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
    * Conceptually, the GCA is the first point at which the histories of b1 and b2 diverge.
    * We compute by finding the first block from genesis for which there exists a child of that block which is an ancestor of b1 or b2 but not both.
    *
    * TODO: Implement a GCA that doesn't require the genesis block (see https://rchain.atlassian.net/browse/RCHAIN-3002)
    * TODO: Remove usage of BlockStore by just using BlockDagRepresentation (see https://rchain.atlassian.net/browse/RCHAIN-3003)
    */
  def greatestCommonAncestorF[F[_]: Monad](
      b1: BlockMetadata,
      b2: BlockMetadata,
      genesis: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[BlockMetadata] =
    if (b1 == b2) {
      b1.pure[F]
    } else {
      def commonAncestorChild(
          b: BlockMetadata,
          commonAncestors: Set[BlockMetadata]
      ): F[List[BlockMetadata]] =
        for {
          childrenHashesOpt      <- dag.children(b.blockHash)
          childrenHashes         = childrenHashesOpt.getOrElse(Set.empty[BlockHash]).toList
          children               <- childrenHashes.traverse(dag.lookup).map(r => r.flatten.toSet)
          commonAncestorChildren = children.intersect(commonAncestors)
        } yield commonAncestorChildren.toList

      for {
        b1Ancestors <- bfTraverseF[F, BlockMetadata](List(b1))(
                        bm => bm.parents.traverse(dag.lookup).map(r => r.flatten)
                      ).toSet
        b2Ancestors <- bfTraverseF[F, BlockMetadata](List(b2))(
                        bm => bm.parents.traverse(dag.lookup).map(r => r.flatten)
                      ).toSet
        commonAncestors = b1Ancestors.intersect(b2Ancestors)
        gca <- bfTraverseF[F, BlockMetadata](List(genesis))(commonAncestorChild(_, commonAncestors))
                .findF(
                  b =>
                    for {
                      childrenOpt <- dag.children(b.blockHash)
                      children    = childrenOpt.getOrElse(Set.empty[BlockHash]).toList
                      result <- children.existsM(
                                 hash =>
                                   for {
                                     co <- dag.lookup(hash)
                                   } yield {
                                     if (co.isEmpty) false
                                     else {
                                       val c = co.get
                                       b1Ancestors(c) ^ b2Ancestors(c)
                                     }
                                   }
                               )
                    } yield result
                )
      } yield gca.get
    }
}
