package coop.rchain.casper.util

import cats.{ApplicativeError, Eval, Monad}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.{BlockDag, BlockMetadata}
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

  def uncommonAncestors(blocks: IndexedSeq[BlockMetadata], lookup: BlockMetadata.Lookup)(
      implicit topoSort: Ordering[BlockMetadata]): Map[BlockMetadata, BitSet] = {
    def parents(b: BlockMetadata): List[BlockMetadata] = b.parents.map(lookup)
    def isCommon(set: BitSet): Boolean                 = (0 until blocks.length).forall(set.contains)

    val initMap = blocks.zipWithIndex.map { case (b, i) => b -> BitSet(i) }.toMap
    val q       = new mutable.PriorityQueue[BlockMetadata]()
    q.enqueue(blocks: _*)

    @tailrec
    def loop(currMap: Map[BlockMetadata, BitSet],
             visited: HashSet[BlockMetadata]): Map[BlockMetadata, BitSet] =
      if (q.isEmpty) currMap
      else {
        val currBlock = q.dequeue()
        val currSet   = currMap.getOrElse(currBlock, BitSet.empty)
        if (visited(currBlock) || isCommon(currSet)) {
          //already visited or is common ancestor
          loop(currMap, visited)
        } else {
          val (newMap, newVisted) = parents(currBlock).foldLeft(currMap -> visited) {
            case ((map, vstd), p) =>
              if (!vstd(p)) q.enqueue(p)
              updatedWith(map, p)(currSet)(_ | currSet) -> (vstd + p)
          }

          loop(newMap, newVisted)
        }
      }

    loop(initMap, HashSet.empty[BlockMetadata]).filter { case (_, set) => !isCommon(set) }
  }

  //Conceptually, the GCA is the first point at which the histories of b1 and b2 diverge.
  //Based on that, we compute by finding the first block from genesis for which there
  //exists a child of that block which is an ancestor of b1 or b2 but not both.
  def greatestCommonAncestorF[F[_]: Monad: BlockStore](b1: BlockMessage,
                                                       b2: BlockMessage,
                                                       genesis: BlockMessage,
                                                       dag: BlockDag): F[BlockMessage] =
    if (b1 == b2) {
      b1.pure[F]
    } else {
      def commonAncestorChild(b: BlockMessage,
                              commonAncestors: Set[BlockMessage]): F[List[BlockMessage]] = {
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
                      .existsM(hash =>
                        for {
                          c <- ProtoUtil.unsafeGetBlock[F](hash)
                        } yield b1Ancestors(c) ^ b2Ancestors(c)))
      } yield gca.get
    }
}
