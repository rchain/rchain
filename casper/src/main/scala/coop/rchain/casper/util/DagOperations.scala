package coop.rchain.casper.util

import cats.{ApplicativeError, Eval, Monad}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.BlockDag
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.catscontrib.ListContrib
import coop.rchain.shared.StreamT

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, Queue}
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

  //Conceptually, the GCA is the first point at which the histories of b1 and b2 diverge.
  //Based on that, we compute by finding the first block from genesis for which there
  //exists a child of that block which is an ancestor of b1 or b2 but not both.
  def greatestCommonAncestorF[F[_]: Monad: BlockStore](b1: BlockMessage.Safe,
                                                       b2: BlockMessage.Safe,
                                                       genesis: BlockMessage.Safe,
                                                       dag: BlockDag): F[BlockMessage.Safe] =
    if (b1 == b2) {
      b1.pure[F]
    } else {
      def commonAncestorChild(
          b: BlockMessage.Safe,
          commonAncestors: Set[BlockMessage.Safe]): F[List[BlockMessage.Safe]] = {
        val childrenHashes = dag.childMap.getOrElse(b.blockHash, HashSet.empty[BlockHash])
        for {
          children               <- childrenHashes.toList.traverse(ProtoUtil.unsafeGetBlock[F])
          commonAncestorChildren = children.filter(commonAncestors)
        } yield commonAncestorChildren
      }

      for {
        b1Ancestors     <- bfTraverseF[F, BlockMessage.Safe](List(b1))(ProtoUtil.unsafeGetParents[F]).toSet
        b2Ancestors     <- bfTraverseF[F, BlockMessage.Safe](List(b2))(ProtoUtil.unsafeGetParents[F]).toSet
        commonAncestors = b1Ancestors.intersect(b2Ancestors)
        gca <- bfTraverseF[F, BlockMessage.Safe](List(genesis))(
                commonAncestorChild(_, commonAncestors))
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
