package coop.rchain.casper.util

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.BlockDag
import coop.rchain.casper.Estimator.BlockHash

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable

object DagOperations {
  def bfTraverse[A](start: Iterable[A])(neighbours: (A) => Iterator[A]): Iterator[A] =
    new Iterator[A] {
      private val visited    = new mutable.HashSet[A]()
      private val underlying = new mutable.Queue[A]()
      start.foreach(underlying.enqueue(_))

      @tailrec
      final override def hasNext: Boolean = underlying.headOption match {
        case None => false
        case Some(nxt) =>
          if (visited(nxt)) {
            underlying.dequeue() //remove already visited block
            hasNext              //try again to find existence of next block
          } else {
            true
          }
      }

      override def next(): A =
        if (hasNext) {
          val nxt = underlying.dequeue()
          visited.add(nxt)

          neighbours(nxt)
            .filterNot(a => visited(a)) //only add parents that have not already been visited
            .foreach(underlying.enqueue(_))

          nxt
        } else {
          Iterator.empty.next()
        }
    }

  //Conceptually, the GCA is the first point at which the histories of b1 and b2 diverge.
  //Based on that, we compute by finding the first block from genesis for which there
  //exists a child of that block which is an ancestor of b1 or b2 but not both.
  def greatestCommonAncestor(b1: BlockMessage,
                             b2: BlockMessage,
                             genesis: BlockMessage,
                             dag: BlockDag): BlockMessage =
    if (b1 == b2) b1
    else {
      def parents(b: BlockMessage): Iterator[BlockMessage] =
        ProtoUtil.parents(b).iterator.map(dag.blockLookup)

      val b1Ancestors = new mutable.HashSet[BlockMessage]
      bfTraverse[BlockMessage](Some(b1))(parents).foreach(b1Ancestors += _)

      val b2Ancestors = new mutable.HashSet[BlockMessage]
      bfTraverse[BlockMessage](Some(b2))(parents).foreach(b2Ancestors += _)

      val commonAncestors = b1Ancestors.intersect(b2Ancestors)

      def commonAncestorChild(b: BlockMessage): Iterator[BlockMessage] =
        dag.childMap
          .getOrElse(b.blockHash, HashSet.empty[BlockHash])
          .iterator
          .map(dag.blockLookup)
          .find(commonAncestors(_))
          .iterator

      val gca = bfTraverse[BlockMessage](Some(genesis))(commonAncestorChild).find(b => {
        dag.childMap
          .getOrElse(b.blockHash, HashSet.empty[BlockHash])
          .exists(hash => {
            val c = dag.blockLookup(hash)
            b1Ancestors(c) ^ b2Ancestors(c)
          })
      })

      gca.get //none found iff b1 == b2, which is checked at the beginning
    }
}
