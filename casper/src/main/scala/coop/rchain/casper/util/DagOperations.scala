package coop.rchain.casper.util

import scala.annotation.tailrec
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
}
