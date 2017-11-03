package coop.rchain.rosette

import java.io.{File, PrintWriter}

import scala.collection.mutable

package object utils {

  def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def pSlice[T](src: mutable.Seq[T], start: Int, end: => Int) =
    new mutable.Seq[T] {
      def checkBounds(idx: Int): Unit =
        if (idx > end - start) {
          throw new IndexOutOfBoundsException
        }

      override def update(idx: Int, elem: T): Unit = {
        checkBounds(idx)

        src(idx + start) = elem
      }

      override def length: Int =
        math.min(src.size, end) - start

      override def apply(idx: Int): T = {
        checkBounds(idx)

        src(idx + start)
      }

      override def iterator: Iterator[T] =
        src.slice(start, end).iterator
    }
}
