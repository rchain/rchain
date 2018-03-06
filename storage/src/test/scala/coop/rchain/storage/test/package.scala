package coop.rchain.storage

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

package object test {

  /**
    * Makes a SimpleFileVisitor to delete files and the directories that contained them
    *
    * [[https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileVisitor.html]]
    */
  private def makeDeleteFileVisitor: SimpleFileVisitor[Path] =
    new SimpleFileVisitor[Path] {
      override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(p)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(p: Path, e: java.io.IOException): FileVisitResult = {
        Files.delete(p)
        FileVisitResult.CONTINUE
      }
    }

  def recursivelyDeletePath(p: Path): Path =
    Files.walkFileTree(p, makeDeleteFileVisitor)

  /** Drops the 'i'th element of a list.
    */
  def dropIndex[T](xs: List[T], n: Int): List[T] = {
    val (l1, l2) = xs splitAt n
    l1 ++ (l2 drop 1)
  }

  // TODO(ht): Give this a better name
  def dropFirst[K, V](xs: List[(K, V)], k: K): List[(K, V)] = {
    val maybeIndex: Option[Int] = xs.zipWithIndex.collectFirst {
      case ((ck, _), index) if ck == k => index
    }
    maybeIndex match {
      case Some(index) => dropIndex(xs, index)
      case None        => xs
    }
  }
}
