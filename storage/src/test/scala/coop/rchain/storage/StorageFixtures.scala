package coop.rchain.storage

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import coop.rchain.storage.util.withResource

trait StorageFixtures {

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

  private def recursivelyDeletePath(p: Path): Path =
    Files.walkFileTree(p, makeDeleteFileVisitor)

  def withTestStorage[A](f: Storage => A): A = {
    val tmpDir = Files.createTempDirectory("rchain-storage-test-")
    try {
      val storage = Storage.create(tmpDir, "test", 500000L)
      withResource(storage)(f)
    } finally {
      recursivelyDeletePath(tmpDir)
    }
  }
}
