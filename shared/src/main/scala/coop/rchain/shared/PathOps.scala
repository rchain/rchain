package coop.rchain.shared

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.effect.Sync
import cats.implicits._

object PathOps {

  implicit class RichPath(value: Path) {
    def folderSize: Long =
      Files
        .walk(value)
        .mapToLong(p => {
          val f = p.toFile
          if (f.isFile)
            f.length
          else
            0
        })
        .sum()

    def recursivelyDelete(): Path =
      Files.walkFileTree(value, makeDeleteFileVisitor)
  }

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

  implicit class PathDelete(path: Path) {
    def deleteSingleFile[F[_]: Sync: Log]()(implicit logSource: LogSource): F[Unit] = {
      import coop.rchain.catscontrib.Catscontrib.ToBooleanOpsFromBoolean

      def delete(): F[Unit] =
        for {
          result <- Sync[F].delay(path.toFile.delete).attempt
          _ <- result match {
                case Left(t) =>
                  Log[F].error(s"Can't delete file $path: ${t.getMessage}", t)
                case Right(false) =>
                  Log[F].warn(s"Can't delete file $path.")
                case Right(true) =>
                  Log[F].info(s"Deleted file $path")
              }
        } yield ()

      for {
        exists <- Sync[F].delay(path.toFile.exists)
        _      <- exists.fold(delete(), Log[F].warn(s"Can't delete file $path. File not found."))
      } yield ()
    }

  }
}
