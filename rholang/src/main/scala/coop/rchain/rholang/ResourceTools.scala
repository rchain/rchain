package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.effect.Resource
import cats.Applicative
import cats.effect.ExitCase.{Canceled, Completed, Error}
import com.typesafe.scalalogging.Logger

import scala.reflect.io.Directory

object ResourceTools {
  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  def mkTempDir[F[_]: Applicative](prefix: String): Resource[F, Path] =
    Resource.makeCase(Applicative[F].pure(Files.createTempDirectory(prefix)))(
      (path, exitCase) =>
        Applicative[F].pure(exitCase match {
          case Error(ex) =>
            logger
              .error(
                s"Exception thrown while using the tempDir '$path'. Temporary dir NOT deleted.",
                ex
              )
          case _ => new Directory(new File(path.toString)).deleteRecursively()
        })
    )
}
