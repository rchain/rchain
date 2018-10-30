package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.{FlatMap, MonadError}
import cats.implicits._
import com.typesafe.scalalogging.Logger

import scala.reflect.io.Directory

object ResourceTools {
  def withResource[F[_]: FlatMap, T, R](
      acquire: => F[R],
      release: (R, Option[Throwable]) => F[Unit]
  )(job: R => F[T])(implicit F: MonadError[F, Throwable]): F[T] =
    for {
      resource <- acquire
      attempt  <- job(resource).attempt
      result <- attempt.fold(
                 ex => release(resource, Some(ex)) *> F.raiseError(ex),
                 r => release(resource, None) *> F.pure(r)
               )
    } yield result

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  def withTempDir[F[_], A](
      prefix: String
  )(job: Path => F[A])(implicit F: MonadError[F, Throwable]): F[A] = {
    def deleteTempDir(path: Path, maybeError: Option[Throwable]): F[Unit] =
      F.pure(maybeError match {
        case None => new Directory(new File(path.toString)).deleteRecursively()
        case Some(ex) =>
          logger
            .error(
              s"Exception thrown while using the tempDir '$path'. Temporary dir NOT deleted.",
              ex
            )
      })

    withResource[F, A, Path](
      F.pure(Files.createTempDirectory(prefix)),
      deleteTempDir
    )(job)
  }
}
