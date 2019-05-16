package coop.rchain.blockstorage.util.io

import java.io.FileNotFoundException
import java.nio.file.Path

import cats.effect.{Resource, Sync}
import cats.syntax.functor._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError

import scala.io.Source

final case class SourceIO[F[_]: Sync: RaiseIOError] private (
    private val source: Source
) {
  def getLines: F[List[String]] =
    handleIo(source.getLines.toList, UnexpectedIOError.apply)

  def close: F[Unit] =
    handleIo(source.close(), ClosingFailed.apply)
}

object SourceIO {
  def open[F[_]: Sync: RaiseIOError](path: Path): Resource[F, SourceIO[F]] =
    Resource.make(
      handleIo(Source.fromFile(path.toFile), {
        case e: FileNotFoundException => FileNotFound(e)
        case e                        => UnexpectedIOError(e)
      }).map(SourceIO.apply[F])
    )(_.close)
}
