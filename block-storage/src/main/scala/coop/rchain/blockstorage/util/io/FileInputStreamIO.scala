package coop.rchain.blockstorage.util.io

import java.io.{FileNotFoundException, InputStream}
import java.nio.file.{Files, OpenOption, Path}

import cats.effect.{Resource, Sync}
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError

object FileInputStreamIO {
  def open[F[_]: Sync: RaiseIOError](path: Path, options: OpenOption*): Resource[F, InputStream] =
    Resource.make[F, InputStream](
      handleIo(Files.newInputStream(path, options: _*), {
        case e: FileNotFoundException => FileNotFound(e)
        case e                        => UnexpectedIOError(e)
      })
    )(stream => handleIo(stream.close(), ClosingFailed.apply))
}
