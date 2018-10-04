package coop.rchain.casper.util.comm

import coop.rchain.comm.protocol.routing._
import cats._, cats.data._, cats.implicits._

object BlobHandler {
  def handleBlob[F[_]: Applicative]: Blob => F[Unit] = (blob: Blob) => ().pure[F] // FIX-ME
}
