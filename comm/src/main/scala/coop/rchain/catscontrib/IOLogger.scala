package coop.rchain.catscontrib

import com.typesafe.scalalogging.Logger
import cats._, cats.data._, cats.implicits._

class IOLogger(name: String) {

  val logger = Logger(name)

  def info[F[_]: Capture](msg: String): F[Unit] = Capture[F].capture {
    logger.info(msg)
  }
}

object IOLogger {
  def apply(name: String): IOLogger = new IOLogger(name)
}
