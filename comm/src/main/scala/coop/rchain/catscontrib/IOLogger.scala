package coop.rchain.catscontrib

import com.typesafe.scalalogging.Logger
import cats._, cats.data._, cats.implicits._

// TODO remove, use Log effect instead
class IOLogger(name: String) {

  val logger = Logger(name)

  def info[F[_]: Capture](msg: String): F[Unit] = Capture[F].capture {
    logger.info(msg)
  }

  def debug[F[_]: Capture](msg: String): F[Unit] = Capture[F].capture {
    logger.debug(msg)
  }

}

object IOLogger {
  def apply(name: String): IOLogger = new IOLogger(name)
}
