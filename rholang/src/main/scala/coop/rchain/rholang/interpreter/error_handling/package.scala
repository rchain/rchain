package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.MonadError
import cats.effect.concurrent.Ref

package object error_handling {

  type _error[F[_]] = Ref[F, Option[Throwable]]

  def runAndReportErrors[F[_]: MonadError[?[_], Throwable]](
      process: F[Unit]
  )(implicit error: _error[F]): F[Unit] =
    error.get >>= {
      case Some(_) => ().pure[F]
      case None =>
        process.handleErrorWith { currentError =>
          error.update {
            case Some(previousError) => Some(previousError)
            case None                => Some(currentError)
          }
        }
    }

}
