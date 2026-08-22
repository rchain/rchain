package coop.rchain.models.either.syntax

import cats.effect.Sync
import cats.implicits._

import coop.rchain.casper.protocol.ServiceError
import coop.rchain.models.StacksafeMessage

trait EitherSyntax {

  implicit final def modelEitherSyntaxGrpModel[F[_]: Sync, R <: StacksafeMessage[R], A](
      response: F[R]
  ): ScalaEitherServiceErrorOps[F, R, A] =
    new ScalaEitherServiceErrorOps(response)
}

final class ScalaEitherServiceErrorOps[F[_]: Sync, R <: StacksafeMessage[R], A](response: F[R]) {
  def toEitherF(
      error: R => Option[ServiceError],
      result: R => Option[A]
  ): F[Either[Seq[String], A]] =
    response.flatMap { r =>
      error(r)
        .map(_.messages.asLeft[A].pure[F])
        .orElse(result(r).map(_.asRight[Seq[String]].pure[F]))
        .getOrElse(Sync[F].raiseError(new RuntimeException("Response is empty")))
    }
}
