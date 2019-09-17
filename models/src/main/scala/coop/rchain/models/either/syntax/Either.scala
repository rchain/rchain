package coop.rchain.models.either.syntax

import cats.effect.Sync
import cats.implicits._

import coop.rchain.casper.protocol.ServiceError
import coop.rchain.models.StacksafeMessage
import coop.rchain.models.either.EitherHelper

import scalapb.GeneratedMessageCompanion

trait EitherSyntax {
  implicit final def modelEitherSyntaxGrpcEither(msg: coop.rchain.either.Either): GrpcEitherOps =
    new GrpcEitherOps(msg)

  implicit final def modelEitherSyntaxScalaEitherString[A <: StacksafeMessage[A]](
      response: Either[String, A]
  ): ScalaEitherStringOps[A] =
    new ScalaEitherStringOps(response)

  implicit final def modelEitherSyntaxGrpModel[F[_]: Sync, R <: StacksafeMessage[R], A](
      response: F[R]
  ): ScalaEitherServiceErrorOps[F, R, A] =
    new ScalaEitherServiceErrorOps(response)

  implicit final def modelEitherSyntaxScalaEitherThrowable[A <: StacksafeMessage[A]](
      response: Either[Throwable, A]
  ): ScalaEitherThrowableOps[A] =
    new ScalaEitherThrowableOps(response)
}

final class GrpcEitherOps(msg: coop.rchain.either.Either) {
  def toEither[A <: StacksafeMessage[A]](
      implicit cmp: GeneratedMessageCompanion[A]
  ): Either[Seq[String], A] = EitherHelper.toEither(msg)
}

final class ScalaEitherStringOps[A <: StacksafeMessage[A]](response: Either[String, A]) {
  def toGrpcEither: coop.rchain.either.Either = EitherHelper.fromEitherString[A](response)
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

final class ScalaEitherThrowableOps[A <: StacksafeMessage[A]](response: Either[Throwable, A]) {
  def toGrpcEither: coop.rchain.either.Either = EitherHelper.fromEitherThrowable[A](response)
}
