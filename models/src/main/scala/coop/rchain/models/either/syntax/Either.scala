package coop.rchain.models.either.syntax

import coop.rchain.models.StacksafeMessage
import coop.rchain.models.either.EitherHelper
import cats.syntax.either._

import scalapb.GeneratedMessageCompanion

trait EitherSyntax {
  implicit final def modelEitherSyntaxGrpcEither(msg: coop.rchain.either.Either): GrpcEitherOps =
    new GrpcEitherOps(msg)

  implicit final def modelEitherSyntaxScalaEitheString[A <: StacksafeMessage[A]](
      response: Either[String, A]
  ): ScalaEitherStringOps[A] =
    new ScalaEitherStringOps(response)

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

final class ScalaEitherThrowableOps[A <: StacksafeMessage[A]](response: Either[Throwable, A]) {
  def toGrpcEither: coop.rchain.either.Either = EitherHelper.fromEitherThrowable[A](response)
}
