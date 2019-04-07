package coop.rchain.models.either

import coop.rchain.either.{EitherError, EitherSuccess}
import coop.rchain.either.Either.Content
import coop.rchain.models.StacksafeMessage
import coop.rchain.shared.ThrowableOps._

import scalapb.GeneratedMessageCompanion

object EitherHelper {
  def toEither[A <: StacksafeMessage[A]](
      msg: coop.rchain.either.Either
  )(implicit cmp: GeneratedMessageCompanion[A]): Either[Seq[String], A] = {
    require(msg.content.isDefined, "Either message has no content")
    if (msg.content.isError) {
      require(msg.content.error.isDefined, "Either message is an undefined Error")
      Left(msg.content.error.get.messages)
    } else {
      require(msg.content.success.isDefined, "Either message is an undefined Success")
      require(
        msg.content.success.get.response.isDefined,
        "Either message is a Success with an undefined response"
      )
      Right(EitherAnyHelper.unpack[A](msg.content.success.get.response.get))
    }
  }

  def fromEitherString[A <: StacksafeMessage[A]](
      response: Either[String, A]
  ): coop.rchain.either.Either =
    response match {
      case Right(a) =>
        coop.rchain.either.Either(Content.Success(EitherSuccess(Some(EitherAnyHelper.pack(a)))))
      case Left(e) =>
        coop.rchain.either.Either(Content.Error(EitherError(Seq(e))))
    }

  def fromEitherThrowable[A <: StacksafeMessage[A]](
      response: Either[Throwable, A]
  ): coop.rchain.either.Either =
    response match {
      case Right(a) =>
        coop.rchain.either.Either(Content.Success(EitherSuccess(Some(EitherAnyHelper.pack(a)))))
      case Left(t) =>
        coop.rchain.either.Either(Content.Error(EitherError(t.toMessageList())))
    }

}
