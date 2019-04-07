package coop.rchain.models.either

import coop.rchain.either.EitherAny
import coop.rchain.models.StacksafeMessage

import scalapb._

object EitherAnyHelper {
  def pack[A <: StacksafeMessage[A]](generatedMessage: A): EitherAny =
    pack(generatedMessage, "type.rchain.coop/")

  def pack[A <: StacksafeMessage[A]](
      generatedMessage: A,
      urlPrefix: String
  ): EitherAny =
    EitherAny(
      typeUrl =
        if (urlPrefix.endsWith("/"))
          urlPrefix + generatedMessage.companion.scalaDescriptor.fullName
        else
          urlPrefix + "/" + generatedMessage.companion.scalaDescriptor.fullName,
      value = generatedMessage.toByteString
    )

  private def typeNameFromTypeUrl(typeUrl: String): String =
    typeUrl.split("/").lastOption.getOrElse(typeUrl)

  def is[A <: StacksafeMessage[A]](
      eitherAny: EitherAny
  )(implicit cmp: GeneratedMessageCompanion[A]): Boolean =
    typeNameFromTypeUrl(eitherAny.typeUrl) == cmp.scalaDescriptor.fullName

  def unpack[A <: StacksafeMessage[A]](
      eitherAny: EitherAny
  )(implicit cmp: GeneratedMessageCompanion[A]): A = {
    require(is[A](eitherAny), s"Type of the EitherAny message does not match the given class.")
    cmp.parseFrom(eitherAny.value.newCodedInput())
  }
}
