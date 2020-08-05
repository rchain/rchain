package coop.rchain.node.api

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.node.api.WebApi.{BlockApiException, SignatureException}

object WebApiSyntax {
  implicit final class OptionExt[A](val x: Option[A]) extends AnyVal {
    def liftToSigErr[F[_]: Sync](error: String): F[A] =
      x.liftTo[F](new SignatureException(error))
  }

  implicit final class EitherStringExt[A](val x: Either[String, A]) extends AnyVal {
    def liftToBlockApiErr[F[_]: Sync]: F[A] =
      x.leftMap(new BlockApiException(_)).liftTo[F]
  }
}
