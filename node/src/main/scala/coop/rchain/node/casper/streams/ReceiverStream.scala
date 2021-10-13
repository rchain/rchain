package coop.rchain.node.casper.streams

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.v2.casperclient.CasperObserver.{IgnoreReason, ObserveEffects}
import coop.rchain.v2.casperclient.{CasperObserver, CasperRetriever, CasperValidator}
import fs2.Stream

/**
 * Stream processing incoming Casper messages.
 */
object ReceiverStream {

  def apply[F[_]: Concurrent, M, S](
      input: Stream[F, M],
      checkIgnore: M => EitherT[F, IgnoreReason, M],
      persistMessage: M => F[Unit],
      ignoredEffect: IgnoreReason => F[Unit],
      observer: CasperObserver[F, M],
      validator: CasperValidator[F, M, S],
      retriever: CasperRetriever[F, M],
      persistEffect: ObserveEffects[M, S] => F[Unit]
  ): Stream[F, Option[ObserveEffects[M, S]]] = input
    // Check and store incoming messages concurrently
    .parEvalMapProcBounded(checkIgnore(_).semiflatTap(persistMessage).value)
    // Invoke effect sequentially
    .evalMap {
      case Left(reason) => ignoredEffect(reason).as(None[ObserveEffects[M, S]])
      case Right(m)     =>
        observer.add[S](m).flatMap { case r @ ObserveEffects(_, _, deps, doValidate) =>
          // Ask retriever to get dependencies
          retriever.retrieve(deps).whenA(deps.nonEmpty) *>
            // Send to validation if suitable
            validator.validate(m, selfMessage = false).whenA(doValidate) *>
            persistEffect(r).as(r.some)
        }
    }
}
