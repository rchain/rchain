package coop.rchain.v2.caspernode.impl.streams

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.v2.casper.data.{FinalizationFringe, LatestMessages}
import coop.rchain.v2.caspernode.{CasperObserver, CasperRetriever, CasperValidator}
import coop.rchain.v2.caspernode.CasperObserver.AddResult
import coop.rchain.v2.caspernode.impl.streams.ValidatorStream.ValidationTarget
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
      persistEffect: AddResult[M, S] => F[Unit]
  ): Stream[F, Option[AddResult[M, S]]] = input
    // Check and store incoming messages concurrently
    .parEvalMapProcBounded(checkIgnore(_).semiflatTap(persistMessage).value)
    // Invoke effect sequentially
    .evalMap {
      case Left(reason) => ignoredEffect(reason).as(None[AddResult[M, S]])
      case Right(m)     =>
        observer.add[S](m).flatMap { case r @ AddResult(_, _, deps, doValidate) =>
          // Ask retriever to get dependencies
          retriever.retrieve(deps).whenA(deps.nonEmpty) *>
            // Send to validation if
            validator.validate(m, selfMessage = false).whenA(doValidate) *>
            persistEffect(r).as(r.some)
        }
    }

  /**
   * Message has invalid signature, therefore meaningless.
   */
  def rejectSignatureInvalid: IgnoreReason = SignatureInvalid

  /**
   * Message is already processed | processing in progress.
   */
  def rejectDuplicate: IgnoreReason = Duplicate

  /**
   * Message is from lazy sender. Sender is lazy when message uses justifications below finalization fringe,
   * therefore cannot keep up with the network.
   */
  def rejectLazy: IgnoreReason = LazySender

  /**
   * Reason for message to not be processed.
   */
  trait IgnoreReason

  case object SignatureInvalid extends IgnoreReason
  case object Duplicate        extends IgnoreReason
  case object LazySender       extends IgnoreReason
}
