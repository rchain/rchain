package coop.rchain.casper.processing

import cats.data.EitherT
import coop.rchain.casper.processing.MessageReceiver._
import fs2.Stream

import scala.collection.immutable.Set

/** Guard for validator, processes messages incoming and makes sure they are ready for validation.*/
trait MessageReceiver[F[_], M, S] {

  /**
    * Stream of inbound messages.
    */
  def input: Stream[F, M]

  /**
    * Check whether message should be ignored.
    */
  def checkIgnore(message: M): EitherT[F, ReceiveReject, M]

  /**
    * Persist message to storage.
    */
  def store(message: M): F[Unit]

  /**
    * Write diagnostic data.
    */
  def diagRejected(message: M, r: ReceiveReject): F[Unit]

  /**
    * @return [[ReceiveResult]] */
  def receivedEffect(message: M): F[ReceiveResult[M, S]]
}

object MessageReceiver {
  trait ReceiveReject
  case object Validated            extends ReceiveReject
  case object PendingValidation    extends ReceiveReject
  case object ValidationInProgress extends ReceiveReject
  case object SignatureInvalid     extends ReceiveReject

  def validated: ReceiveReject            = Validated
  def pendingValidation: ReceiveReject    = PendingValidation
  def validationInProgress: ReceiveReject = ValidationInProgress
  def signatureInvalid: ReceiveReject     = SignatureInvalid

  /**
    * @param newState               State after receive.
    * @param dependenciesPending    Dependencies that are received and pending validation.
    * @param dependenciesToRetrieve Dependencies that are unknown and should be retrieved.
    */
  final case class ReceiveResult[M, S](
      newState: S,
      dependenciesPending: Set[M],
      dependenciesToRetrieve: Set[M]
  )
}
