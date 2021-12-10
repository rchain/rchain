package coop.rchain.casper.processing

import coop.rchain.casper.processing.MessageValidator.ValidationResult
import fs2.Stream

/** Validation of messages that have all dependencies validated. */
trait MessageValidator[F[_], M, S] {

  /**
    * Input messages to be validated.
    */
  def input: Stream[F, M]

  /**
    * Validate the message.
    */
  def validate(message: M): F[ValidationResult[M, S]]

  /**
    * Validation of a message can trigger validation of a child, so being able to append to input is required.
    */
  def appendToInput(message: M): F[Unit]
}

object MessageValidator {
  final case class ValidationResult[M, S](newState: S, dependentUnlocked: Set[M])
}
