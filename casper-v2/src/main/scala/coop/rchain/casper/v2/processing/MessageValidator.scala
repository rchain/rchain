package coop.rchain.casper.v2.processing

import coop.rchain.casper.v2.processing.MessageValidator.ValidationResult
import fs2.Stream

/**
  * Validator of messages Casper messages. NOTE: input messages have to have no missing dependencies.
  * @tparam M Type of the message.
  * @tparam S Type of the state that messages are validated against.
  */
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

  /**
    * Result of message validation.
    * @param newState           New state after validation effect.
    * @param dependentUnlocked  Children that can be validated as message is the last dependency.
    */
  final case class ValidationResult[M, S](newState: S, dependentUnlocked: Set[M])
}
