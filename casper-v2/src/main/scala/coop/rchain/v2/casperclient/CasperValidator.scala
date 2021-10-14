package coop.rchain.v2.casperclient
import coop.rchain.v2.casper.data.CasperScope

/**
 * Validator of Casper messages.
 *
 * Uses execution engine to build state. Detects offences.
 */
trait CasperValidator[F[_], M, S] {

  /**
   * Validate message.
   *
   * @param message message with all dependencies validated.
   */
  def validate(message: M): F[Unit]
}

object CasperValidator {

  /**
   * Validator expects only messages with all dependencies validated.
   */
  val missingDependencies = "Validation error: message does not have all dependencies validated."

  /**
   * When offence is detected on self proposed message, node should shut down.
   */
  val invalidProposeMsg = "Validation error: self proposed message is invalid."

  case object MissingDependencies extends Exception(missingDependencies)

  case object InvalidSelfMessage extends Exception(invalidProposeMsg)
}
