package coop.rchain.v2.caspernode
import cats.effect.Sync
import coop.rchain.v2.casper.data.LatestMessages
import coop.rchain.v2.caspernode.CasperValidator.ValidateResult
import coop.rchain.v2.validation.Offence

/**
 * Validator of Casper messages.
 *
 * Uses execution engine to build state. Detects offences.
 */
trait CasperValidator[F[_], M, S] {

  /**
   * Validate message against its scope, computed from local validated state.
   *
   * @param selfMessage Whether message validated is self proposed.
   */
  def validate(message: M, selfMessage: Boolean): F[ValidateResult[M]]
}

object CasperValidator {

  /**
   * Higher validated messages for all known senders.
   */
  type ValidationFringe[M, S] = LatestMessages[M, S]

  /**
   * @param offenceOpt       Offence found.
   * @param unlockedChildren Children that have all dependencies validated.
   * @tparam M               Type of a message
   */
  final case class ValidateResult[M](
      offenceOpt: Option[Offence],
      unlockedChildren: List[M]
  )

  /**
   * When proposed message turn out to be invalid, node should shut down.
   */
  val invalidProposeSet              = "Unrecoverable error, attempt to propose message which is invalid."
  def invalidSelfMessage[F[_]: Sync] = Sync[F].raiseError(new Exception(invalidProposeSet))
}
