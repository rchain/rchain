package coop.rchain.casper.v2.core

object Validation {
  trait Offence
  trait SlashableOffence extends Offence
  final case class Slashing[M](message: M, offence: SlashableOffence)

  /** Message should have justifications for validators mentioned in message scope:
    * all slashed, all bonded. */
  case object InvalidJustifications
}
