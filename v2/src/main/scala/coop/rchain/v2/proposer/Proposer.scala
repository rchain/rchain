package coop.rchain.v2.proposer

/**
 * Proposes changes to the network state.
 */
trait Proposer[F[_]] {

  /**
   * Propose a state change.
   */
  def propose: F[Unit]
}
