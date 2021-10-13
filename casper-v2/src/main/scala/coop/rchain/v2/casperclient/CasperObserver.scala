package coop.rchain.v2.casperclient

/**
 * Consensus client following the latest state of the network.
 *
 * Any action performed by the node should be based on the network view maintained by CasperObserver.
 * Can be used as a standalone, does not require [[CasperValidator]].
 */
trait CasperObserver[F[_], M] {

  /**
   * Detect if messages is already processed | processing in progress.
   */
  def checkDuplicate(message: M): F[Boolean]

  /**
   * Detect lazy message, that uses justifications below finalization fringe, therefore sender is out of sync.
   */
  def checkLazy(message: M): F[Boolean]

  /**
   * Add message to observer state.
   *
   * All messages fed to Observer have to have valid sender's signature, verified upfront.
   *
   * @param message Message backed by valid sender's signature.
   */
  def add(message: M): F[Unit]
}
