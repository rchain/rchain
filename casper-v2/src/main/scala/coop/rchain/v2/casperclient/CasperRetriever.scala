package coop.rchain.v2.casperclient

/**
 * DependenciesRetriever makes sure messages required for Casper are pulled from the network.
 *
 * Contains only a single method. How exactly delivery is provided is up to implementation.
 */
trait CasperRetriever[F[_], M] {

  /**
   * Accept request to retrieve.
   */
  def ackRequest(messages: Set[M]): F[Unit]
}
