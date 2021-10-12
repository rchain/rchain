package coop.rchain.v2.caspernode

/**
 * DependenciesRetriever makes sure messages required for Casper are pulled from the network.
 */
trait CasperRetriever[F[_], M] {
  def retrieve(messages: Set[M]): F[Unit]
}

object CasperRetriever {
  type MissingDependencies[M] = Set[M]
}
