package coop.rchain.casper.processing

/** DependenciesRetriever makes sure messages required for Casper are pulled from the network. */
trait DependenciesRetriever[F[_], M] {
  def retrieve(messages: Set[M]): F[Unit]
  def ackRetrieved(message: M): F[Unit]
}
