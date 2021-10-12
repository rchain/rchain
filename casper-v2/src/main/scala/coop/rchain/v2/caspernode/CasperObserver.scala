package coop.rchain.v2.caspernode

import coop.rchain.v2.casper.data.{FinalizationFringe, LatestMessages}
import coop.rchain.v2.caspernode.CasperObserver.AddResult
import coop.rchain.v2.caspernode.CasperRetriever.MissingDependencies

/**
 * Consensus client following the latest state of the network.
 *
 * Any action performed by the node should be based on the network view maintained by CasperClient.
 * Can be used as a standalone, does not require [[CasperValidator]].
 */
trait CasperObserver[F[_], M] {

  /**
   * Add message to observer state.
   *
   * All messages fed to Observer have to have valid sender's signature, verified upfront.
   *
   * @param message Message backed by valid sender's signature.
   * @return        Dependencies of M that are missing and new finalization fringe.
   */
  def add[S](message: M): F[AddResult[M, S]]
}

object CasperObserver {

  /**
   * There are 4 effects that Observer can produce by adding a message:
   * 1. Add message to the state
   * 2. Update latest messages.
   * 3. Advance finalization fringe.
   * 4. Forward message to validator if all dependencies are already validated.
   * 5. Request missing dependencies if there are some.
   */
  case class AddResult[M, S](
      message: M,
      newLatestMessages: Option[LatestMessages[M, S]],
      newFinalizationFringe: Option[FinalizationFringe[M]],
      dependenciesMissing: MissingDependencies[M],
      forwardToValidator: Boolean
  )
}
