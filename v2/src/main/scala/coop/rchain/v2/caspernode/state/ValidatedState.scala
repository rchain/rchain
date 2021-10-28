package coop.rchain.v2.caspernode.state

import coop.rchain.v2.casper.data.{CasperData, CasperScope, LatestMessages}
import coop.rchain.v2.caspernode.state.ValidatedState.FinalizationState

import scala.collection.immutable.{Set, SortedMap}

/**
 * View on the network from validated state.
 */
final case class ValidatedState[M, S, U](
    messages: Map[M, CasperData[M, S, U]],
    latestMessages: LatestMessages[M, S],
    parentsMap: Map[M, Set[M]],
    //    childrenMap: Map[M, Set[M]], TODO where used?
    heightMap: SortedMap[Long, Set[M]],
    invalidBlocksSet: Set[M],
    casperScope: CasperScope[M, S],
    finalizationState: FinalizationState[U]
) { def dagSet: Set[M] = messages.keySet }

object ValidatedState {

  /**
   * State of finalization of according to the view on the network
   */
  final case class FinalizationState[U](accepted: Set[U], rejected: Set[U])
}
