package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.kernel.Monoid
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger.{StateChange, _}
import coop.rchain.rspace.syntax._

final case class DeployIdWithCost(id: ByteString, cost: Long)

/** index of deploys depending on each other inside a single block (state transition) */
final case class DeployChainIndex(
    deploysWithCost: Set[DeployIdWithCost],
    preStateHash: Blake2b256Hash,
    postStateHash: Blake2b256Hash,
    eventLogIndex: EventLogIndex,
    stateChanges: StateChange
)

object DeployChainIndex {
  def apply[F[_]: Concurrent, C, P, A, K](
      deploys: Set[DeployIndex],
      preStateHash: StateHash,
      postStateHash: StateHash,
      historyRepository: HistoryRepository[F, C, P, A, K]
  ): F[DeployChainIndex] = {

    val deploysWithCost = deploys.map(v => DeployIdWithCost(v.deployId, v.cost))
    val preStatHash     = Blake2b256Hash.fromByteString(preStateHash)
    val postStatHash    = Blake2b256Hash.fromByteString(postStateHash)
    val eventLogIndex   = deploys.map(_.eventLogIndex).toList.combineAll

    val preStateReader  = historyRepository.getHistoryReader(preStatHash).readerBinary
    val postStateReader = historyRepository.getHistoryReader(postStatHash).readerBinary
    for {
      stateChanges <- StateChange.computeStateChange[F, C, P, A, K](
                       preStateReader = preStateReader,
                       postStateReader = postStateReader,
                       eventLogIndex,
                       channelsStore = historyRepository,
                       historyRepository.getSerializeC
                     )

    } yield DeployChainIndex(
      deploysWithCost,
      preStatHash,
      postStatHash,
      eventLogIndex,
      stateChanges
    )
  }
}
