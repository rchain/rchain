package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger._
import coop.rchain.rspace.syntax._

import java.util.Objects

final case class DeployIdWithCost(id: ByteString, cost: Long)

/** index of deploys depending on each other inside a single block (state transition) */
final case class DeployChainIndex(
    hostBlock: Blake2b256Hash,
    deploysWithCost: Set[DeployIdWithCost],
    preStateHash: Blake2b256Hash,
    postStateHash: Blake2b256Hash,
    eventLogIndex: EventLogIndex,
    stateChanges: StateChange,
    private val hashCodeVal: Int
) {
  // equals and hash overrides are required to make conflict resolution faster, particularly rejection options calculation
  override def equals(obj: Any): Boolean = obj match {
    case that: DeployChainIndex => that.deploysWithCost == this.deploysWithCost
    case _                      => false
  }
  // caching hash code helps a lot to increase performance of computing rejection options
  // TODO mysterious speedup of merging benchmark when setting this to some fixed value
  override def hashCode(): Int = hashCodeVal
}

object DeployChainIndex {

  implicit val ord = Ordering.by((x: DeployChainIndex) => (x.hostBlock, x.postStateHash))

  def apply[F[_]: Concurrent, C, P, A, K](
      hostBlock: Blake2b256Hash,
      deploys: Set[DeployIndex],
      preStateHash: Blake2b256Hash,
      postStateHash: Blake2b256Hash,
      historyRepository: HistoryRepository[F, C, P, A, K]
  ): F[DeployChainIndex] = {

    val deploysWithCost = deploys.map(v => DeployIdWithCost(v.deployId, v.cost))
    val eventLogIndex   = deploys.map(_.eventLogIndex).toList.combineAll

    for {
      preHistoryReader  <- historyRepository.getHistoryReader(preStateHash)
      preStateReader    = preHistoryReader.readerBinary
      postHistoryReader <- historyRepository.getHistoryReader(postStateHash)
      postStateReader   = postHistoryReader.readerBinary

      stateChanges <- StateChange[F, C, P, A, K](
                       preStateReader = preStateReader,
                       postStateReader = postStateReader,
                       eventLogIndex,
                       historyRepository.getSerializeC
                     )
    } yield DeployChainIndex(
      hostBlock,
      deploysWithCost,
      preStateHash,
      postStateHash,
      eventLogIndex,
      stateChanges,
      Objects.hash(deploysWithCost.map(_.id).toSeq: _*)
    )
  }

  def deployChainCost(r: DeployChainIndex) = r.deploysWithCost.map(_.cost).sum
  def depends(a: DeployChainIndex, b: DeployChainIndex) =
    EventLogMergingLogic.depends(a.eventLogIndex, b.eventLogIndex)
  def branchesAreConflicting(as: Set[DeployChainIndex], bs: Set[DeployChainIndex]): Boolean =
    (as.flatMap(_.deploysWithCost.map(_.id)) intersect bs.flatMap(_.deploysWithCost.map(_.id))).nonEmpty ||
      EventLogMergingLogic.areConflicting(
        as.map(_.eventLogIndex).toList.combineAll,
        bs.map(_.eventLogIndex).toList.combineAll
      )
  // TODO make this a map compute on replay
  def deploysAreConflicting(as: DeployChainIndex, bs: DeployChainIndex): Boolean =
    (as.deploysWithCost.map(_.id) intersect bs.deploysWithCost.map(_.id)).nonEmpty ||
      EventLogMergingLogic.areConflicting(as.eventLogIndex, bs.eventLogIndex)
}
