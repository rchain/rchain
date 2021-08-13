package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger.{StateChange, _}
import coop.rchain.rspace.syntax._

import scala.util.Random

final case class DeployIdWithCost(id: ByteString, cost: Long)

/** index of deploys depending on each other inside a single block (state transition) */
final case class DeployChainIndex(
    deploysWithCost: Set[DeployIdWithCost],
    preStateHash: Blake2b256Hash,
    postStateHash: Blake2b256Hash,
    eventLogIndex: EventLogIndex,
    stateChanges: StateChange
) {
  // equals and hash overrides are required to make conflict resolution faster, particularly rejection options calculation
  override def equals(obj: Any): Boolean = obj match {
    case that: DeployChainIndex => that.deploysWithCost.map(_.id) == this.deploysWithCost.map(_.id)
    case _                      => false
  }

  override def hashCode(): Int =
    deploysWithCost.map(_.id).foldLeft(0)((acc, v) => acc + v.hashCode() * 31)
}

object DeployChainIndex {

  implicit val ord = Ordering.by((_: DeployChainIndex).postStateHash)

  def apply[F[_]: Concurrent, C, P, A, K](
      deploys: Set[DeployIndex],
      preStateHash: Blake2b256Hash,
      postStateHash: Blake2b256Hash,
      historyRepository: HistoryRepository[F, C, P, A, K]
  ): F[DeployChainIndex] = {

    val deploysWithCost = deploys.map(v => DeployIdWithCost(v.deployId, v.cost))
    val eventLogIndex   = deploys.map(_.eventLogIndex).toList.combineAll

    val preStateReader  = historyRepository.getHistoryReader(preStateHash).readerBinary
    val postStateReader = historyRepository.getHistoryReader(postStateHash).readerBinary
    for {
      stateChanges <- StateChange[F, C, P, A, K](
                       preStateReader = preStateReader,
                       postStateReader = postStateReader,
                       eventLogIndex,
                       historyRepository.getSerializeC
                     )
    } yield DeployChainIndex(
      deploysWithCost,
      preStateHash,
      postStateHash,
      eventLogIndex,
      stateChanges
    )
  }

  def random: Iterator[DeployChainIndex] =
    Iterator.continually[Int](Random.nextInt(10)).map { size =>
      val deployIds = Range(0, size)
        .map(
          _ => ByteString.copyFrom(Array.fill(64)((scala.util.Random.nextInt(256) - 128).toByte))
        )
      DeployChainIndex(
        deployIds.map(id => DeployIdWithCost(id, 0)).toSet,
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        EventLogIndex.empty,
        StateChange.empty
      )
    }
}
