package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger._
import coop.rchain.rspace.syntax._

import java.util.Objects
import scala.util.Random

final case class DeployIdWithCost(id: ByteString, cost: Long)

/** index of deploys depending on each other inside a single block (state transition) */
final case class DeployChainIndex(
    preStateHash: Blake2b256Hash,
    postStateHash: Blake2b256Hash,
    eventLogIndex: EventLogIndex,
    stateChanges: StateChange,
    cost: Long,
    private val hashCodeVal: Int
)

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
      preStateHash,
      postStateHash,
      eventLogIndex,
      stateChanges,
      deploysWithCost.map(_.cost).sum,
      Objects.hash(deploysWithCost.map(_.id).toSeq: _*)
    )
  }

  def random: Iterator[DeployChainIndex] =
    Iterator.continually[Int](Random.nextInt(10) + 1).map { size =>
      val deployIds = Range(0, size)
        .map(
          _ => ByteString.copyFrom(Array.fill(64)((scala.util.Random.nextInt(256) - 128).toByte))
        )
      DeployChainIndex(
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        EventLogIndex.empty,
        StateChange.empty,
        Random.nextLong,
        Objects.hash(deployIds.map(id => DeployIdWithCost(id, 0)).map(_.id): _*)
      )
    }
}
