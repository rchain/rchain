package coop.rchain.rspace.history.syntax

import cats.effect.Concurrent
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{internal, Blake2b256Hash}
import coop.rchain.shared.Serialize
import scodec.Codec

import scala.language.{higherKinds, implicitConversions}

trait HistoryRepositorySyntax {
  implicit final def syntaxHistoryRepository[F[_]: Concurrent, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K]
  ): HistoryRepositoryOps[F, C, P, A, K] =
    new HistoryRepositoryOps[F, C, P, A, K](historyRepo)
}
final class HistoryRepositoryOps[F[_]: Concurrent, C, P, A, K](
    private val historyRepo: HistoryRepository[F, C, P, A, K]
) {

  /**
    * get content at particular state, addressing by channel or channel encoded into hash
    */
  def getData(state: Blake2b256Hash, channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.getHistoryReader(state).getData(channel)

  def getJoins(state: Blake2b256Hash, channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[Seq[C]]] =
    historyRepo.getHistoryReader(state).getJoins(channel)

  def getContinuations(state: Blake2b256Hash, channels: Seq[C])(
      implicit serializeC: Serialize[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.getHistoryReader(state).getContinuations(channels)

  def getData(state: Blake2b256Hash, dataHash: Blake2b256Hash): F[Seq[internal.Datum[A]]] =
    historyRepo.getHistoryReader(state).getData(dataHash)

  def getJoins(state: Blake2b256Hash, joinHash: Blake2b256Hash): F[Seq[Seq[C]]] =
    historyRepo.getHistoryReader(state).getJoins(joinHash)

  def getContinuations(
      state: Blake2b256Hash,
      continuationHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.getHistoryReader(state).getContinuations(continuationHash)

  /**
    * get from channel hash written in event log (TODO to be removed)
    */
  def getDataFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.getHistoryReader(state).getDataFromChannelHash(channelHash)(historyRepo)
  def getJoinsFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[Seq[C]]] =
    historyRepo.getHistoryReader(state).getJoinsFromChannelHash(channelHash)(historyRepo)
  def getContinuationFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.getHistoryReader(state).getContinuationFromChannelHash(channelHash)(historyRepo)

}
