package coop.rchain.rspace.history.syntax

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.channelStore.{ChannelStore, DataJoinHash}
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.history._
import coop.rchain.rspace.serializers.ScodecSerialize.{DatumB, JoinsB, WaitingContinuationB}
import coop.rchain.rspace.internal

trait HistoryReaderSyntax {
  implicit final def rspaceSyntaxHistoryReader[F[_]: Sync, C, P, A, K](
      historyReader: HistoryReader[F, Blake2b256Hash, C, P, A, K]
  ): HistoryReaderOps[F, C, P, A, K] =
    new HistoryReaderOps[F, C, P, A, K](historyReader)
}

final class HistoryReaderOps[F[_]: Sync, C, P, A, K](
    private val historyReader: HistoryReader[F, Blake2b256Hash, C, P, A, K]
) {

  /**
    * Reader with binary data included in result
    *
    * These methods returning raw bytes along with decode value is performance optimization.
    * Making diff for two [[WaitingContinuation]] is 5-10 times slower then in [[ByteVector]] form,
    * so the binary value is used as equality comparison for class instance.
    */
  def readerBinary: HistoryReaderBinary[F, C, P, A, K] =
    new HistoryReaderBinary[F, C, P, A, K] {
      override def getData(key: Blake2b256Hash): F[Seq[DatumB[A]]] =
        historyReader.getDataProj(key)(DatumB(_, _))

      override def getContinuations(key: Blake2b256Hash): F[Seq[WaitingContinuationB[P, K]]] =
        historyReader.getContinuationsProj(key)(WaitingContinuationB(_, _))

      override def getJoins(key: Blake2b256Hash): F[Seq[JoinsB[C]]] =
        historyReader.getJoinsProj(key)(JoinsB(_, _))
    }

  /**
    * Get from channel hash written in event log (TODO to be removed)
    */
  def getDataFromChannelHash(
      channelHash: Blake2b256Hash
  )(implicit channelStore: ChannelStore[F, C]): F[Seq[internal.Datum[A]]] =
    for {
      maybeDataHash <- channelStore.getChannelHash(channelHash)
      dataHash <- maybeDataHash match {
                   case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found data hash for $channelHash in channel store")
                     )
                 }
      data <- historyReader.getData(dataHash)
    } yield data

  def getJoinsFromChannelHash(
      channelHash: Blake2b256Hash
  )(implicit channelStore: ChannelStore[F, C]): F[Seq[Seq[C]]] =
    for {
      maybeJoinHash <- channelStore.getChannelHash(channelHash)
      joinHash <- maybeJoinHash match {
                   case Some(DataJoinHash(_, joinHash)) => joinHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found join hash for $channelHash in channel store")
                     )
                 }
      data <- historyReader.getJoins(joinHash)
    } yield data
}
