package coop.rchain.rspace.history.syntax

import cats.effect.Sync
import coop.rchain.rspace.Hasher.{hashContinuationsChannels, hashDataChannel, hashJoinsChannel}
import coop.rchain.rspace.channelStore.{ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.HashHistoryReader
import coop.rchain.rspace.{internal, Blake2b256Hash}
import coop.rchain.rspace.internal.{RichDatum, RichJoin, RichKont}
import coop.rchain.shared.Serialize
import scodec.Codec
import cats.syntax.all._

trait HashHistoryReaderSyntax {
  implicit final def syntaxHistoryReader[F[_]: Sync, C, P, A, K](
      hashHistoryReader: HashHistoryReader[F, C, P, A, K]
  ): HashHistoryReaderOps[F, C, P, A, K] =
    new HashHistoryReaderOps[F, C, P, A, K](hashHistoryReader)
}

final class HashHistoryReaderOps[F[_]: Sync, C, P, A, K](
    private val hashHistoryReader: HashHistoryReader[F, C, P, A, K]
) {

  /**
    * RhoHistoryReader API having HashHistoryReader, requires codec for channel
    */
  def getJoins(channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[Seq[C]]] =
    hashHistoryReader.getJoins(hashJoinsChannel(channel, codecC))

  def getData(channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[internal.Datum[A]]] =
    hashHistoryReader.getData(hashDataChannel(channel, codecC))

  def getContinuations(channels: Seq[C])(
      implicit serializeC: Serialize[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    hashHistoryReader.getContinuations(hashContinuationsChannels(channels, serializeC))

  def getRichData(channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[RichDatum[A]]] =
    hashHistoryReader.getRichDatums(hashDataChannel(channel, codecC))

  def getRichJoins(channel: C)(
      implicit codecC: Codec[C]
  ): F[Seq[RichJoin[C]]] =
    hashHistoryReader.getRichJoins(hashJoinsChannel(channel, codecC))

  def getRichContinuations(channels: Seq[C])(
      implicit serializeC: Serialize[C]
  ): F[Seq[RichKont[P, K]]] =
    hashHistoryReader.getRichContinuations(hashContinuationsChannels(channels, serializeC))

  /**
    * get from channel hash written in event log (TODO to be removed)
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
      data <- hashHistoryReader.getData(dataHash)
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
      data <- hashHistoryReader.getJoins(joinHash)
    } yield data
  def getContinuationFromChannelHash(
      channelHash: Blake2b256Hash
  )(implicit channelStore: ChannelStore[F, C]): F[Seq[internal.WaitingContinuation[P, K]]] =
    for {
      maybeContinuationHash <- channelStore.getChannelHash(channelHash)
      continuationHash <- maybeContinuationHash match {
                           case Some(ContinuationHash(continuationHash)) => continuationHash.pure[F]
                           case _ =>
                             Sync[F].raiseError[Blake2b256Hash](
                               new Exception(
                                 s"not found continuation hash for $channelHash in channel store"
                               )
                             )
                         }
      continuations <- hashHistoryReader.getContinuations(continuationHash)
    } yield continuations
}
