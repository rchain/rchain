package coop.rchain.rspace.channelStore.instances

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.Hasher.{hashContinuationsChannels, hashDataChannel, hashJoinsChannel}
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.Store
import coop.rchain.rspace.internal.{toOrderedByteVectors, RichAttempt}
import coop.rchain.rspace.{Blake2b256Hash, StableHashProvider}
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.codecs.{discriminated, uint2}

object ChannelStoreImpl {
  def apply[F[_]: Sync, C](store: Store[F], sc: Serialize[C], codecC: Codec[C]) =
    ChannelStoreImpl(store, sc, codecC)

  final case class ChannelStoreImpl[F[_]: Sync, C](
      store: Store[F],
      sc: Serialize[C],
      codecC: Codec[C]
  ) extends ChannelStore[F, C] {
    implicit val serializeC: Serialize[C]    = sc
    val continuationSerializeC: Serialize[C] = Serialize.fromCodec(codecC)
    val channelHashCodec                     = codecChannelHash

    override def putChannelHash(channel: C): F[Unit] =
      for {
        eventKey <- StableHashProvider.hash(channel)(sc).pure[F]
        dataHash = hashDataChannel(channel, codecC)
        joinHash = hashJoinsChannel(channel, codecC)
        _        <- store.put(eventKey, channelHashCodec.encode(DataJoinHash(dataHash, joinHash)).get)
      } yield ()

    override def putContinuationHash(channels: Seq[C]): F[Unit] =
      for {
        channelsHashes   <- toOrderedByteVectors(channels).map(Blake2b256Hash.create).pure[F]
        eventKey         = continuationKey(channelsHashes)
        continuationHash = hashContinuationsChannels(channels, continuationSerializeC)
        _                <- store.put(eventKey, channelHashCodec.encode(ContinuationHash(continuationHash)).get)
      } yield ()

    override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] =
      for {
        itemOpt <- store.get(hash)
        result = itemOpt.map(
          channelHashCodec.decode(_).get.value
        )
      } yield result

    override def close(): F[Unit] = store.close()
  }

  def continuationKey(channels: Seq[Blake2b256Hash]): Blake2b256Hash =
    Blake2b256Hash.create(channels.map(_.toByteString.toByteArray).foldLeft(Array[Byte]())(_ ++ _))

  def codecChannelHash: Codec[ChannelHash] =
    discriminated[ChannelHash]
      .by(uint2)
      .subcaseP(0) {
        case (dataJoinHash: DataJoinHash) => dataJoinHash
      }(Codec[DataJoinHash])
      .subcaseP(1) {
        case continuationHash: ContinuationHash => continuationHash
      }(Codec[ContinuationHash])
}
