package coop.rchain.rspace.channelStore.instances

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.Hasher.{hashContinuationsChannels, hashDataChannel, hashJoinsChannel}
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.internal.{toOrderedByteVectors}
import coop.rchain.rspace.{Blake2b256Hash, StableHashProvider}
import coop.rchain.shared.Serialize
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import coop.rchain.shared.syntax._
import scodec.Codec
import scodec.codecs.{discriminated, uint2}

object ChannelStoreImpl {
  def apply[F[_]: Sync, C](store: KeyValueStore[F], sc: Serialize[C], codecC: Codec[C]) = {
    val kvStore = store.toTypedStore(Blake2b256Hash.codecPureBlake2b256Hash, codecChannelHash)
    ChannelStoreImpl(kvStore, sc, codecC)
  }

  final case class ChannelStoreImpl[F[_]: Sync, C](
      store: KeyValueTypedStore[F, Blake2b256Hash, ChannelHash],
      sc: Serialize[C],
      codecC: Codec[C]
  ) extends ChannelStore[F, C] {
    implicit val serializeC: Serialize[C]    = sc
    val continuationSerializeC: Serialize[C] = Serialize.fromCodec(codecC)

    override def putChannelHash(channel: C): F[Unit] =
      for {
        eventKey <- StableHashProvider.hash(channel)(sc).pure[F]
        dataHash = hashDataChannel(channel, codecC)
        joinHash = hashJoinsChannel(channel, codecC)
        _        <- store.put(eventKey, DataJoinHash(dataHash, joinHash))
      } yield ()

    override def putContinuationHash(channels: Seq[C]): F[Unit] =
      for {
        channelsHashes   <- toOrderedByteVectors(channels).map(Blake2b256Hash.create).pure[F]
        eventKey         = continuationKey(channelsHashes)
        continuationHash = hashContinuationsChannels(channels, continuationSerializeC)
        _                <- store.put(eventKey, ContinuationHash(continuationHash))
      } yield ()

    override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] = store.get(hash)

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
