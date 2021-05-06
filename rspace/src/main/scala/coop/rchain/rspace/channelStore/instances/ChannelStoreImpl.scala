package coop.rchain.rspace.channelStore.instances

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.Blake2b256Hash.codecPureBlake2b256Hash
import coop.rchain.rspace.Hasher.{hashContinuationsChannels, hashDataChannel, hashJoinsChannel}
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.rspace.{Blake2b256Hash, StableHashProvider}
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import scodec.Codec
import scodec.codecs.{discriminated, uint2}

object ChannelStoreImpl {
  def apply[F[_]: Sync, C](
      store: KeyValueStore[F],
      sc: Serialize[C]
  ) =
    ChannelStoreImpl(store.toTypedStore(codecPureBlake2b256Hash, codecChannelHash), sc)

  final case class ChannelStoreImpl[F[_]: Sync, C](
      store: KeyValueTypedStore[F, Blake2b256Hash, ChannelHash],
      sc: Serialize[C]
  ) extends ChannelStore[F, C] {
    override def putChannelHash(channel: C): F[Unit] =
      for {
        // C => hash(C)
        eventKey <- Sync[F].delay(StableHashProvider.hash(channel)(sc))

        // C => hashPrefix(C)
        dataHash = hashDataChannel(channel, sc)
        joinHash = hashJoinsChannel(channel, sc)
        _        <- store.put(eventKey, DataJoinHash(dataHash, joinHash))
      } yield ()

    override def putContinuationHash(channels: Seq[C]): F[Unit] =
      for {
        // Hash each channel in a list
        channelsHashes <- Sync[F].delay(
                           toOrderedByteVectors(channels)(sc).map(Blake2b256Hash.create)
                         )
        // Concatenate channel hashes and hash result
        // Seq[C] => Seq[hash(C)] => Seq[bytes(C)] => bytes(C) => hash(C)
        eventKey = continuationKey(channelsHashes)

        // Concatenate channels and hash result
        // Seq[C] => Seq[bytes(C)] => bytes(C) => hashPrefix(C)
        continuationHash = hashContinuationsChannels(channels, sc)
        _                <- store.put(eventKey, ContinuationHash(continuationHash))
      } yield ()

    override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] = store.get(hash)
  }

  def codecChannelHash: Codec[ChannelHash] =
    discriminated[ChannelHash]
      .by(uint2)
      .subcaseP(0) {
        case (dataJoinHash: DataJoinHash) => dataJoinHash
      }(Codec[DataJoinHash])
      .subcaseP(1) {
        case continuationHash: ContinuationHash => continuationHash
      }(Codec[ContinuationHash])

  /**
    * No operation implementation of [[ChannelStore]]
    *
    * Useful in places where we want to have disabled or dummy storage.
    */
  final case class NoOpChannelStore[F[_]: Applicative, C]() extends ChannelStore[F, C] {
    override def putChannelHash(channel: C): F[Unit]            = ().pure[F]
    override def putContinuationHash(channels: Seq[C]): F[Unit] = ().pure[F]
    override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] =
      none[ChannelHash].pure[F]
  }
}
