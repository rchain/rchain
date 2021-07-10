package coop.rchain.rspace.channelStore.instances

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.hashing.Blake2b256Hash.codecPureBlake2b256Hash
import coop.rchain.rspace.hashing.ChannelHash._
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import scodec.Codec
import scodec.codecs.{discriminated, uint2}

import java.util.Comparator

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

    // Get lexical Ordering for ByteString
    implicit val bsLexicalOrdering: Ordering[ByteString] =
      Ordering.comparatorToOrdering(ByteString.unsignedLexicographicalComparator())

    override def putChannelHashes(channels: Seq[C]): F[Unit] = {
      def convert(channel: C): F[(Blake2b256Hash, DataJoinHash)] =
        for {
          // C => hash(C)
          eventKey <- Sync[F].delay(StableHashProvider.hash(channel)(sc))

          // C => hashPrefix(C)
          dataHash = hashDataChannel(channel, sc)
          joinHash = hashJoinsChannel(channel, sc)
        } yield (eventKey, DataJoinHash(dataHash, joinHash))

      channels.toList
        .traverse(convert)
        .flatMap(kvs => store.put(kvs.sortBy({ case (k, _) => k.toByteString })(bsLexicalOrdering)))
    }

    override def putContinuationHashes(conts: Seq[Seq[C]]): F[Unit] = {
      def convert(channels: Seq[C]): F[(Blake2b256Hash, ContinuationHash)] =
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
        } yield (eventKey, ContinuationHash(continuationHash))

      conts.toList
        .traverse(convert)
        .flatMap(kvs => store.put(kvs.sortBy({ case (k, _) => k.toByteString })(bsLexicalOrdering)))
    }
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
    override def putChannelHash(channel: C): F[Unit]                   = ().pure[F]
    override def putContinuationHash(channels: Seq[C]): F[Unit]        = ().pure[F]
    override def putChannelHashes(channel: Seq[C]): F[Unit]            = ().pure[F]
    override def putContinuationHashes(channels: Seq[Seq[C]]): F[Unit] = ().pure[F]
    override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] =
      none[ChannelHash].pure[F]
  }
}
