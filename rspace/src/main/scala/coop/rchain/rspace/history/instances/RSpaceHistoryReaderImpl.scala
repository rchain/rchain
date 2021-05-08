package coop.rchain.rspace.history.instances

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.hashing.{ChannelHash}
import coop.rchain.rspace.history.ColdStoreInstances.ColdKeyValueStore
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import scodec.bits.ByteVector

class RSpaceHistoryReaderImpl[F[_]: Concurrent, C, P, A, K](
    targetHistory: History[F],
    leafStore: ColdKeyValueStore[F]
)(
    implicit
    sc: Serialize[C],
    sp: Serialize[P],
    sa: Serialize[A],
    sk: Serialize[K]
) extends HistoryReader[F, Blake2b256Hash, C, P, A, K] {

  override def root: Blake2b256Hash = targetHistory.root

  override def getDataProj[R](key: Blake2b256Hash)(proj: (Datum[A], ByteVector) => R): F[Seq[R]] =
    fetchData(key).flatMap {
      case Some(DataLeaf(bytes)) =>
        Sync[F].delay(decodeDatumsProj(bytes)(proj))
      case Some(p) =>
        new RuntimeException(s"Found unexpected leaf while looking for data at key $key, data: $p").raiseError
      case None => Seq[R]().pure[F]
    }

  override def getContinuationsProj[R](key: Blake2b256Hash)(
      proj: (WaitingContinuation[P, K], ByteVector) => R
  ): F[Seq[R]] =
    fetchData(key).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        Sync[F].delay(decodeContinuationsProj[P, K, R](bytes)(proj))
      case Some(p) =>
        new RuntimeException(
          s"Found unexpected leaf while looking for continuations at key $key, data: $p"
        ).raiseError
      case None => Seq[R]().pure[F]
    }

  override def getJoinsProj[R](key: Blake2b256Hash)(proj: (Seq[C], ByteVector) => R): F[Seq[R]] =
    fetchData(key).flatMap {
      case Some(JoinsLeaf(bytes)) =>
        Sync[F].delay(decodeJoinsProj[C, R](bytes)(proj))
      case Some(p) =>
        new RuntimeException(s"Found unexpected leaf while looking for join at key $key, data: $p").raiseError
      case None => Seq[R]().pure[F]
    }

  /** Fetch data on a hash pointer */
  def fetchData(
      key: Blake2b256Hash
  ): F[Option[PersistedData]] =
    targetHistory.find(key.bytes.toSeq.toList).flatMap {
      case (trie, _) =>
        trie match {
          case LeafPointer(dataHash) => leafStore.get(dataHash)
          case EmptyPointer          => none[PersistedData].pure[F]
          case _                     => new RuntimeException(s"unexpected data at key $key, data: $trie").raiseError
        }
    }

  override def base: HistoryReaderBase[F, C, P, A, K] = {
    val historyReader = this

    new HistoryReaderBase[F, C, P, A, K] {
      override def getDataProj[R](key: C): ((Datum[A], ByteVector) => R) => F[Seq[R]] =
        historyReader.getDataProj[R](ChannelHash.hashDataChannel(key, sc))

      override def getContinuationsProj[R](
          key: Seq[C]
      ): ((WaitingContinuation[P, K], ByteVector) => R) => F[Seq[R]] =
        historyReader.getContinuationsProj[R](ChannelHash.hashContinuationsChannels(key, sc))

      override def getJoinsProj[R](key: C): ((Seq[C], ByteVector) => R) => F[Seq[R]] =
        historyReader.getJoinsProj[R](ChannelHash.hashJoinsChannel(key, sc))
    }
  }
}
