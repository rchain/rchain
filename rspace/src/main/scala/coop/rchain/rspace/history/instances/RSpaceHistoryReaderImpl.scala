package coop.rchain.rspace.history.instances

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
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
    fetchData(HistoryRepositoryInstances.PREFIX_DATUM, key).flatMap {
      case Some(DataLeaf(bytes)) =>
        Sync[F].delay(decodeDatumsProj(bytes)(proj))
      case Some(p) =>
        new RuntimeException(s"Found unexpected leaf while looking for data at key $key, data: $p").raiseError
      case None => Seq[R]().pure[F]
    }

  override def getContinuationsProj[R](key: Blake2b256Hash)(
      proj: (WaitingContinuation[P, K], ByteVector) => R
  ): F[Seq[R]] =
    fetchData(HistoryRepositoryInstances.PREFIX_KONT, key).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        Sync[F].delay(decodeContinuationsProj[P, K, R](bytes)(proj))
      case Some(p) =>
        new RuntimeException(
          s"Found unexpected leaf while looking for continuations at key $key, data: $p"
        ).raiseError
      case None => Seq[R]().pure[F]
    }

  override def getJoinsProj[R](key: Blake2b256Hash)(proj: (Seq[C], ByteVector) => R): F[Seq[R]] =
    fetchData(HistoryRepositoryInstances.PREFIX_JOINS, key).flatMap {
      case Some(JoinsLeaf(bytes)) =>
        Sync[F].delay(decodeJoinsProj[C, R](bytes)(proj))
      case Some(p) =>
        new RuntimeException(s"Found unexpected leaf while looking for join at key $key, data: $p").raiseError
      case None => Seq[R]().pure[F]
    }

  /** Fetch data on a hash pointer */
  def fetchData(
      prefix: Byte,
      key: Blake2b256Hash
  ): F[Option[PersistedData]] =
    targetHistory.read(prefix +: key.bytes).flatMap { dataHashOpt =>
      val dataKey = dataHashOpt.map(x => Blake2b256Hash.fromByteArray(x.toArray))
      dataKey.flatTraverse(leafStore.get(_))
    }

  override def base: HistoryReaderBase[F, C, P, A, K] = {
    val historyReader = this

    new HistoryReaderBase[F, C, P, A, K] {
      override def getDataProj[R](key: C): ((Datum[A], ByteVector) => R) => F[Seq[R]] =
        historyReader.getDataProj[R](StableHashProvider.hash(key)(sc))

      override def getContinuationsProj[R](
          key: Seq[C]
      ): ((WaitingContinuation[P, K], ByteVector) => R) => F[Seq[R]] =
        historyReader.getContinuationsProj[R](StableHashProvider.hash(key)(sc))

      override def getJoinsProj[R](key: C): ((Seq[C], ByteVector) => R) => F[Seq[R]] =
        historyReader.getJoinsProj[R](StableHashProvider.hash(key)(sc))
    }
  }
}
