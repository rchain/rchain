package coop.rchain.rspace.nextgenrspace.history

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.{internal, util, Blake2b256Hash, HotStoreAction}
import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.ByteVector
import HistoryRepositoryImpl._
import com.typesafe.scalalogging.Logger
import HistoryTransformers._

final case class HistoryRepositoryImpl[F[_]: Sync, C, P, A, K](
    history: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdStore[F]
)(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K])
    extends HistoryRepository[F, C, P, A, K] {
  val codecJoin: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  private val historyTransformer =
    HistoryTransformers.create[C, P, A, K]()(codecC, codecP, codecA, codecK)

  private def fetchData(key: Blake2b256Hash): F[Option[PersistedData]] =
    history.findPath(key.bytes.toSeq.toList).flatMap {
      case (trie, _) =>
        trie match {
          case Leaf(dataHash) => leafStore.get(dataHash)
          case EmptyTrie      => Applicative[F].pure(None)
          case _ =>
            Sync[F].raiseError(new RuntimeException(s"unexpected data at key $key, data: $trie"))
        }
    }

  override def getJoins(channel: C): F[Seq[Seq[C]]] =
    fetchData(historyTransformer.hashJoinsChannel(channel)).flatMap {
      case Some(JoinsLeaf(bytes)) =>
        decodeJoins[C](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[Seq[C]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for joins at key $channel, data: $p"
          )
        )
      case None => Seq.empty[Seq[C]].pure[F]
    }

  override def getData(channel: C): F[Seq[internal.Datum[A]]] =
    fetchData(historyTransformer.hashDataChannel(channel)).flatMap {
      case Some(DataLeaf(bytes)) =>
        decodeSorted[internal.Datum[A]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.Datum[A]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for data at key $channel, data: $p"
          )
        )
      case None => Seq.empty[internal.Datum[A]].pure[F]
    }

  override def getContinuations(channels: Seq[C]): F[Seq[internal.WaitingContinuation[P, K]]] =
    fetchData(historyTransformer.hashContinuationsChannels(channels)).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        decodeSorted[internal.WaitingContinuation[P, K]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.WaitingContinuation[P, K]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for continuations at key $channels, data: $p"
          )
        )
      case None => Seq.empty[internal.WaitingContinuation[P, K]].pure[F]
    }
  private def storeLeaves(leafs: List[Result]): F[List[HistoryAction]] =
    leafs.traverse {
      case (key, Some(data), historyAction) =>
        leafStore.put(key, data).map(_ => historyAction)
      case (_, None, historyAction) =>
        Applicative[F].pure(historyAction)
    }

  protected[this] val dataLogger: Logger = Logger("coop.rchain.rspace.datametrics")

  override def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] =
    for {
      trieActions <- Applicative[F].pure(historyTransformer.transform(actions))
      journal     = historyTransformer.asMeasureJournal(actions)
      _ = journal.foreach(
        j =>
          dataLogger.debug(
            s"${j.key};${j.size};${j.action};${j.datumSize};${j.datumLen};${j.continuationsLength};${j.continuationsSize}"
          )
      )
      historyActions <- storeLeaves(trieActions)
      next           <- history.process(historyActions)
      _              <- rootsRepository.commit(next.root)
    } yield this.copy(history = next)

  override def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]] =
    for {
      _    <- rootsRepository.validateRoot(root)
      next = history.copy(root = root)
    } yield this.copy(history = next)

  override def close(): F[Unit] =
    for {
      _ <- leafStore.close()
      _ <- rootsRepository.close()
      _ <- history.close()
    } yield ()
}

object HistoryRepositoryImpl {
  val codecSeqByteVector: Codec[Seq[ByteVector]] = codecSeq(codecByteVector)

  private def decodeSorted[D](data: ByteVector)(implicit codec: Codec[D]): Seq[D] =
    codecSeqByteVector.decode(data.bits).get.value.map(bv => codec.decode(bv.bits).get.value)

  private def encodeSorted[D](data: Seq[D])(implicit codec: Codec[D]): ByteVector =
    codecSeqByteVector
      .encode(
        data
          .map(d => Codec.encode[D](d).get.toByteVector)
          .sorted(util.ordByteVector)
      )
      .get
      .toByteVector

  def encodeData[A](data: Seq[internal.Datum[A]])(implicit codec: Codec[Datum[A]]): ByteVector =
    encodeSorted(data)

  def encodeContinuations[P, K](
      continuations: Seq[internal.WaitingContinuation[P, K]]
  )(implicit codec: Codec[WaitingContinuation[P, K]]): ByteVector =
    encodeSorted(continuations)

  def decodeJoins[C](data: ByteVector)(implicit codec: Codec[C]): Seq[Seq[C]] =
    codecSeqByteVector
      .decode(data.bits)
      .get
      .value
      .map(
        bv => codecSeqByteVector.decode(bv.bits).get.value.map(v => codec.decode(v.bits).get.value)
      )

  def encodeJoins[C](joins: Seq[Seq[C]])(implicit codec: Codec[C]): ByteVector =
    codecSeqByteVector
      .encode(
        joins
          .map(
            channels => encodeSorted(channels)
          )
          .sorted(util.ordByteVector)
      )
      .get
      .toByteVector
}
