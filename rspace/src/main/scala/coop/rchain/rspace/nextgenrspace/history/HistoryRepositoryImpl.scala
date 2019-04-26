package coop.rchain.rspace.nextgenrspace.history

import java.nio.charset.StandardCharsets

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.{
  internal,
  util,
  Blake2b256Hash,
  DeleteContinuations,
  DeleteData,
  DeleteJoins,
  HotStoreAction,
  InsertContinuations,
  InsertData,
  InsertJoins,
  Serialize,
  StableHashProvider
}
import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import HistoryRepositoryImpl._

final case class HistoryRepositoryImpl[F[_]: Sync, C, P, A, K](
    history: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdStore[F]
)(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K])
    extends HistoryRepository[F, C, P, A, K] {
  val joinSuffixBits                = BitVector.apply("-joins".getBytes(StandardCharsets.UTF_8))
  val codecJoin: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  implicit val serializeC: Serialize[C] = Serialize.fromCodec(codecC)

  private def hashChannels(channels: Seq[C]): Blake2b256Hash =
    StableHashProvider.hash(channels)

  private def hashChannel(channel: C): Blake2b256Hash =
    StableHashProvider.hash(channel)

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

  private def hashJoinsChannel(channel: C): Blake2b256Hash = {
    val channelBits     = codecC.encode(channel).get
    val joinChannelBits = channelBits ++ joinSuffixBits
    Blake2b256Hash.create(joinChannelBits.toByteVector)
  }

  def decode[R](bv: ByteVector)(implicit codecR: Codec[R]): Seq[R] =
    Codec.decode[Seq[R]](bv.bits).get.value

  override def getJoins(channel: C): F[Seq[Seq[C]]] =
    fetchData(hashJoinsChannel(channel)).flatMap {
      case Some(JoinsLeaf(bytes)) =>
        decode[Seq[C]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[Seq[C]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for joins at key $channel, data: $p"
          )
        )
      case None => Seq.empty[Seq[C]].pure[F]
    }

  override def getData(channel: C): F[Seq[internal.Datum[A]]] =
    fetchData(hashChannel(channel)).flatMap {
      case Some(DataLeaf(bytes)) =>
        decode[internal.Datum[A]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.Datum[A]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for data at key $channel, data: $p"
          )
        )
      case None => Seq.empty[internal.Datum[A]].pure[F]
    }

  override def getContinuations(channels: Seq[C]): F[Seq[internal.WaitingContinuation[P, K]]] =
    fetchData(hashChannels(channels)).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        decode[internal.WaitingContinuation[P, K]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.WaitingContinuation[P, K]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for continuations at key $channels, data: $p"
          )
        )
      case None => Seq.empty[internal.WaitingContinuation[P, K]].pure[F]
    }

  type Result = (Blake2b256Hash, Option[PersistedData], HistoryAction)

  private def transform(actions: List[HotStoreAction]): List[Result] =
    actions.map {
      case i: InsertData[C, A] =>
        val key      = hashChannel(i.channel)
        val data     = encodeData(i.data)
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (dataHash, Some(dataLeaf), InsertAction(key.bytes.toSeq.toList, dataHash))
      case i: InsertContinuations[C, P, K] =>
        val key               = hashChannels(i.channels)
        val data              = encodeContinuations(i.continuations)
        val continuationsLeaf = ContinuationsLeaf(data)
        val continuationsHash = Blake2b256Hash.create(data)
        (
          continuationsHash,
          Some(continuationsLeaf),
          InsertAction(key.bytes.toSeq.toList, continuationsHash)
        )
      case i: InsertJoins[C] =>
        val key       = hashJoinsChannel(i.channel)
        val data      = encodeJoins(i.joins)
        val joinsLeaf = JoinsLeaf(data)
        val joinsHash = Blake2b256Hash.create(data)
        (joinsHash, Some(joinsLeaf), InsertAction(key.bytes.toSeq.toList, joinsHash))
      case d: DeleteData[C] =>
        val key = hashChannel(d.channel)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
      case d: DeleteContinuations[C] =>
        val key = hashChannels(d.channels)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
    }

  private def storeLeaves(leafs: List[Result]): F[List[HistoryAction]] =
    leafs.traverse {
      case (key, Some(data), historyAction) =>
        leafStore.put(key, data).map(_ => historyAction)
      case (_, None, historyAction) =>
        Applicative[F].pure(historyAction)
    }

  override def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] =
    for {
      trieActions    <- Applicative[F].pure(transform(actions))
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
