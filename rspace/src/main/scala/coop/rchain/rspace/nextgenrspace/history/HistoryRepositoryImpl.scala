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
  Serialize
}
import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import HistoryRepositoryImpl._
import com.typesafe.scalalogging.Logger

final case class HistoryRepositoryImpl[F[_]: Sync, C, P, A, K](
    history: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdStore[F]
)(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K])
    extends HistoryRepository[F, C, P, A, K] {
  val joinSuffixBits                = BitVector("-joins".getBytes(StandardCharsets.UTF_8))
  val dataSuffixBits                = BitVector("-data".getBytes(StandardCharsets.UTF_8))
  val continuationSuffixBits        = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))
  val codecJoin: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  implicit val serializeC: Serialize[C] = Serialize.fromCodec(codecC)

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

  private def hashWithSuffix(bits: BitVector, suffix: BitVector): Blake2b256Hash = {
    val suffixed = bits ++ suffix
    Blake2b256Hash.create(suffixed.toByteVector)
  }

  private def hashJoinsChannel(channel: C): Blake2b256Hash =
    hashWithSuffix(codecC.encode(channel).get, joinSuffixBits)

  private def hashContinuationsChannels(channels: Seq[C]): Blake2b256Hash = {
    val chs = channels
      .map(c => serializeC.encode(c))
      .sorted(util.ordByteVector)
    val channelsBits = codecSeq(codecByteVector).encode(chs).get
    hashWithSuffix(channelsBits, continuationSuffixBits)
  }

  private def hashDataChannel(channel: C): Blake2b256Hash =
    hashWithSuffix(codecC.encode(channel).get, dataSuffixBits)

  def decode[R](bv: ByteVector)(implicit codecR: Codec[R]): Seq[R] =
    Codec.decode[Seq[R]](bv.bits).get.value

  override def getJoins(channel: C): F[Seq[Seq[C]]] =
    fetchData(hashJoinsChannel(channel)).flatMap {
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
    fetchData(hashDataChannel(channel)).flatMap {
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
    fetchData(hashContinuationsChannels(channels)).flatMap {
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

  type Result = (Blake2b256Hash, Option[PersistedData], HistoryAction)
  case class MeasureJurnal(
      key: String,
      size: Long,
      action: String,
      datumSize: Int,
      datumLen: Long,
      continuationsLength: Long,
      continuationsSize: Int
  )

  protected[this] val dataLogger: Logger = Logger("coop.rchain.rspace.datametrics")
  protected def asMeasureJournal(actions: List[HotStoreAction]): List[MeasureJurnal] =
    actions.map {
      case i: InsertData[C, A] =>
        val key      = hashDataChannel(i.channel)
        val data     = encodeData(i.data)
        val dataLeaf = DataLeaf(data)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = data.toBitVector.size,
          action = "InsertData",
          datumSize = 0,
          datumLen = dataLeaf.bytes.size,
          continuationsLength = 0L,
          continuationsSize = 0
        )
      case i: InsertContinuations[C, P, K] =>
        val key               = hashContinuationsChannels(i.channels)
        val data              = encodeContinuations(i.continuations)
        val continuationsLeaf = ContinuationsLeaf(data)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = data.size,
          action = "InsertContinu",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = continuationsLeaf.bytes.toBitVector.size,
          continuationsSize = continuationsLeaf.bytes.size.toInt
        )
      case i: InsertJoins[C] =>
        val key       = hashJoinsChannel(i.channel)
        val data      = encodeJoins(i.joins)
        val joinsLeaf = JoinsLeaf(data)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = data.size,
          action = "InsertContinu",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = joinsLeaf.bytes.toBitVector.size,
          continuationsSize = joinsLeaf.bytes.size.toInt
        )
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = 0,
          action = "DeleteData",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = 0,
          continuationsSize = 0
        )
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = 0,
          action = "DeleteData",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = 0,
          continuationsSize = 0
        )
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel)
        MeasureJurnal(
          key = key.bytes.toHex,
          size = 0,
          action = "DeleteData",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = 0,
          continuationsSize = 0
        )

    }

  private def transform(actions: List[HotStoreAction]): List[Result] =
    actions.map {
      case i: InsertData[C, A] =>
        val key      = hashDataChannel(i.channel)
        val data     = encodeData(i.data)
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (dataHash, Some(dataLeaf), InsertAction(key.bytes.toSeq.toList, dataHash))
      case i: InsertContinuations[C, P, K] =>
        val key               = hashContinuationsChannels(i.channels)
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
        val key = hashDataChannel(d.channel)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels)
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
      trieActions <- Applicative[F].pure(transform(actions))
      journal     = asMeasureJournal(actions)
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
