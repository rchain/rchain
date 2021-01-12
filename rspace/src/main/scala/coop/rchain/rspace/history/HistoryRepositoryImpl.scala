package coop.rchain.rspace.history

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, Parallel}
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.history.HistoryRepositoryImpl._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.history.ColdStoreInstances.ColdKeyValueStore
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
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
  RSpace
}
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

final case class HistoryRepositoryImpl[F[_]: Sync: Parallel, C, P, A, K](
    history: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdKeyValueStore[F],
    rspaceExporter: RSpaceExporter[F],
    rspaceImporter: RSpaceImporter[F]
)(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K])
    extends HistoryRepository[F, C, P, A, K] {
  val joinSuffixBits                = BitVector("-joins".getBytes(StandardCharsets.UTF_8))
  val dataSuffixBits                = BitVector("-data".getBytes(StandardCharsets.UTF_8))
  val continuationSuffixBits        = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))
  val codecJoin: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  implicit val serializeC: Serialize[C] = Serialize.fromCodec(codecC)

  private def fetchData(key: Blake2b256Hash): F[Option[PersistedData]] =
    history.find(key.bytes.toSeq.toList).flatMap {
      case (trie, _) =>
        trie match {
          case LeafPointer(dataHash) => leafStore.get(dataHash)
          case EmptyPointer          => Applicative[F].pure(None)
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

  protected[this] val dataLogger: Logger =
    Logger("coop.rchain.rspace.datametrics")

  private def measure(actions: List[HotStoreAction]): F[Unit] =
    Sync[F].delay(
      dataLogger.whenDebugEnabled {
        computeMeasure(actions).foreach(p => dataLogger.debug(p))
      }
    )

  private def computeMeasure(actions: List[HotStoreAction]): List[String] =
    actions.map {
      case i: InsertData[C, A] =>
        val key  = hashDataChannel(i.channel).bytes
        val data = encodeData(i.data)
        s"${key.toHex};insert-data;${data.length};${i.data.length}"
      case i: InsertContinuations[C, P, K] =>
        val key  = hashContinuationsChannels(i.channels).bytes
        val data = encodeContinuations(i.continuations)
        s"${key.toHex};insert-continuation;${data.length};${i.continuations.length}"
      case i: InsertJoins[C] =>
        val key  = hashJoinsChannel(i.channel).bytes
        val data = encodeJoins(i.joins)
        s"${key.toHex};insert-join;${data.length}"
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel).bytes
        s"${key.toHex};delete-data;0"
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels).bytes
        s"${key.toHex};delete-continuation;0"
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel).bytes
        s"${key.toHex};delete-join;0"
    }

  private def transformAndStore(hotStoreActions: List[HotStoreAction]): F[List[HistoryAction]] =
    for {
      transformedActions <- Sync[F].delay(hotStoreActions.map(transform))
      historyActions     <- storeLeaves(transformedActions)
    } yield historyActions

  private def transform(action: HotStoreAction): Result =
    action match {
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

  private def storeLeaves(leafs: List[Result]): F[List[HistoryAction]] = {
    val toBeStored = leafs.collect { case (key, Some(data), _) => (key, data) }
    leafStore.putIfAbsent(toBeStored).map(_ => leafs.map(_._3))
  }

  override def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] = {
    import cats.instances.list._
    val batchSize = Math.max(1, actions.size / RSpace.parallelism)
    for {
      batches        <- Sync[F].delay(actions.grouped(batchSize).toList)
      historyActions <- batches.parFlatTraverse(transformAndStore)
      next           <- history.process(historyActions)
      _              <- rootsRepository.commit(next.root)
      _              <- measure(actions)
    } yield this.copy(history = next)
  }

  override def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]] =
    for {
      _    <- rootsRepository.validateAndSetCurrentRoot(root)
      next = history.reset(root = root)
    } yield this.copy(history = next)

  override def exporter: F[RSpaceExporter[F]] = Sync[F].delay(rspaceExporter)

  override def importer: F[RSpaceImporter[F]] = Sync[F].delay(rspaceImporter)
}

object HistoryRepositoryImpl {
  val codecSeqByteVector: Codec[Seq[ByteVector]] = codecSeq(codecByteVector)

  def decodeSorted[D](data: ByteVector)(implicit codec: Codec[D]): Seq[D] =
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
