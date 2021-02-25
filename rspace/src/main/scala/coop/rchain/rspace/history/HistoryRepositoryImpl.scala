package coop.rchain.rspace.history

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Applicative, Parallel}
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.Hasher.{hashContinuationsChannels, hashDataChannel, hashJoinsChannel}
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore}
import coop.rchain.rspace.history.HistoryRepositoryImpl._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.merger.StateMerger
import coop.rchain.rspace.merger.instances.StateMergerImpl.StateMergerImpl
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
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import scodec.Codec
import scodec.bits.ByteVector

final case class HistoryRepositoryImpl[F[_]: Concurrent: Parallel: Log: Span, C, P, A, K](
    currentHistory: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdKeyValueStore[F],
    rspaceExporter: RSpaceExporter[F],
    rspaceImporter: RSpaceImporter[F],
    // Map channel hash in event log -> channel hash in history
    // We need to maintain this for event log merge
    channelHashesStore: ChannelStore[F, C],
    sc: Serialize[C],
    rSpaceCache: HistoryCache[F, C, P, A, K]
)(
    implicit codecC: Codec[C],
    codecP: Codec[P],
    codecA: Codec[A],
    codecK: Codec[K]
) extends HistoryRepository[F, C, P, A, K] {

  implicit val serializeC: Serialize[C] = Serialize.fromCodec(codecC)

  implicit val ms = Metrics.Source(RSpaceMetricsSource, "history")

  val datumsCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichDatum[A]]] =
    rSpaceCache.dtsCache
  val contsCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichKont[P, K]]] =
    rSpaceCache.wksCache
  val joinsCache: LazyAdHocKeyValueCache[F, HistoryPointer, Seq[RichJoin[C]]] =
    rSpaceCache.jnsCache

  override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] =
    channelHashesStore.getChannelHash(hash)

  override def putChannelHash(channel: C): F[Unit] = channelHashesStore.putChannelHash(channel)

  override def putContinuationHash(channels: Seq[C]): F[Unit] =
    channelHashesStore.putContinuationHash(channels)

  override def getJoins(hash: Blake2b256Hash): F[Seq[Seq[C]]] =
    fetchData(hash).flatMap {
      case Some(JoinsLeaf(bytes)) =>
        decodeJoins[C](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[Seq[C]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for continuations at key $hash, data: $p"
          )
        )
      case None => Seq.empty[Seq[C]].pure[F]
    }

  override def getJoins(channel: C): F[Seq[Seq[C]]] =
    getJoins(hashJoinsChannel(channel, codecC))

  override def getData(channelHash: Blake2b256Hash): F[Seq[Datum[A]]] =
    fetchData(channelHash).flatMap {
      case Some(DataLeaf(bytes)) =>
        decodeSorted[internal.Datum[A]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.Datum[A]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for data at key $channelHash, data: $p"
          )
        )
      case None => Seq.empty[internal.Datum[A]].pure[F]
    }

  override def getDataWithBytes(channelHash: Blake2b256Hash): F[Seq[(Datum[A], ByteVector)]] =
    fetchData(channelHash).flatMap {
      case Some(DataLeaf(bytes)) =>
        decodeSortedWithBytes[internal.Datum[A]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[(internal.Datum[A], ByteVector)]](
          new RuntimeException(
            s"Found unexpected leaf while looking for data at key $channelHash, data: $p"
          )
        )
      case None => Seq.empty[(internal.Datum[A], ByteVector)].pure[F]
    }

  override def getData(channel: C): F[Seq[internal.Datum[A]]] =
    getData(hashDataChannel(channel, codecC))

  override def getContinuations(hash: Blake2b256Hash): F[Seq[WaitingContinuation[P, K]]] =
    fetchData(hash).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        decodeSorted[internal.WaitingContinuation[P, K]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[internal.WaitingContinuation[P, K]]](
          new RuntimeException(
            s"Found unexpected leaf while looking for continuations at key $hash, data: $p"
          )
        )
      case None => Seq.empty[internal.WaitingContinuation[P, K]].pure[F]
    }

  // This methods returning raw bytes along with decode value is performnce optimisatoin
  // Making diff for two Seq[(WaitingContinuation[P, K]] is 5-10 tims slower then Seq[ByteVector],
  // so the second val of the tuple is exposed to compare values
  override def getContinuationsWithBytes(
      hash: Blake2b256Hash
  ): F[Seq[(WaitingContinuation[P, K], ByteVector)]] =
    fetchData(hash).flatMap {
      case Some(ContinuationsLeaf(bytes)) =>
        decodeSortedWithBytes[internal.WaitingContinuation[P, K]](bytes).pure[F]
      case Some(p) =>
        Sync[F].raiseError[Seq[(WaitingContinuation[P, K], ByteVector)]](
          new RuntimeException(
            s"Found unexpected leaf while looking for continuations at key $hash, data: $p"
          )
        )
      case None => Seq.empty[(WaitingContinuation[P, K], ByteVector)].pure[F]
    }

  override def getContinuations(channels: Seq[C]): F[Seq[internal.WaitingContinuation[P, K]]] =
    getContinuations(hashContinuationsChannels(channels, serializeC))

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
    actions.par.map {
      case i: InsertData[C, A] =>
        val key  = hashDataChannel(i.channel, codecC).bytes
        val data = encodeData(i.data)
        s"${key.toHex};insert-data;${data.length};${i.data.length}"
      case i: InsertContinuations[C, P, K] =>
        val key  = hashContinuationsChannels(i.channels, serializeC).bytes
        val data = encodeContinuations(i.continuations)
        s"${key.toHex};insert-continuation;${data.length};${i.continuations.length}"
      case i: InsertJoins[C] =>
        val key  = hashJoinsChannel(i.channel, codecC).bytes
        val data = encodeJoins(i.joins)
        s"${key.toHex};insert-join;${data.length}"
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel, codecC).bytes
        s"${key.toHex};delete-data;0"
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels, serializeC).bytes
        s"${key.toHex};delete-continuation;0"
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel, codecC).bytes
        s"${key.toHex};delete-join;0"
    }.toList

  private def transformAndStore(hotStoreActions: List[HotStoreAction]): F[List[HistoryAction]] = {
    import cats.instances.list._
    for {
      transformedActions <- Sync[F].delay(hotStoreActions.map(transform))
      historyActions     <- storeLeaves(transformedActions)
      _                  <- hotStoreActions.traverse(storeChannelHash)
    } yield historyActions
  }

  private def storeChannelHash(action: HotStoreAction) =
    action match {
      case i: InsertData[C, A] =>
        channelHashesStore.putChannelHash(i.channel)
      case i: InsertContinuations[C, P, K] =>
        channelHashesStore.putContinuationHash(i.channels)
      case i: InsertJoins[C] =>
        channelHashesStore.putChannelHash(i.channel)
      case d: DeleteData[C] =>
        channelHashesStore.putChannelHash(d.channel)
      case d: DeleteContinuations[C] =>
        channelHashesStore.putContinuationHash(d.channels)
      case d: DeleteJoins[C] =>
        channelHashesStore.putChannelHash(d.channel)
    }

  private def transform(action: HotStoreAction): Result =
    action match {
      case i: InsertData[C, A] =>
        val key      = hashDataChannel(i.channel, codecC)
        val data     = encodeData(i.data)
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (dataHash, Some(dataLeaf), InsertAction(key.bytes.toSeq.toList, dataHash))
      case i: InsertContinuations[C, P, K] =>
        val key               = hashContinuationsChannels(i.channels, serializeC)
        val data              = encodeContinuations(i.continuations)
        val continuationsLeaf = ContinuationsLeaf(data)
        val continuationsHash = Blake2b256Hash.create(data)
        (
          continuationsHash,
          Some(continuationsLeaf),
          InsertAction(key.bytes.toSeq.toList, continuationsHash)
        )
      case i: InsertJoins[C] =>
        val key       = hashJoinsChannel(i.channel, codecC)
        val data      = encodeJoins(i.joins)
        val joinsLeaf = JoinsLeaf(data)
        val joinsHash = Blake2b256Hash.create(data)
        (joinsHash, Some(joinsLeaf), InsertAction(key.bytes.toSeq.toList, joinsHash))
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel, codecC)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels, serializeC)
        (key, None, DeleteAction(key.bytes.toSeq.toList))
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel, codecC)
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
    } yield this.copy(history = next, channelHashesStore = channelHashesStore)
  }

  override def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]] =
    for {
      _    <- rootsRepository.validateAndSetCurrentRoot(root)
      next = history.reset(root = root)
    } yield this.copy(currentHistory = next)

  override def exporter: F[RSpaceExporter[F]] = Sync[F].delay(rspaceExporter)

  override def importer: F[RSpaceImporter[F]] = Sync[F].delay(rspaceImporter)

  override def stateMerger: F[StateMerger[F]] =
    Sync[F].delay(
      new StateMergerImpl[F, C, P, A, K](
        this,
        history,
        leafStore,
        rootsRepository,
        sc
      )
    )
  override def history: History[F] = currentHistory

  override def root: Blake2b256Hash = currentHistory.root

  override def getHistoryReader(stateHash: Blake2b256Hash): HashHistoryReader[F, C, P, A, K] =
    new CachingHashHistoryReaderImpl(history.reset(root = stateHash), rSpaceCache, leafStore)
}

object HistoryRepositoryImpl {
  def fetchData[F[_]: Sync](
      key: Blake2b256Hash,
      history: History[F],
      leafStore: ColdKeyValueStore[F]
  ): F[Option[PersistedData]] =
    history.find(key.bytes.toSeq.toList).flatMap {
      case (trie, _) =>
        trie match {
          case LeafPointer(dataHash) => leafStore.get(dataHash)
          case EmptyPointer          => Applicative[F].pure(None)
          case _ =>
            Sync[F].raiseError(new RuntimeException(s"unexpected data at key $key, data: $trie"))

        }
    }
}
