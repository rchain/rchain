package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace._
import coop.rchain.rspace.channelStore.{ChannelHash, ChannelStore}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.hashing.ChannelHash.{
  hashContinuationsChannels,
  hashDataChannel,
  hashJoinsChannel
}
import coop.rchain.rspace.history.ColdStoreInstances.ColdKeyValueStore
import coop.rchain.rspace.history.instances.RSpaceHistoryReaderImpl
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Serialize}
import fs2.Stream

final case class HistoryRepositoryImpl[F[_]: Concurrent: Parallel: Log: Span, C, P, A, K](
    currentHistory: History[F],
    rootsRepository: RootRepository[F],
    leafStore: ColdKeyValueStore[F],
    rspaceExporter: RSpaceExporter[F],
    rspaceImporter: RSpaceImporter[F],
    // Map channel hash in event log -> channel hash in history
    // We need to maintain this for event log merge
    channelHashesStore: ChannelStore[F, C],
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K]
) extends HistoryRepository[F, C, P, A, K] {

  implicit val ms = Metrics.Source(RSpaceMetricsSource, "history")

  override def getChannelHash(hash: Blake2b256Hash): F[Option[ChannelHash]] =
    channelHashesStore.getChannelHash(hash)

  override def putChannelHash(channel: C): F[Unit] = channelHashesStore.putChannelHash(channel)

  override def putContinuationHash(channels: Seq[C]): F[Unit] =
    channelHashesStore.putContinuationHash(channels)

  type CacheAction = Blake2b256Hash => F[Unit]
  type ColdAction  = (Blake2b256Hash, Option[PersistedData])

  type Result = (ColdAction, HistoryAction)

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
        val key  = hashDataChannel(i.channel, serializeC).bytes
        val data = encodeDatums(i.data)(serializeA)
        s"${key.toHex};insert-data;${data.length};${i.data.length}"
      case i: InsertContinuations[C, P, K] =>
        val key  = hashContinuationsChannels(i.channels, serializeC).bytes
        val data = encodeContinuations(i.continuations)(serializeP, serializeK)
        s"${key.toHex};insert-continuation;${data.length};${i.continuations.length}"
      case i: InsertJoins[C] =>
        val key  = hashJoinsChannel(i.channel, serializeC).bytes
        val data = encodeJoins(i.joins)(serializeC)
        s"${key.toHex};insert-join;${data.length}"
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel, serializeC).bytes
        s"${key.toHex};delete-data;0"
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels, serializeC).bytes
        s"${key.toHex};delete-continuation;0"
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel, serializeC).bytes
        s"${key.toHex};delete-join;0"
    }.toList

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

  private def calculateStorageActions(action: HotStoreTrieAction): Result =
    action match {
      case i: TrieInsertProduce[A] =>
        val data     = encodeDatums(i.data)(serializeA)
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (
          (dataHash, Some(dataLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, dataHash)
        )
      case i: TrieInsertConsume[P, K] =>
        val data              = encodeContinuations(i.continuations)(serializeP, serializeK)
        val continuationsLeaf = ContinuationsLeaf(data)
        val continuationsHash = Blake2b256Hash.create(data)
        (
          (continuationsHash, Some(continuationsLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, continuationsHash)
        )
      case i: TrieInsertJoins[C] =>
        val data      = encodeJoins(i.joins)(serializeC)
        val joinsLeaf = JoinsLeaf(data)
        val joinsHash = Blake2b256Hash.create(data)
        (
          (joinsHash, Some(joinsLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, joinsHash)
        )
      case i: TrieInsertBinaryProduce =>
        val data     = encodeDatumsBinary(i.data)
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (
          (dataHash, Some(dataLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, dataHash)
        )
      case i: TrieInsertBinaryConsume =>
        val data              = encodeContinuationsBinary(i.continuations)
        val continuationsLeaf = ContinuationsLeaf(data)
        val continuationsHash = Blake2b256Hash.create(data)
        (
          (continuationsHash, Some(continuationsLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, continuationsHash)
        )
      case i: TrieInsertBinaryJoins =>
        val data      = encodeJoinsBinary(i.joins)
        val joinsLeaf = JoinsLeaf(data)
        val joinsHash = Blake2b256Hash.create(data)
        (
          (joinsHash, Some(joinsLeaf)),
          InsertAction(i.hash.bytes.toSeq.toList, joinsHash)
        )
      case d: TrieDeleteProduce =>
        ((d.hash, None), DeleteAction(d.hash.bytes.toSeq.toList))
      case d: TrieDeleteConsume =>
        ((d.hash, None), DeleteAction(d.hash.bytes.toSeq.toList))
      case d: TrieDeleteJoins =>
        ((d.hash, None), DeleteAction(d.hash.bytes.toSeq.toList))
    }

  private def transform(hotStoreAction: HotStoreAction): HotStoreTrieAction =
    hotStoreAction match {
      case i: InsertData[C, A] =>
        val key = hashDataChannel(i.channel, serializeC)
        TrieInsertProduce(key, i.data)
      case i: InsertContinuations[C, P, K] =>
        val key = hashContinuationsChannels(i.channels, serializeC)
        TrieInsertConsume(key, i.continuations)
      case i: InsertJoins[C] =>
        val key = hashJoinsChannel(i.channel, serializeC)
        TrieInsertJoins(key, i.joins)
      case d: DeleteData[C] =>
        val key = hashDataChannel(d.channel, serializeC)
        TrieDeleteProduce(key)
      case d: DeleteContinuations[C] =>
        val key = hashContinuationsChannels(d.channels, serializeC)
        TrieDeleteConsume(key)
      case d: DeleteJoins[C] =>
        val key = hashJoinsChannel(d.channel, serializeC)
        TrieDeleteJoins(key)
    }

  // this method is what chackpoint is supposed to do, but checkoint operates on actions on channels, and this
  // address by hashes. TODO elaborate unified API
  def doCheckpoint(trieActions: Seq[HotStoreTrieAction]): F[HistoryRepository[F, C, P, A, K]] = {
    val storageActions = trieActions.par.map(calculateStorageActions)
    val coldActions    = storageActions.map(_._1).collect { case (key, Some(data)) => (key, data) }
    val historyActions = storageActions.map(_._2)

    // save new root for state after checkpoint
    val storeRoot = (root: Blake2b256Hash) => Stream.eval(rootsRepository.commit(root))

    // store cold data
    val storeLeaves =
      Stream.eval(leafStore.putIfAbsent(coldActions.toList).map(_.asLeft[History[F]]))
    // store everything related to history (history data, new root and populate cache for new root)
    val storeHistory = Stream
      .eval(
        history
          .process(historyActions.toList)
          .flatMap(
            resultHistory =>
              storeRoot(resultHistory.root).compile.drain.as(resultHistory.asRight[Unit])
          )
      )

    for {
      newHistory <- Stream
                     .emits(List(storeLeaves, storeHistory))
                     .parJoinProcBounded
                     .collect { case Right(history) => history }
                     .compile
                     .lastOrError
    } yield this.copy[F, C, P, A, K](
      currentHistory = newHistory,
      channelHashesStore = channelHashesStore
    )
  }

  override def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] = {
    val trieActions = actions.par.map(transform).toList
    // store channels mapping
    val storeChannels = Stream
      .emits(actions.map(a => Stream.eval(storeChannelHash(a).map(_.asLeft[History[F]]))))
      .parJoinProcBounded
    for {
      r <- doCheckpoint(trieActions)
      _ <- storeChannels.compile.drain
      _ <- measure(actions)
    } yield r
  }

  override def reset(root: Blake2b256Hash): F[HistoryRepository[F, C, P, A, K]] =
    for {
      _    <- rootsRepository.validateAndSetCurrentRoot(root)
      next = history.reset(root = root)
    } yield this.copy[F, C, P, A, K](currentHistory = next)

  override def exporter: F[RSpaceExporter[F]] = Sync[F].delay(rspaceExporter)

  override def importer: F[RSpaceImporter[F]] = Sync[F].delay(rspaceImporter)

  override def history: History[F] = currentHistory

  override def root: Blake2b256Hash = currentHistory.root

  override def getSerializeC: Serialize[C] = serializeC

  override def getHistoryReader(
      stateHash: Blake2b256Hash
  ): HistoryReader[F, Blake2b256Hash, C, P, A, K] =
    new RSpaceHistoryReaderImpl(history.reset(root = stateHash), leafStore)(
      Concurrent[F],
      serializeC,
      serializeP,
      serializeA,
      serializeK
    )
}
