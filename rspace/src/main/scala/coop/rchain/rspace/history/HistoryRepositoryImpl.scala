package coop.rchain.rspace.history

import java.nio.charset.StandardCharsets

import cats.{Applicative, Monad, Parallel}
import cats.effect.{IO, Sync}
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
  RSpace
}
import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import HistoryRepositoryImpl._
import com.typesafe.scalalogging.Logger
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.Serialize

final case class HistoryRepositoryImpl[F[_]: Sync: Parallel, C, P, A, K](
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
    leafStore.put(toBeStored).map(_ => leafs.map(_._3))
  }

  case class ChStat(
      insertData: Int = 0,
      insertJoins: Int = 0,
      insertConts: Int = 0,
      deleteData: Int = 0,
      deleteJoins: Int = 0,
      deleteConts: Int = 0,
      contNotBalanced: Boolean = false
  ) {
    def append(s: ChStat) =
      ChStat(
        insertData = insertData + s.insertData,
        insertJoins = insertJoins + s.insertJoins,
        insertConts = insertConts + s.insertConts,
        deleteData = deleteData + s.deleteData,
        deleteJoins = deleteJoins + s.deleteJoins,
        deleteConts = deleteConts + s.deleteConts
      )
    def isValid = {
      val hasDupe = insertData > 1 || insertJoins > 1 || insertConts > 1 || deleteData > 1 || deleteJoins > 1 || deleteConts > 1
      !(contNotBalanced || hasDupe)
    }
  }

  def checkpointWithRetry(
      actions: List[HotStoreAction],
      expectedRoot: Blake2b256Hash
  ): F[HistoryRepository[F, C, P, A, K]] = {
    implicit val P                  = Parallel[F]
    def toHex(hash: Blake2b256Hash) = hash.bytes.take(5).toHex

    def inc(ch: C, m: Map[C, ChStat], stat: ChStat) = {
      val v = m.getOrElse(ch, ChStat())
      m.updated(ch, v.append(stat))
    }
    val chDistrib = actions.foldl(Map[C, ChStat]()) {
      case (m, act) =>
        act match {
          case i: InsertData[C, A] => inc(i.channel, m, ChStat(insertData = 1))
          case i: InsertJoins[C]   => inc(i.channel, m, ChStat(insertJoins = 1))
          case i: InsertContinuations[C, P, K] =>
            val isBalanced = i.channels.size == i.continuations.size
            i.channels.toVector.foldl(m)({
              case (m1, c) => inc(c, m1, ChStat(insertConts = 1, contNotBalanced = isBalanced))
            })
          case d: DeleteData[C]  => inc(d.channel, m, ChStat(deleteData = 1))
          case d: DeleteJoins[C] => inc(d.channel, m, ChStat(deleteJoins = 1))
          case d: DeleteContinuations[C] =>
            d.channels.toVector.foldl(m)({ case (m1, c) => inc(c, m1, ChStat(deleteConts = 1)) })
          case _ => m
        }
    }
    println(s"DISTRIBUTION size: ${chDistrib.toSeq.size}\n")
    implicit val ordChStat = Ordering.by(ChStat.unapply)
    val distribStr = chDistrib.toSeq
      .filterNot(_._2.isValid)
      .sortBy(_._2)
      .map {
        case (c, stat) =>
          val chStr = serializeC.encode(c).toHex
          s"    $stat, ch: $chStr"
      }
      .mkString("\n")
    println(s"$distribStr")

    val mkCP = for {
      _              <- println(s"CHECKPOINT root: ${toHex(history.root)}, changes: ${actions.size}").pure[F]
      historyActions <- List(actions).parFlatTraverse(transformAndStore)
      next           <- history.process(historyActions)
//      _ <- println(
//            s"       root (${toHex(next.root)}, ${toHex(expectedRoot)}, ${toHex(history.root)}: (new, expected, old)"
//          ).pure[F]
    } yield next
    val maxRetries = 5
    for {
      next <- withRetry[F, History[F]](
               mkCP,
               maxRetries, {
                 case next => next.root != expectedRoot
               }, {
                 case (next, n) =>
                   println(
                     s"History root hash mismatch (retry $n/$maxRetries): expected: ${toHex(expectedRoot)}, new: ${toHex(next.root)}"
                   ).pure[F]
               }
             )
      _ <- rootsRepository.commit(next.root)
      _ <- measure(actions)
    } yield this.copy(history = next)
  }

  // Retry F[A] while condition is satisfied
  def withRetry[M[_]: Monad, T](
      fa: M[T],
      retries: Int,
      condition: T => Boolean,
      onRetry: (T, Int) => M[Unit]
  ): M[T] = {
    def loop(n: Int): M[Either[Int, T]] =
      for {
        a           <- fa
        shouldRetry = n < retries && condition(a)
        _           <- if (shouldRetry) onRetry(a, n) else ().pure[M]
        res <- if (shouldRetry) (n + 1).asLeft[T].pure[M]
              else a.asRight[Int].pure[M]
      } yield res
    Monad[M].tailRecM(0)(loop)
  }

  override def checkpoint(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] = {
    implicit val P = Parallel[F]
//    Thread.sleep(2000)
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
      _    <- rootsRepository.validateRoot(root)
      next = history.reset(root = root)
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

  private def encodeUnsorted[D](data: Seq[D])(implicit codec: Codec[D]): ByteVector =
    codecSeqByteVector
      .encode(
        data
          .map(d => Codec.encode[D](d).get.toByteVector)
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
            channels => encodeUnsorted(channels)
          )
      )
      .get
      .toByteVector
}
