package coop.rchain.rspace.nextgenrspace.history

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.{
  internal,
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
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector

final case class HistoryRepositoryImpl[F[_]: Sync, C, P, A, K](
    history: History[F],
    leafStore: ColdStore[F]
)(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K])
    extends HistoryRepository[F, C, P, A, K] {

  def codecSeqC: Codec[Seq[C]] = codecSeq(codecC)

  implicit def codecSeqSeqC: Codec[Seq[Seq[C]]] =
    codecSeq(codecSeqC)

  implicit def codecSeqDatum: Codec[Seq[internal.Datum[A]]] =
    codecSeq(internal.codecDatum(codecA))

  implicit def codecSeqWaitingContinuation: Codec[Seq[internal.WaitingContinuation[P, K]]] =
    codecSeq(internal.codecWaitingContinuation(codecP, codecK))

  val joinSuffixBits = BitVector.apply("-joins".getBytes)

  private def hashChannels(channels: Seq[C]): Blake2b256Hash =
    StableHashProvider.hash(channels)(Serialize.fromCodec(codecC))

  private def hashChannel(channel: C): Blake2b256Hash =
    StableHashProvider.hash(channel)(Serialize.fromCodec(codecC))

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

  def hashJoinsChannel(channel: C): Blake2b256Hash = {
    val channelBits     = codecC.encode(channel).get
    val joinChannelBits = channelBits ++ joinSuffixBits
    Blake2b256Hash.create(joinChannelBits.toByteVector)
  }

  override def getJoins(channel: C): F[Seq[Seq[C]]] = {
    val key = hashJoinsChannel(channel)
    for {
      storedLeaf <- fetchData(key)
      result = storedLeaf match {
        case Some(JoinsLeaf(bytes)) =>
          codecSeqSeqC.decode(bytes.bits).get.value
        case _ => Seq.empty
      }
    } yield result
  }

  override def getData(channel: C): F[Seq[internal.Datum[A]]] = {
    val key = hashChannel(channel)
    for {
      storedLeaf <- fetchData(key)
      result = storedLeaf match {
        case Some(DataLeaf(bytes)) =>
          codecSeqDatum.decode(bytes.bits).get.value
        case _ => Seq.empty
      }
    } yield result
  }

  override def getContinuations(channels: Seq[C]): F[Seq[internal.WaitingContinuation[P, K]]] = {
    val key = hashChannels(channels)
    for {
      storedLeaf <- fetchData(key)
      result = storedLeaf match {
        case Some(ContinuationsLeaf(bytes)) =>
          codecSeqWaitingContinuation.decode(bytes.bits).get.value
        case _ => Seq.empty
      }
    } yield result
  }

  type Result = (Blake2b256Hash, Option[PersistedData], HistoryAction)

  private def transform(actions: List[HotStoreAction]): List[Result] =
    actions.map {
      case i: InsertData[C, A] =>
        val key      = hashChannel(i.channel)
        val data     = codecSeqDatum.encode(i.data).get.toByteVector
        val dataLeaf = DataLeaf(data)
        val dataHash = Blake2b256Hash.create(data)
        (dataHash, Some(dataLeaf), InsertAction(key.bytes.toSeq.toList, dataHash))
      case i: InsertContinuations[C, P, K] =>
        val key               = hashChannels(i.channels)
        val data              = codecSeqWaitingContinuation.encode(i.continuations).get.toByteVector
        val continuationsLeaf = ContinuationsLeaf(data)
        val continuationsHash = Blake2b256Hash.create(data)
        (
          continuationsHash,
          Some(continuationsLeaf),
          InsertAction(key.bytes.toSeq.toList, continuationsHash)
        )
      case i: InsertJoins[C] =>
        val key       = hashJoinsChannel(i.channel)
        val data      = codecSeqSeqC.encode(i.joins).get.toByteVector
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

  override def process(actions: List[HotStoreAction]): F[HistoryRepository[F, C, P, A, K]] =
    for {
      trieActions    <- Applicative[F].pure(transform(actions))
      historyActions <- storeLeaves(trieActions)
      nextHistory    <- history.process(historyActions)
    } yield this.copy(history = nextHistory)
}
