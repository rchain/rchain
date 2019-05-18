package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.{
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
import java.nio.charset.StandardCharsets
import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import coop.rchain.rspace.nextgenrspace.history.HistoryRepositoryImpl._

object HistoryTransformers {

  type Result = (Blake2b256Hash, Option[PersistedData], HistoryAction)

  final case class MeasureJournal(
      key: String,
      size: Long,
      action: String,
      datumSize: Int,
      datumLen: Long,
      continuationsLength: Long,
      continuationsSize: Int
  )
  def create[C, P, A, K]()(
      implicit
      codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): HistoryTransformers[C, P, A, K] =
    new HistoryTransformers[C, P, A, K]()(codecC, codecP, codecA, codecK): HistoryTransformers[
      C,
      P,
      A,
      K
    ]
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class HistoryTransformers[C, P, A, K](
    implicit
    codecC: Codec[C],
    codecP: Codec[P],
    codecA: Codec[A],
    codecK: Codec[K]
) {

  import HistoryTransformers._

  implicit val serializeC: Serialize[C] = Serialize.fromCodec(codecC)
  private val dataSuffixBits            = BitVector("-data".getBytes(StandardCharsets.UTF_8))
  private val continuationSuffixBits    = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))
  private val joinSuffixBits            = BitVector("-joins".getBytes(StandardCharsets.UTF_8))

  private def hashWithSuffix(bits: BitVector, suffix: BitVector): Blake2b256Hash = {
    val suffixed = bits ++ suffix
    Blake2b256Hash.create(suffixed.toByteVector)
  }

  def hashJoinsChannel(channel: C): Blake2b256Hash =
    hashWithSuffix(codecC.encode(channel).get, joinSuffixBits)

  def hashContinuationsChannels(channels: Seq[C]): Blake2b256Hash = {
    val chs = channels
      .map(c => serializeC.encode(c))
      .sorted(util.ordByteVector)
    val channelsBits = codecSeq(codecByteVector).encode(chs).get
    hashWithSuffix(channelsBits, continuationSuffixBits)
  }

  def hashDataChannel(channel: C): Blake2b256Hash =
    hashWithSuffix(codecC.encode(channel).get, dataSuffixBits)

  def decode[R](bv: ByteVector)(implicit codecR: Codec[R]): Seq[R] =
    Codec.decode[Seq[R]](bv.bits).get.value

  def asMeasureJournal(actions: List[HotStoreAction]): List[MeasureJournal] =
    actions.map {
      case i: InsertData[C, A] =>
        val key      = hashDataChannel(i.channel)
        val data     = encodeData(i.data)
        val dataLeaf = DataLeaf(data)
        MeasureJournal(
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
        MeasureJournal(
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
        MeasureJournal(
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
        MeasureJournal(
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
        MeasureJournal(
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
        MeasureJournal(
          key = key.bytes.toHex,
          size = 0,
          action = "DeleteData",
          datumSize = 0,
          datumLen = 0L,
          continuationsLength = 0,
          continuationsSize = 0
        )
    }

  def transform(actions: List[HotStoreAction]): List[Result] =
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

}
