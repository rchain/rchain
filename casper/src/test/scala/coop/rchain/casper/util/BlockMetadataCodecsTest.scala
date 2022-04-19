package coop.rchain.casper.util

import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs
import coop.rchain.casper.protocol.Justification
import coop.rchain.models.{BlockMetadata, BlockMetadataScodec, JustificationArr, MetadataScodec}
import coop.rchain.shared.{Base16, Stopwatch}
import monix.eval.Task
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.ByteVector
import scodec.codecs.{bytes, uint8, variableSizeBytes}

import scala.concurrent.duration.Duration

class BlockMetadataCodecsTest extends FlatSpec {
  private def createByteArray(s: String) = Base16.unsafeDecode(s)
  "encode BlockMetadata object with new codec and decode serialized data" should "give initial object" in {
    val testBlockMetadataScodec = BlockMetadataScodec(
      blockHash = createByteArray("112233AA"),
      parents =
        List(createByteArray("AABBCC"), createByteArray("DCAA01"), createByteArray("AABBCC")),
      sender = createByteArray("56FA55C1FC"),
      justifications = List(
        JustificationArr(createByteArray("347899"), createByteArray("561131")),
        JustificationArr(createByteArray("34789943"), createByteArray("5631"))
      ),
      weightMap = Map(
        createByteArray("437800") -> 233212L,
        createByteArray("9911")   -> 23232L
      ),
      blockNum = 34,
      seqNum = 2,
      invalid = false,
      directlyFinalized = false,
      finalized = false
    )

    val serialized         = testBlockMetadataScodec.toByteString
    val reconstructedBlock = MetadataScodec.decode(serialized)

    reconstructedBlock.toBlockMetadataBV() shouldBe testBlockMetadataScodec.toBlockMetadataBV()
  }
}

class TimingMeasure extends FlatSpec {
  it should "BlockMetadata with protobuf codec" in {
    timingExperiment(
      codecName = "BlockMetadata with protobuf codec",
      new BlockMetadataProtobufCodec,
      generateRandomBlockMetadata
    )
  }

  it should "new scodec - codec" in {
    timingExperiment(
      codecName = "New scodec - codec",
      new BlockMetadataScodecSerializer,
      generateRandomBlockMetadataScodec
    )
  }

  sealed trait UniversalCodec[T] {
    def encode(block: T): ByteVector
    def decode(serialized: ByteVector): T
  }

  class BlockMetadataProtobufCodec extends UniversalCodec[BlockMetadata] {
    override def encode(block: BlockMetadata): ByteVector =
      codecs.codecBlockMetadata.encode(block).require.toByteVector
    def decode(serialized: ByteVector): BlockMetadata =
      codecs.codecBlockMetadata.decode(serialized.toBitVector).require.value
  }

  class BlockMetadataScodecSerializer extends UniversalCodec[BlockMetadataScodec] {
    override def encode(block: BlockMetadataScodec): ByteVector =
      MetadataScodec.encode(block)
    def decode(serialized: ByteVector): BlockMetadataScodec =
      MetadataScodec.decode(serialized)
  }

  def timingExperiment[T, D <: UniversalCodec[T]](
      codecName: String,
      anyCodec: D,
      generateBlock: (Int, Int, Int) => T,
      averageStatistic: Boolean = false
  ): Unit = {
    def experiment(num: Int): Unit = {
      val averageNum    = 500
      val averageWarmUp = 1

      val result =
        (0 until (averageNum + averageWarmUp)).toList
          .foldLeft((0L, 0L)) {
            case ((timeEncode, timeDecode), i) =>
              val blocks = (0 until num).map { _ =>
                generateBlock(100, 100, 100)
              }

              def statistic(timeEncode: Long, timeDecode: Long): Unit = {
                def iI(v: Int): String = "%7d) ".format(v)

                def mS(v: Long) = "%7.3f ".format(v.toDouble / 1000)

                val str = iI(i) + "  " + mS(timeEncode) + "    " + mS(timeDecode)
                println(str)
              }

              val (blockEncode, timeEncodeTemp) = {
                val t0  = System.nanoTime
                val res = blocks.map(anyCodec.encode)
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val (_, timeDecodeTemp) = {
                val t0  = System.nanoTime
                val res = blockEncode.map(v => anyCodec.decode(v))
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              //  assert(blockDecode == nodes, "Encoded and decoded node should be same")

              if (averageStatistic) statistic(timeEncodeTemp, timeDecodeTemp)

              if (i < averageWarmUp) (timeEncode, timeDecode)
              else
                (
                  timeEncode + timeEncodeTemp,
                  timeDecode + timeDecodeTemp
                )
          }

      def numStr(v: Int) = "%7d".format(v)

      def mS(v: Long) = "%7.3f".format(v.toDouble / (1000.toDouble * averageNum.toDouble))

      val str = numStr(num) + " | " + mS(result._1) + " | " + mS(result._2)
      println(str)
    }

    def fS(v: String): String = "%7s".format(v)

    println(s"$codecName")
    val strTitle = fS("num") + " | " + fS("timeEnc(sec)") + " | " + fS("timeDec(sec)")
    println(strTitle)

    experiment(num = 40)
  }

  private def arrayToByteString(arrInt: Array[Byte]) = ByteString.copyFrom(arrInt)
  private def generateRandomArray(size: Int): Array[Byte] = {
    val r   = scala.util.Random
    val arr = new Array[Byte](size)
    for (i <- arr.indices) arr(i) = r.nextInt(255).toByte
    arr
  }
  private def generateRandomBlockMetadata(
      parentsCount: Int,
      justCount: Int,
      weightCount: Int
  ): BlockMetadata = {
    val blockHash = arrayToByteString(generateRandomArray(size = 32))
    val parents =
      (0 until parentsCount).toList.map(_ => arrayToByteString(generateRandomArray(size = 32)))
    val sender = arrayToByteString(generateRandomArray(size = 10))
    val justifications = (0 until justCount).toList.map(
      _ =>
        Justification(
          arrayToByteString(generateRandomArray(size = 32)),
          arrayToByteString(generateRandomArray(size = 22))
        )
    )

    val weightMap = (0 until weightCount).toList
      .map(_ => arrayToByteString(generateRandomArray(size = 11)) -> 11223344L)
      .toMap

    BlockMetadata(
      blockHash,
      parents,
      sender,
      justifications,
      weightMap,
      blockNum = 6,
      seqNum = 4,
      invalid = false,
      directlyFinalized = false,
      finalized = false
    )
  }

  private def generateRandomBlockMetadataScodec(
      parentsCount: Int,
      justCount: Int,
      weightCount: Int
  ): BlockMetadataScodec =
    toBlockMetadataScodec(generateRandomBlockMetadata(parentsCount, justCount, weightCount))

  private def toBlockMetadataScodec(block: BlockMetadata): BlockMetadataScodec =
    BlockMetadataScodec(
      block.blockHash.toByteArray,
      block.parents.map(parent => parent.toByteArray),
      block.sender.toByteArray,
      block.justifications.map(
        just =>
          JustificationArr(
            just.validator.toByteArray,
            just.latestBlockHash.toByteArray
          )
      ),
      block.weightMap.map { case (str, long) => (str.toByteArray, long) },
      block.blockNum,
      block.seqNum,
      block.invalid,
      block.directlyFinalized,
      block.finalized
    )
}
