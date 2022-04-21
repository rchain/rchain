import coop.rchain.blockstorage.dag.codecs
import coop.rchain.casper.util.GeneratorBlockMetadata
import coop.rchain.models.{
  BlockMetadata,
  BlockMetadataAB,
  BlockMetadataBS,
  BlockMetadataBV,
  BlockMetadataScodecBV
}
import org.scalatest.FlatSpec
import scodec.bits.ByteVector

import scala.concurrent.duration.Duration

class BlockMetadataTimingMeasure extends FlatSpec {
  sealed trait UniversalCodec[T] {
    def encode(block: T): ByteVector
    def decode(serialized: ByteVector): T
  }

  import GeneratorBlockMetadata._

  class BlockMetadataProtobufSerializer extends UniversalCodec[BlockMetadata] {
    override def encode(block: BlockMetadata): ByteVector =
      codecs.codecBlockMetadata.encode(block).require.toByteVector
    def decode(serialized: ByteVector): BlockMetadata =
      codecs.codecBlockMetadata.decode(serialized.toBitVector).require.value
  }

  class BlockMetadataScodecSerializer extends UniversalCodec[BlockMetadataAB] {
    override def encode(block: BlockMetadataAB): ByteVector =
      block.toByteVector
    def decode(serialized: ByteVector): BlockMetadataAB =
      BlockMetadataAB.fromByteVector(serialized)
  }

  class BlockMetadataScodecBVSerializer extends UniversalCodec[BlockMetadataBV] {
    override def encode(block: BlockMetadataBV): ByteVector =
      block.toByteVector
    def decode(serialized: ByteVector): BlockMetadataBV =
      BlockMetadataBV.fromByteVector(serialized)
  }

  class BlockMetadataScodecBSSerializer extends UniversalCodec[BlockMetadataBS] {
    override def encode(block: BlockMetadataBS): ByteVector =
      block.toByteVector
    def decode(serialized: ByteVector): BlockMetadataBS =
      BlockMetadataBS.fromByteVector(serialized)
  }

  it should "Time - measurement of protobuf codec" in {
    def compareBlocks(a: BlockMetadata, b: BlockMetadata): Boolean = a == b
    timingExperiment(
      codecName = "BlockMetadata with protobuf codec",
      new BlockMetadataProtobufSerializer,
      randomBlockMetadata,
      compareBlocks
    )
  }

  it should "Time - measurement of scodec - codec (using Array[Byte])" in {
    def compareBlocks(a: BlockMetadataAB, b: BlockMetadataAB): Boolean = a.isEqualTo(b)
    timingExperiment(
      codecName = "New BlockMetadata scodec(Array[Byte])",
      new BlockMetadataScodecSerializer,
      randomBlockMetadataScodec,
      compareBlocks
    )
  }

  it should "Time - measurement of scodec - codec (using ByteVector)" in {
    def compareBlocks(a: BlockMetadataBV, b: BlockMetadataBV): Boolean = a == b
    timingExperiment(
      codecName = "New BlockMetadata scodec (ByteVector)",
      new BlockMetadataScodecBVSerializer,
      randomBlockMetadataBV,
      compareBlocks
    )
  }

  it should "Time - measurement of scodec - codec (using ByteString)" in {
    def compareBlocks(a: BlockMetadataBS, b: BlockMetadataBS): Boolean = a == b
    timingExperiment(
      codecName = "New BlockMetadata scodec (ByteString)",
      new BlockMetadataScodecBSSerializer,
      randomBlockMetadataBS,
      compareBlocks
    )
  }

  def timingExperiment[T, D <: UniversalCodec[T]](
      codecName: String,
      codec: D,
      generateBlock: (Int, Int, Int) => T,
      compareBlocks: (T, T) => Boolean,
      averageStatistic: Boolean = false
  ): Unit = {
    def experiment(num: Int, elementsCount: Int): Unit = {
      val averageNum    = 50
      val averageWarmUp = 1

      val result =
        (0 until (averageNum + averageWarmUp)).toList
          .foldLeft((0L, 0L)) {
            case ((timeEncode, timeDecode), i) =>
              val blocks = (0 until num).map { _ =>
                generateBlock(elementsCount, elementsCount, elementsCount)
              }

              def statistic(timeEncode: Long, timeDecode: Long): Unit = {
                def iI(v: Int): String = "%7d) ".format(v)

                def mS(v: Long) = "%7.3f ".format(v.toDouble / 1000)

                val str = iI(i) + "  " + mS(timeEncode) + "    " + mS(timeDecode)
                println(str)
              }

              val (blocksEncode, timeEncodeTemp) = {
                val t0  = System.nanoTime
                val res = blocks.map(codec.encode)
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val (blocksDecode, timeDecodeTemp) = {
                val t0  = System.nanoTime
                val res = blocksEncode.map(codec.decode)
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val areEqual = (blocksDecode zip blocks).forall { case (a, b) => compareBlocks(a, b) }
              assert(areEqual, s"${i}: Encoded and decoded blocks should be same")

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

      val str = numStr(num) + " | " + numStr(elementsCount) + " | " + mS(result._1) +
        " | " + mS(result._2)
      println(str)
    }

    def fS(v: String): String = "%7s".format(v)

    println(s"$codecName")
    val strTitle = fS("num") + " | " + fS("elementsCount") + " | " + fS("timeEnc(sec)") +
      " | " + fS("timeDec(sec)")
    println(strTitle)

    experiment(num = 500, elementsCount = 100)
  }
}
