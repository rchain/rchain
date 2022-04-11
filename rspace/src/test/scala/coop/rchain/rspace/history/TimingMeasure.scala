package coop.rchain.rspace.history

import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.serializers._
import coop.rchain.store.InMemoryKeyValueStore
import org.scalatest.{Assertion, FlatSpec}
import scodec.Codec
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.higherKinds

// Old codec
object codecs {
  import scodec.codecs._

  val codecByteVector: Codec[ByteVector] = variableSizeBytes(uint8, bytes)

  // Binary codecs for Tree
  val codecLeaf: Codec[Leaf] = (codecByteVector :: codecByteVector).as[Leaf]

  val codecNodePrt: Codec[NodePtr] = (codecByteVector :: codecByteVector).as[NodePtr]

  val codecChild: DiscriminatorCodec[Item, Int] =
    discriminated[Item]
      .by(uint8)
      .subcaseP(tag = 0) {
        case e: EmptyItem.type => e
      }(provide(EmptyItem))
      .subcaseP(tag = 1) {
        case l: Leaf => l
      }(codecLeaf)
      .subcaseP(tag = 2) {
        case n: NodePtr => n
      }(codecNodePrt)

  val codecNode: Codec[Vector[Item]] =
    vectorOfN(provide(numItems), codecChild).as[Vector[Item]]
}

class TimingMeasure extends FlatSpec {
  it should "old codec" in {
    timingExperiment(codecName = "Old scodec - codec", new OldCodec)
  }

  it should "new scodec - codec" in {
    timingExperiment(codecName = "New scodec - codec", new NewCodec)
  }

  it should "new codec 3" in {
    timingExperiment(codecName = "Existing codec", new ExistingCodec)
  }

  sealed trait UniversalCodec {
    def encode(node: Node): ByteVector
    def decode(serialized: ByteVector): Node
  }

  class OldCodec extends UniversalCodec {
    override def encode(node: Node): ByteVector =
      codecs.codecNode.encode(node).require.toByteVector
    def decode(serialized: ByteVector): Node =
      codecs.codecNode.decode(serialized.toBitVector).require.value
  }

  class ExistingCodec extends UniversalCodec {
    override def encode(node: Node): ByteVector =
      RadixTree.Codecs.encode(node)
    override def decode(serialized: ByteVector): Node =
      RadixTree.Codecs.decode(serialized)
  }

  class NewCodec extends UniversalCodec {
    override def encode(node: Node): ByteVector =
      ScodecRadix.encode(node)
    override def decode(serialized: ByteVector): Node =
      ScodecRadix.decode(serialized)
  }

  def timingExperiment[D <: UniversalCodec](
      codecName: String,
      anyCodec: D,
      averageStatistic: Boolean = false
  ): Unit = {
    val itemsCount = numItems

    def experiment(num: Int, nonEmptyItemsCount: Int): Unit = {
      def bytes(size: Int) = (0 until size).map(
        i => i.toByte
      )

      @tailrec
      def generateNode(node: Node, itemsReady: Int, startIdx: Int): Node =
        if (nonEmptyItemsCount > 0 && itemsReady < nonEmptyItemsCount) {
          val itemIndex = startIdx % itemsCount
          val updatedNode = if (itemIndex % 2 > 0) {
            node.updated(
              itemIndex,
              Leaf(ByteVector(bytes(startIdx % 128)), ByteVector(bytes(size = 32)))
            )
          } else {
            node.updated(
              itemIndex,
              NodePtr(ByteVector(bytes(startIdx % 128)), ByteVector(bytes(size = 32)))
            )
          }

          generateNode(updatedNode, itemsReady + 1, startIdx + 1)
        } else node

      val averageNum    = 50
      val averageWarmUp = 1

      val result =
        (0 until (averageNum + averageWarmUp)).toList
          .foldLeft((0L, 0L)) {
            case ((timeEncode, timeDecode), i) =>
              val nodes = (0 until num).map { numNode =>
                generateNode(emptyNode, itemsReady = 0, numNode)
              }

              def statistic(timeEncode: Long, timeDecode: Long): Unit = {
                def iI(v: Int) = "%7d) ".format(v)

                def mS(v: Long) = "%7.3f ".format(v.toDouble / 1000)

                val str = iI(i) + "  " + mS(timeEncode) + "    " + mS(timeDecode)
                println(str)
              }

              val (nodesEncode, timeEncodeTemp) = {
                val t0  = System.nanoTime
                val res = nodes.map(anyCodec.encode)
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              val (nodesDecode, timeDecodeTemp) = {
                val t0  = System.nanoTime
                val res = nodesEncode.map(v => anyCodec.decode(v))
                val t1  = System.nanoTime
                val m   = Duration.fromNanos(t1 - t0).toMillis
                (res, m)
              }

              assert(nodesDecode == nodes, "Encoded and decoded node should be same")

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

      val str = numStr(num) + " | " + s"${nonEmptyItemsCount} | " +
        mS(result._1) + " | " + mS(result._2)
      println(str)
    }

    def fS(v: String): String = "%7s".format(v)

    println(s"${codecName}")
    val strTitle = fS("num") + " | " + fS("Non-empty items count") + " | " +
      fS("timeEnc(msec)") + " | " + fS("timeDec(msec)")
    println(strTitle)

    (0 until itemsCount).foreach { x =>
      experiment(num = 5, x)
    }
  }
}
