package coop.rchain.rspace.serializers

import coop.rchain.rspace.history.RadixTree
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.shared.Base16
import monix.eval.Task
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, TransformSyntax}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration

object ScodecRadix {
  import scodec.codecs._

  // For encoding prefix length necessary 7 bits
  private val codecPrefix: Codec[ByteVector] = variableSizeBytes(uint(bits = 7), bytes)
  private val codecData: Codec[ByteVector]   = fixedSizeBytes(size = 32L, bytes)

  // Binary codecs for Leaf and NodePtr
  private val codecLeaf: Codec[Leaf]       = (codecPrefix :: codecData).as[Leaf]
  private val codecNodePrt: Codec[NodePtr] = (codecPrefix :: codecData).as[NodePtr]
  private val codecChild: DiscriminatorCodec[Item, Boolean] =
    discriminated[Item]
      .by(bool)
      .subcaseP(tag = false) {
        case l: Leaf => l
      }(codecLeaf)
      .subcaseP(tag = true) {
        case n: NodePtr => n
      }(codecNodePrt)

  //  We need to know index of item, that's why we use tuple (Int, Item) for serialization
  private val codecIndexAndChild                             = uint8 ~ codecChild
  private val codecNonEmptyItems: Codec[Vector[(Int, Item)]] = vector(codecIndexAndChild)
  private val codecNode: Codec[Node] = codecNonEmptyItems.widen[Node](
    { indicesAndItems =>
      //  Postprocessing - collect non-empty items to node
      @tailrec
      def collectNode(node: Node, itemIdx: Int = 0): Node =
        if (itemIdx < indicesAndItems.size) {
          val (index, item) = indicesAndItems(itemIdx)
          val updatedNode   = node.updated(index, item)
          collectNode(updatedNode, itemIdx + 1)
        } else node

      collectNode(emptyNode)
    }, { node =>
      //  Preprocessing - we should know indices of non-empty items to encode them
      val idxAndItems = node.zipWithIndex.map { case (item, idx) => (idx, item) }

      val nonEmptyItems = idxAndItems.filter(idxAndItem => idxAndItem._2 != EmptyItem)

      //  Remove EmptyItem's before encoding - they mustn't be in serialized ByteVector
      Attempt.successful(nonEmptyItems)
    }
  )

  def encode(node: Node): ByteVector =
    codecNode.encode(node).getOrElse(BitVector.empty).toByteVector

  def decode(serialized: ByteVector): Node =
    try {
      codecNode.decode(serialized.toBitVector).require.value
    } catch {
      case _: Exception =>
        assert(assertion = false, "Error during deserialization: invalid data format")
        emptyNode
    }
}
