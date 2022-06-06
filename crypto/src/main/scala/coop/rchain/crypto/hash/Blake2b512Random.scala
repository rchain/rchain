package coop.rchain.crypto.hash

import com.google.protobuf.ByteString
import org.bouncycastle.util.Pack
import org.scalacheck.{Arbitrary, Gen}
import scalapb.TypeMapper

import java.nio.{ByteBuffer, ByteOrder, LongBuffer}
import java.security.SecureRandom
import scala.annotation.tailrec
import scala.util.Random

/** Blake2b512 based splittable and mergeable random number generator
  * specialized for generating 256-bit unforgeable names.
  * splitByte and splitShort are the interfaces to make the random number
  * generator diverge.
  * Blake2b512.merge uses online tree hashing to merge two random generator
  * states.
  */
@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements"))
class Blake2b512Random private (
    private val digest: Blake2b512Block,
    private val lastBlock: ByteBuffer
) {
  private val pathView: ByteBuffer = lastBlock.duplicate()
  pathView.limit(112)
  private val countView: LongBuffer = {
    val lastDuplicate = lastBlock.duplicate()
    lastDuplicate.position(112)
    lastDuplicate.slice().order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
  }

  private val hashArray: Array[Byte] = new Array[Byte](64)
  private var position: Int          = 0
  def getPosition: Int               = position

  private def addByte(index: Byte): Unit = {
    if (pathView.position() == pathView.limit()) {
      digest.update(lastBlock.array(), 0)
      lastBlock.put(Blake2b512Random.BLANK_BLOCK.asReadOnlyBuffer())
      lastBlock.rewind()
      pathView.rewind()
    }
    pathView.put(index)
  }

  def copy(): Blake2b512Random = {
    val cloneBlock = ByteBuffer.allocate(128)
    cloneBlock.put(lastBlock.asReadOnlyBuffer())
    cloneBlock.rewind()
    val result = new Blake2b512Random(Blake2b512Block(digest), cloneBlock)
    result.pathView.position(pathView.position)
    result
  }

  private def hash(): Unit = {
    digest.peekFinalRoot(lastBlock.array(), 0, hashArray, 0)
    val low = countView.get(0)
    if (low == -1) {
      val high = countView.get(1)
      countView.put(0, 0)
      countView.put(1, high + 1)
    } else {
      countView.put(0, low + 1)
    }
  }

  def next(): Array[Byte] =
    if (position == 0) {
      hash()
      position = 32
      hashArray.slice(0, 32)
    } else {
      position = 0
      hashArray.slice(32, 64)
    }

  def splitByte(index: Byte): Blake2b512Random = {
    val split = copy()
    split.addByte(index)
    split
  }

  def splitShort(index: Short): Blake2b512Random = {
    val split  = copy()
    val packed = new Array[Byte](2)
    Pack.shortToLittleEndian(index, packed, 0)
    split.addByte(packed(0))
    split.addByte(packed(1))
    split
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Blake2b512Random =>
        digest == that.digest &&
          pathView.position() == that.pathView.position() &&
          position == that.position &&
          lastBlock.equals(that.lastBlock) && {
          if (position == 0)
            true
          else
            ByteBuffer
              .wrap(hashArray, position, 64 - position)
              .equals(ByteBuffer.wrap(that.hashArray, position, 64 - position))
        }
    }

  override def hashCode(): Int =
    ((digest.hashCode * 31 + pathView.position()) * 31 + position) * 31 + lastBlock.hashCode
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object Blake2b512Random {
  private[this] def apply(init: Array[Byte], offset: Int, length: Int): Blake2b512Random = {
    val result = new Blake2b512Random(Blake2b512Block(0.toByte), ByteBuffer.allocate(128))
    val range  = Range(offset, offset + length - 127, 128)
    range.foreach { base =>
      result.digest.update(init, base)
    }
    val partialBase =
      if (range.isEmpty)
        offset
      else
        range.last + 128

    // If there is any remainder:
    if (offset + length != partialBase) {
      val padded = new Array[Byte](128)
      Array.copy(init, partialBase, padded, 0, offset + length - partialBase)
      result.digest.update(padded, 0)
    }
    result
  }

  def apply(length: Int): Blake2b512Random = {
    val bytes = new Array[Byte](length)
    new SecureRandom().nextBytes(bytes)
    apply(bytes)
  }

  def apply(init: Array[Byte]): Blake2b512Random =
    apply(init, 0, init.length)

  def merge(children: Seq[Blake2b512Random]): Blake2b512Random = {
    @tailrec
    def internalMerge(children: Vector[Blake2b512Random]): Blake2b512Random = {
      val squashedBuilder = Vector.newBuilder[Blake2b512Random]
      val chainBlock      = new Array[Byte](128)
      children.grouped(255).foreach { slice =>
        val result =
          new Blake2b512Random(Blake2b512Block(slice.size.toByte), ByteBuffer.allocate(128))
        squashedBuilder += result
        slice.grouped(4).foreach { quad =>
          for (i <- 0 until quad.size) {
            quad(i).digest.finalizeInternal(quad(i).lastBlock.array(), 0, chainBlock, i * 32)
          }
          if (quad.size != 4) {
            BLANK_BLOCK.asReadOnlyBuffer().get(chainBlock, quad.size * 32, (4 - quad.size) * 32)
          }
          result.digest.update(chainBlock, 0)
        }
      }

      val result = squashedBuilder.result
      if (result.size == 1) {
        result(0)
      } else
        internalMerge(result)
    }

    // Throw error if at least two random generators are merging
    assert(children.size >= 2, {
      s"Blake2b512Random should have at least 2 inputs to merge, received ${children.size}."
    })

    // Merge two or more random generators
    internalMerge(children.toVector)
  }

  implicit val typeMapper = TypeMapper((byteStr: ByteString) => {
    if (byteStr.isEmpty)
      Blake2b512Random(new Array[Byte](0))
    else {
      val digestSize = 80
      val buffer     = byteStr.asReadOnlyByteBuffer()
      val digest     = Blake2b512Block.typeMapper.toCustom(byteStr.substring(16, 16 + digestSize))
      val result     = new Blake2b512Random(digest, ByteBuffer.allocate(128))
      val countSrc = {
        val bufDuplicate = buffer.duplicate()
        bufDuplicate.limit(16)
        bufDuplicate.slice().order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
      }
      result.countView.duplicate().put(countSrc)
      buffer.position(digestSize + 16)
      val pathPosition: Int      = buffer.get().toInt
      val remainderPosition: Int = buffer.get().toInt
      val pathSrc = {
        val bufDuplicate = buffer.duplicate()
        bufDuplicate.limit(bufDuplicate.position() + pathPosition)
        bufDuplicate.slice()
      }
      result.pathView.put(pathSrc)
      buffer.position(buffer.position() + pathSrc.capacity())
      if (remainderPosition != 0)
        buffer.get(result.hashArray, remainderPosition, 64 - remainderPosition)
      result.position = remainderPosition
      result
    }
  })((rand: Blake2b512Random) => {
    val remainderSize =
      if (rand.position == 0)
        0
      else
        64 - rand.position
    val digestSize = 80
    // 128-bit rand count, digest, 2 positions, partial path, and remainder
    val totalSize = 16 + digestSize + 2 + rand.pathView.position() + remainderSize
    val result    = ByteBuffer.allocate(totalSize)
    result.order(ByteOrder.LITTLE_ENDIAN).asLongBuffer().put(rand.countView.asReadOnlyBuffer())
    result.position(16)
    Blake2b512Block.fillByteBuffer(rand.digest, result)
    result.put(rand.pathView.position().toByte)
    result.put(rand.position.toByte)
    val pathCopy = rand.pathView.duplicate
    pathCopy.flip()
    result.put(pathCopy)
    if (remainderSize != 0) {
      result.put(rand.hashArray, rand.position, 64 - rand.position)
    }
    result.rewind()
    ByteString.copyFrom(result)
  })

  val BLANK_BLOCK = ByteBuffer.allocate(128).asReadOnlyBuffer()

  // For testing only, will result in incorrect results otherwise
  def tweakLength0(rand: Blake2b512Random): Unit =
    rand.countView.put(0, -1)

  implicit val arbitrary: Arbitrary[Blake2b512Random] = Arbitrary(for {
    digest   <- Arbitrary.arbitrary[Blake2b512Block]
    position <- Gen.oneOf[Int](0, 32)
    // This only works at 0 and 32.
    remainder    <- Gen.containerOfN[Array, Byte](position, Arbitrary.arbitrary[Byte])
    countLow     <- Arbitrary.arbitrary[Long]
    countHigh    <- Arbitrary.arbitrary[Long]
    pathPosition <- Gen.choose[Int](0, 112)
    path         <- Gen.containerOfN[Array, Byte](pathPosition, Arbitrary.arbitrary[Byte])
  } yield {
    val result = new Blake2b512Random(digest, ByteBuffer.allocate(128))
    result.countView.put(0, countLow)
    result.countView.put(1, countHigh)
    result.pathView.put(path)
    if (position != 0)
      Array.copy(remainder, 0, result.hashArray, position, 64 - position)
    result.position = position
    result
  })

  def debugStr(rand: Blake2b512Random): String = {
    val rotPosition = ((rand.position - 1) & 0x3f) + 1
    s"digest: ${Blake2b512Block.debugStr(rand.digest)}\n" +
      s"lastBlock: ${rand.lastBlock.array().mkString(", ")}\n" +
      s"pathPosition: ${rand.pathView.position()}\n" +
      s"position: ${rand.position}\n" +
      s"rotPosition: ${rotPosition}\n" +
      s"remainder: ${rand.hashArray.slice(rotPosition, 64).mkString(", ")}\n"
  }
}
