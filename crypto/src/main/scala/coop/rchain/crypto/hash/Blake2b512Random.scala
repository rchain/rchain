package coop.rchain.crypto.hash

import com.google.protobuf.ByteString
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.LongBuffer
import org.bouncycastle.util.Pack
import scalapb.TypeMapper

class Blake2b512Random private (val digest: Blake2b512Block, val lastBlock: ByteBuffer) {
  val pathView: ByteBuffer = lastBlock.duplicate()
  pathView.limit(112)
  val countView: LongBuffer = {
    val lastDuplicate = lastBlock.duplicate()
    lastDuplicate.position(112)
    lastDuplicate.slice().order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
  }

  val hashArray: Array[Byte] = new Array[Byte](64)
  var position: Int          = 0

  private def addByte(index: Byte): Unit = {
    if (pathView.position() == pathView.limit()) {
      digest.update(lastBlock.array(), 0)
      lastBlock.put(Blake2b512Random.BLANK_BLOCK.asReadOnlyBuffer())
      lastBlock.rewind()
      pathView.rewind()
    }
    pathView.put(index)
  }

  private[this] def copy(): Blake2b512Random = {
    val cloneBlock = ByteBuffer.allocate(128)
    cloneBlock.put(lastBlock.asReadOnlyBuffer())
    cloneBlock.rewind()
    val result = new Blake2b512Random(Blake2b512Block(digest), cloneBlock)
    result.pathView.position(pathView.position)
    result
  }

  def hash(): Unit = {
    digest.peekFinal(lastBlock.array(), 0, hashArray, 0)
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

  def peek: ByteBuffer = lastBlock.asReadOnlyBuffer()
}

object Blake2b512Random {
  def apply(init: Array[Byte], offset: Int, length: Int): Blake2b512Random = {
    val result = new Blake2b512Random(Blake2b512Block(), ByteBuffer.allocate(128))
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
  def apply(init: Array[Byte]): Blake2b512Random =
    apply(init, 0, init.length)

  implicit val typeMapper = TypeMapper((byteStr: ByteString) => {
    if (byteStr.isEmpty)
      Blake2b512Random(new Array[Byte](0))
    else {
      val digest                 = Blake2b512Block.typeMapper.toCustom(byteStr.substring(0, 80))
      val result                 = new Blake2b512Random(digest, ByteBuffer.allocate(128))
      val byteBuffer             = byteStr.substring(80).asReadOnlyByteBuffer().order(ByteOrder.LITTLE_ENDIAN)
      val remainderPosition: Int = byteBuffer.getInt()
      if (remainderPosition != 0)
        byteBuffer.put(result.hashArray, remainderPosition, 64 - remainderPosition)
      result
    }
  })((rand: Blake2b512Random) => {
    val remainderSize =
      if (rand.position == 0)
        0
      else
        64 - rand.position
    val digestSize = 80
    val totalSize  = digestSize + remainderSize + 4
    val result     = ByteBuffer.allocateDirect(totalSize)
    Blake2b512Block.fillByteBuffer(rand.digest, result)
    result.order(ByteOrder.LITTLE_ENDIAN).asIntBuffer().put(rand.position)
    if (remainderSize != 0) {
      result.position(84)
      result.put(rand.hashArray, rand.position, 64 - rand.position)
    }
    result.rewind()
    ByteString.copyFrom(result)
  })

  // For testing only, will result in incorrect results otherwise
  def tweakLength0(rand: Blake2b512Random): Unit =
    rand.countView.put(0, -1)

  val BLANK_BLOCK = ByteBuffer.allocateDirect(128).asReadOnlyBuffer()
}
