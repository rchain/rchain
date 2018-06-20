package coop.rchain.crypto.hash

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.LongBuffer
import org.bouncycastle.util.Pack

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

  val BLANK_BLOCK = ByteBuffer.allocateDirect(128).asReadOnlyBuffer()
}
