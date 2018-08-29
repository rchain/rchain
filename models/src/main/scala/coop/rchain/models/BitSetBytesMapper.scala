package coop.rchain.models

import java.nio.ByteBuffer
import java.nio.ByteOrder.LITTLE_ENDIAN

import com.google.protobuf.ByteString
import scalapb.TypeMapper

import scala.collection.immutable.BitSet

object BitSetBytesMapper {

  val BYTES_PER_LONG = 8

  implicit val bitSetBytesMapper: TypeMapper[ByteString, BitSet] =
    TypeMapper(byteStringToBitSet)(bitSetToByteString)

  def bitSetToByteString(bitset: BitSet): ByteString = {
    val longs: Array[Long] = bitset.toBitMask
    val byteBuffer         = ByteBuffer.allocate(BYTES_PER_LONG * longs.length).order(LITTLE_ENDIAN)
    longs.foreach(byteBuffer.putLong)
    val bytes: Array[Byte]     = byteBuffer.array
    val usedBytes: Array[Byte] = bytes.reverse.dropWhile(_ == 0).reverse
    ByteString.copyFrom(usedBytes)
  }

  def byteStringToBitSet(byteString: ByteString): BitSet = {
    val bufferSize = nextMultiple(BYTES_PER_LONG, byteString.size())
    val byteBuffer = ByteBuffer.allocate(bufferSize).order(LITTLE_ENDIAN)
    byteString.copyTo(byteBuffer)
    byteBuffer.rewind()
    val longBuffer = byteBuffer.asLongBuffer
    // Hack to force long buffer to dump array. See https://stackoverflow.com/a/19003601/2750819
    val longs = Array.fill[Long](longBuffer.capacity)(0)
    longBuffer.get(longs)
    BitSet.fromBitMask(longs)
  }

  private def nextMultiple(base: Int, n: Int): Int =
    (n + (base - 1)) / base * base
}
