package coop.rchain.models

import com.trueaccord.scalapb.TypeMapper

import scala.collection.immutable.BitSet
import java.nio.ByteBuffer

import com.google.protobuf.ByteString

object BitSetBytesMapper {
  val BYTES_PER_LONG = 8

  implicit val bitSetBytesMapper: TypeMapper[ByteString, BitSet] =
    TypeMapper(
      byteStringToBitSet
    )(bitSetToByteString)

  def bitSetToByteString(bitset: BitSet): ByteString = {
    val longs: Array[Long] = bitset.toBitMask
    val bytes: Array[Byte] = (ByteBuffer.allocate(BYTES_PER_LONG * longs.length) /: longs) {
      (acc, long) =>
        acc putLong long
    }.array
    ByteString.copyFrom(bytes)
  }

  def byteStringToBitSet(byteString: ByteString): BitSet = {
    if (byteString.isEmpty) {
      BitSet()
    } else {
      val byteArray = byteString.toByteArray
      val buffer = ByteBuffer.wrap(byteArray)
      val longBuffer = buffer.asLongBuffer
      // Hack to force long buffer to dump array. See https://stackoverflow.com/a/19003601/2750819
      var longs = Array.fill[Long](longBuffer.capacity)(0)
      longBuffer.get(longs)
      BitSet.fromBitMask(longs)
    }
  }
}
