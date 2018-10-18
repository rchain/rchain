package coop.rchain.shared

import net.jpountz.lz4._

object Compression {

  val factory: LZ4Factory               = LZ4Factory.fastestInstance()
  val compressor: LZ4Compressor         = factory.fastCompressor()
  val decompressor: LZ4FastDecompressor = factory.fastDecompressor()

  def compress(content: Array[Byte]): Array[Byte] = {
    val maxCompressedLength = compressor.maxCompressedLength(content.length)
    val compressed          = new Array[Byte](maxCompressedLength)
    compressor.compress(content, 0, content.length, compressed, 0, maxCompressedLength)
    compressed
  }
  def decompress(compressed: Array[Byte], decompressedLength: Int): Array[Byte] = {
    val restored = new Array[Byte](decompressedLength)
    decompressor.decompress(compressed, 0, restored, 0, decompressedLength);
    restored
  }

  implicit class Ops(data: Array[Byte]) {
    def compress: Array[Byte] = Compression.compress(data)
    def decompress(decompressedLength: Int): Array[Byte] =
      Compression.decompress(data, decompressedLength)
  }
}
