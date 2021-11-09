package coop.rchain.shared

import cats._, cats.data._, cats.syntax.all._
import net.jpountz.lz4._
import scala.util.Try

object Compression {

  val factory: LZ4Factory               = LZ4Factory.fastestInstance()
  val compressor: LZ4Compressor         = factory.highCompressor(17)
  val decompressor: LZ4FastDecompressor = factory.fastDecompressor()

  def compress(content: Array[Byte]): Array[Byte] = {
    val maxCompressedLength = compressor.maxCompressedLength(content.length)
    val compressed          = new Array[Byte](maxCompressedLength)
    val length              = compressor.compress(content, 0, content.length, compressed, 0, maxCompressedLength)
    compressed.take(length)
  }
  def decompress(compressed: Array[Byte], decompressedLength: Int): Option[Array[Byte]] = {
    val restored = new Array[Byte](decompressedLength)
    Try(decompressor.decompress(compressed, 0, restored, 0, decompressedLength)).toOption
      .as(restored)
  }

  implicit class Ops(data: Array[Byte]) {
    def compress: Array[Byte] = Compression.compress(data)
    def decompress(decompressedLength: Int): Option[Array[Byte]] =
      Compression.decompress(data, decompressedLength)
  }
}
