package coop.rchain.shared

import java.io._

import com.google.protobuf.ByteString
import coop.rchain.shared.Resources._
import org.apache.commons.compress.compressors.lz4.FramedLZ4CompressorOutputStream.Parameters
import org.apache.commons.compress.compressors.lz4.{
  FramedLZ4CompressorInputStream,
  FramedLZ4CompressorOutputStream
}

import scala.util.control.NonFatal

object ByteStringOps {

  implicit class RichByteString(bs: ByteString) {
    def compress: ByteString = compressLZ4(bs)

    def decompress: Option[ByteString] =
      try {
        Some(decompressLZ4(bs))
      } catch {
        case e: IOException => None
      }
  }

  private def compressLZ4(bs: ByteString): ByteString =
    withResource(bs.newInput()) { is =>
      withResource(new PipedOutputStream()) { pos =>
        withResource(new PipedInputStream(pos)) { pis =>
          new Thread {
            override def run(): Unit =
              // Write compressed data to [[PipedOutputStream]]
              withResource(new FramedLZ4CompressorOutputStream(pos, Parameters.DEFAULT)) { lz4os =>
                val data  = new Array[Byte](8192)
                var count = is.read(data)

                while (count != -1) {
                  lz4os.write(data, 0, count)
                  count = is.read(data)
                }
              }
          }.start()

          // Read compressed data into [[ByteString]]
          ByteString.readFrom(pis)
        }
      }
    }

  private def decompressLZ4(bs: ByteString): ByteString = {
    val pos = new PipedOutputStream()

    withResource(bs.newInput()) { is =>
      withResource(new PipedInputStream(pos)) { pis =>
        new Thread {
          override def run(): Unit =
            withResource(new FramedLZ4CompressorInputStream(is)) { lz4is =>
              // `pos` has to be closed from within this thread
              var exception: Throwable = null
              try {
                val data  = new Array[Byte](8192)
                var count = lz4is.read(data)

                while (count != -1) {
                  pos.write(data, 0, count)
                  count = lz4is.read(data)
                }
              } catch {
                case NonFatal(e) =>
                  exception = e
                  throw e
              } finally {
                closeAndAddSuppressed(exception, pos)
              }
            }
        }.start()

        ByteString.readFrom(pis)
      }
    }
  }
}
