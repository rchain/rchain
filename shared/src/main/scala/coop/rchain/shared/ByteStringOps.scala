package coop.rchain.shared

import java.io._
import java.util.concurrent.{Callable, Executors}

import com.google.protobuf.ByteString
import coop.rchain.shared.Resources._
import org.apache.commons.compress.compressors.lz4.FramedLZ4CompressorOutputStream.Parameters
import org.apache.commons.compress.compressors.lz4.{
  FramedLZ4CompressorInputStream,
  FramedLZ4CompressorOutputStream
}

import scala.util.{Failure, Success, Try}

object ByteStringOps {

  implicit class RichByteString(bs: ByteString) {
    def compress: ByteString = compressLZ4(bs)

    def decompress: Option[ByteString] = decompressLZ4(bs)
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

  private def decompressLZ4(bs: ByteString): Option[ByteString] = {
    val pos = new PipedOutputStream()
    val pis = new PipedInputStream(pos)
    val is  = bs.newInput()

    // Writes decompressed data into `pos`
    val producer = new Callable[Try[Unit]] {
      override def call(): Try[Unit] =
        try {
          val lz4is = new FramedLZ4CompressorInputStream(is)
          val data  = new Array[Byte](8192)
          var count = lz4is.read(data)

          while (count != -1) {
            pos.write(data, 0, count)
            count = lz4is.read(data)
          }

          lz4is.close()
          Success(())
        } catch {
          case e: IOException => Failure(e)
        } finally {
          is.close()
          pos.close()
        }
    }

    // Reads into [[ByteString]] from `pis`
    val consumer = new Callable[ByteString] {
      override def call(): ByteString = {
        val bs = ByteString.readFrom(pis)
        pis.close()
        bs
      }
    }

    // Needs a thread pool with at least two threads
    val es = Executors.newFixedThreadPool(2)
    val t0 = es.submit(producer)
    val t1 = es.submit(consumer)
    es.shutdown()

    t0.get().toOption.map(_ => t1.get())
  }
}
