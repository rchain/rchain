package coop.rchain.shared

import java.nio.ByteBuffer

import scodec.bits.ByteVector

object ByteVectorOps {

  implicit class RichByteVector(byteVector: ByteVector) {

    def toDirectByteBuffer: ByteBuffer = {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(byteVector.size.toInt)
      byteVector.copyToBuffer(buffer)
      buffer.flip()
      buffer
    }
  }
}
