package coop.rchain.shared

import java.nio.ByteBuffer

object ByteArrayOps {
  implicit class RichByteArray(bytes: Array[Byte]) {

    def toDirectByteBuffer: ByteBuffer = {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(bytes.length)
      buffer.put(bytes)
      buffer.flip()
      buffer
    }
  }
}
