package coop.rchain.shared

import java.nio.ByteBuffer

import com.google.protobuf.ByteString

object ByteStringOps {

  implicit class RichByteString(byteVector: ByteString) {

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    def toDirectByteBuffer: ByteBuffer = {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(byteVector.size)
      byteVector.copyTo(buffer)
      buffer.flip()
      buffer
    }
  }

  implicit class RichByteArray(arr: Array[Byte]) {
    def toByteString: ByteString = ByteString.copyFrom(arr)
  }
}
