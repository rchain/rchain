package coop.rchain.blockstorage.util
import java.nio.ByteBuffer

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash

object byteOps {
  implicit class ByteBufferRich(val byteBuffer: ByteBuffer) extends AnyVal {
    def getBlockHash(): BlockHash = {
      val blockHashBytes = Array.ofDim[Byte](32)
      byteBuffer.get(blockHashBytes)
      ByteString.copyFrom(blockHashBytes)
    }

    def getValidator(): Validator = getBlockHash()
  }

  implicit class IntRich(val value: Int) extends AnyVal {
    def toByteString: ByteString = {
      val byteBuffer = ByteBuffer.allocate(4)
      byteBuffer.putInt(value)
      ByteString.copyFrom(byteBuffer.array())
    }
  }
}
