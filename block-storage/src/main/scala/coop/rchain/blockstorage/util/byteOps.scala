package coop.rchain.blockstorage.util
import java.nio.ByteBuffer

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.models.EquivocationRecord
import coop.rchain.shared.Language.ignore

object byteOps {
  implicit class ByteBufferRich(val byteBuffer: ByteBuffer) extends AnyVal {
    def getBlockHash(): BlockHash = {
      val blockHashBytes = Array.ofDim[Byte](32)
      ignore { byteBuffer.get(blockHashBytes) }
      ByteString.copyFrom(blockHashBytes)
    }

    def getValidator(): Validator = getBlockHash()
  }

  implicit class IntRich(val value: Int) extends AnyVal {
    def toByteString: ByteString = {
      val byteBuffer = ByteBuffer.allocate(4)
      ignore { byteBuffer.putInt(value) }
      ByteString.copyFrom(byteBuffer.array())
    }
  }

  implicit class LongRich(val value: Long) extends AnyVal {
    def toByteString: ByteString = {
      val byteBuffer = ByteBuffer.allocate(8)
      ignore { byteBuffer.putLong(value) }
      ByteString.copyFrom(byteBuffer.array())
    }
  }

  implicit class EquivocationRecordRich(val equivocationRecord: EquivocationRecord) extends AnyVal {
    def toByteString: ByteString = {
      val blockHashes =
        equivocationRecord.equivocationDetectedBlockHashes.fold(ByteString.EMPTY)(_.concat(_))
      equivocationRecord.equivocator
        .concat(equivocationRecord.equivocationBaseBlockSeqNum.toByteString)
        .concat(equivocationRecord.equivocationDetectedBlockHashes.size.toByteString)
        .concat(blockHashes)
    }
  }
}
