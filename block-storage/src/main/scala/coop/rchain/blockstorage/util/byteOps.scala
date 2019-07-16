package coop.rchain.blockstorage.util
import java.nio.ByteBuffer

import com.google.protobuf.ByteString
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockHash
import coop.rchain.models.EquivocationRecord
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Validator
import coop.rchain.shared.Language.ignore

object byteOps {
  implicit class ByteBufferRich(private val byteBuffer: ByteBuffer) extends AnyVal {
    def getBlockHash(): BlockHash = {
      val blockHashBytes = Array.ofDim[Byte](BlockHash.Length)
      ignore { byteBuffer.get(blockHashBytes) }
      ByteString.copyFrom(blockHashBytes)
    }

    def getValidator(): Validator = {
      val validatorBytes = Array.ofDim[Byte](Validator.Length)
      ignore { byteBuffer.get(validatorBytes) }
      ByteString.copyFrom(validatorBytes)
    }
  }

  implicit class IntRich(private val value: Int) extends AnyVal {
    def toByteString: ByteString = {
      val byteBuffer = ByteBuffer.allocate(4)
      ignore { byteBuffer.putInt(value) }
      ByteString.copyFrom(byteBuffer.array())
    }
  }

  implicit class LongRich(private val value: Long) extends AnyVal {
    def toByteString: ByteString = {
      val byteBuffer = ByteBuffer.allocate(8)
      ignore { byteBuffer.putLong(value) }
      ByteString.copyFrom(byteBuffer.array())
    }
  }

  implicit class EquivocationRecordRich(private val equivocationRecord: EquivocationRecord)
      extends AnyVal {
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
