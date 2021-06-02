package coop.rchain.models

import com.google.protobuf.ByteString

object BlockHash {
  type BlockHash = ByteString

  val Length = 32
}
