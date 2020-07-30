package coop.rchain.models.block

import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16

object StateHash {
  type StateHash = ByteString

  val Length = 32
  implicit class StateHashOps(bs: StateHash) {
    def base16String: String = Base16.encode(bs.toByteArray)
  }
}
