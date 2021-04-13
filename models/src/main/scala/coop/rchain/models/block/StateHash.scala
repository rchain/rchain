package coop.rchain.models.block

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16

object StateHash {
  type StateHash = ByteString

  val Length = 32
  implicit class StateHashOps(bs: StateHash) {
    def base16String: String = Base16.encode(bs.toByteArray)
  }

  implicit def ordering: Ordering[StateHash] =
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)

  implicit val show = new Show[StateHash] {
    def show(validator: StateHash): String =
      Base16.encode(validator.toByteArray).take(10)
  }
}
