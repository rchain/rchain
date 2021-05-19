package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16

object BlockHash {
  type BlockHash = ByteString

  val Length = 32
  implicit class BlockHashOps(bs: BlockHash) {
    def base16String: String = Base16.encode(bs.toByteArray)
  }

  implicit def ordering: Ordering[BlockHash] =
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)

  implicit val show = new Show[BlockHash] {
    def show(blockHash: BlockHash): String =
      Base16.encode(blockHash.toByteArray).take(10)
  }
}
