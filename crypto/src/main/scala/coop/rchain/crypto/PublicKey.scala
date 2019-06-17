package coop.rchain.crypto

import com.google.protobuf.ByteString

final case class PublicKey(bytes: Array[Byte])

object PublicKey {
  def apply(bs: ByteString): PublicKey = new PublicKey(bs.toByteArray)
}
