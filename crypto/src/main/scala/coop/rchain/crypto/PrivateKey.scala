package coop.rchain.crypto

import com.google.protobuf.ByteString

final case class PrivateKey(bytes: Array[Byte])

object PrivateKey {
  def apply(bs: ByteString): PrivateKey = new PrivateKey(bs.toByteArray)
}
