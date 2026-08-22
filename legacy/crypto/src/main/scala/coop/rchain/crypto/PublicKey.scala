package coop.rchain.crypto

import java.util.Arrays

import com.google.protobuf.ByteString

final case class PublicKey(bytes: Array[Byte]) {

  override def equals(o: Any): Boolean = o match {
    case other @ PublicKey(_) => bytes.sameElements(other.bytes)
    case _                    => false
  }

  override def hashCode(): Int = Arrays.hashCode(bytes)
}

object PublicKey {
  def apply(bs: ByteString): PublicKey = new PublicKey(bs.toByteArray)
}
