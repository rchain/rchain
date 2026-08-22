package coop.rchain.crypto

import java.util.Arrays

import com.google.protobuf.ByteString

final case class PrivateKey(bytes: Array[Byte]) {

  override def equals(o: Any): Boolean = o match {
    case other @ PrivateKey(_) => bytes.sameElements(other.bytes)
    case _                     => false
  }

  override def hashCode(): Int = Arrays.hashCode(bytes)
}

object PrivateKey {
  def apply(bs: ByteString): PrivateKey = new PrivateKey(bs.toByteArray)
}
