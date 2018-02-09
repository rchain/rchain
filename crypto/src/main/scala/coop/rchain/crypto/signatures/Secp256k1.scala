package coop.rchain.crypto.signatures

import org.bitcoin._

object Signatures {

  def verify(
    data: Array[Byte], signature: Array[Byte], pub: Array[Byte]
    ): Boolean =
    NativeSecp256k1.verify(data,signature,pub)

  def sign(
    data: Array[Byte], sec: Array[Byte]
    ): Array[Byte] =
    NativeSecp256k1.sign(data,sec)

}