package coop.rchain.crypto.signatures

import org.abstractj.kalium.keys._

object Ed25519 {

  def verify(
    data: Array[Byte], signature: Array[Byte], pub: Array[Byte]
    ): Boolean =
    new VerifyKey(pub).verify(data, signature)

  def sign(
    data: Array[Byte], sec: Array[Byte]
    ): Array[Byte] =
    new SigningKey(sec).sign(data)

}
