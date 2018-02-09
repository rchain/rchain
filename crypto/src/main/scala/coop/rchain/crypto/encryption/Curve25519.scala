package coop.rchain.crypto.encryption

import org.abstractj.kalium.crypto._

object Curve25519 {

  def encrypt(
    pub: Array[Byte], sec: Array[Byte],
    nonce: Array[Byte], message: Array[Byte]
    ): Array[Byte] =
    new Box(pub,sec).encrypt(nonce, message)

  def decrypt(
    pub: Array[Byte], sec: Array[Byte],
    nonce: Array[Byte], cipher: Array[Byte]
    ): Array[Byte] =
    new Box(pub,sec).decrypt(nonce, cipher)

}
