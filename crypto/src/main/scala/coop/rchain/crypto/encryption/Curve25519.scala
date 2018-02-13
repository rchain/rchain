package coop.rchain.crypto.encryption

import org.abstractj.kalium.crypto._
import org.abstractj.kalium.keys._

object Curve25519 {

  def newKeyPair: (Array[Byte], Array[Byte]) = {
    val keyPair = new KeyPair()
    val pub = keyPair.getPublicKey().toBytes()
    val sec = keyPair.getPrivateKey().toBytes()
    (pub,sec)
  }

  def toPublic(sec: Array[Byte]): Array[Byte] = {
    val keyPair = new KeyPair(sec)
    keyPair.getPublicKey().toBytes()
  }

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
