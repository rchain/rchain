package coop.rchain.crypto.signatures

import org.abstractj.kalium.keys._

object Ed25519 {

  def newKeyPair: (Array[Byte], Array[Byte]) = {
    val key = new SigningKey()
    val sec = key.toBytes()
    val pub = key.getVerifyKey().toBytes()
    (sec,pub)
  }

  def toPublic(sec: Array[Byte]): Array[Byte] = {
    val key = new SigningKey(sec)
    key.getVerifyKey().toBytes()
  }

  def verify(
    data: Array[Byte], signature: Array[Byte], pub: Array[Byte]
    ): Boolean =
    new VerifyKey(pub).verify(data, signature)

  def sign(
    data: Array[Byte], sec: Array[Byte]
    ): Array[Byte] =
    new SigningKey(sec).sign(data)

}
