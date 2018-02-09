package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

// Sha256 hashing algorithm
object Sha256 extends CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ MessageDigest.getInstance("SHA-256").digest(input)
}
