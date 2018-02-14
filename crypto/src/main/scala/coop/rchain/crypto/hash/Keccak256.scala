package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

// Keccak256 hashing algoirthm
object Keccak256 extends Keccak[Digest32] with CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ internalHash(input)
}
