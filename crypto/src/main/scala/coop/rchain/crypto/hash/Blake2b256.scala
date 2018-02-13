package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

// Blake2b256 hashing algoirthm
object Blake2b256 extends Blake2b[Digest32] with CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ internalHash(input)
}
