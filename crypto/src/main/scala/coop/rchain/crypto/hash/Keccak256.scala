package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

/**
Keccak256 hashing algorithm

 * {{{
 * >>> import scorex.crypto.encode._
 *
 * >>> Base16.encode(Keccak256.hash(""))
 * c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
 *
 * >>> Base16.encode(Keccak256.hash("abc"))
 * 4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45
 * }}}
**/
object Keccak256 extends Keccak[Digest32] with CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ internalHash(input)
}
