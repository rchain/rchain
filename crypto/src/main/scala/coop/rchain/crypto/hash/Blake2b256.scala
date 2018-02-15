package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

/**
Blake2b256 hashing algorithm

 * {{{
 * >>> import scorex.crypto.encode._
 *
 * >>> Base16.encode(Blake2b256.hash(""))
 * 0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8
 *
 * >>> Base16.encode(Blake2b256.hash("abc"))
 * bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319
 * }}}
**/
object Blake2b256 extends Blake2b[Digest32] with CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ internalHash(input)
}
