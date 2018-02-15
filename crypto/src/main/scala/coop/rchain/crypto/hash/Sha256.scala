package coop.rchain.crypto.hash

import scorex.crypto.hash._
import java.security.MessageDigest

/**
Sha256 hashing algorithm

 * {{{
 * >>> import scorex.crypto.encode._
 *
 * >>> Base16.encode(Sha256.hash(""))
 * e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
 *
 * >>> Base16.encode(Sha256.hash("abc"))
 * ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
 *
 * >>> Base16.encode(Sha256.hash("hello world"))
 * b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9
 *
 * >>> Base16.encode(Sha256.hash("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
 * 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1
 * }}}
**/
object Sha256 extends CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ MessageDigest.getInstance("SHA-256").digest(input)
}
