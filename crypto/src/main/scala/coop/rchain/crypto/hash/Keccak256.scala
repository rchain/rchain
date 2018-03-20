package coop.rchain.crypto.hash

import org.bouncycastle.crypto.digests.KeccakDigest

/**
Keccak256 hashing algorithm

  * {{{
  * >>> import coop.rchain.crypto.codec._
  *
  * >>> Base16.encode(Keccak256.hash("".getBytes))
  * c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
  *
  * >>> Base16.encode(Keccak256.hash("abc".getBytes))
  * 4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45
  * }}}
**/
object Keccak256 {

  def hash(input: Array[Byte]): Array[Byte] = {
    val digestFn = new KeccakDigest(256)
    synchronized {
      digestFn.update(input, 0, input.length)
      val res = new Array[Byte](32)
      digestFn.doFinal(res, 0)
      res
    }
  }

}
