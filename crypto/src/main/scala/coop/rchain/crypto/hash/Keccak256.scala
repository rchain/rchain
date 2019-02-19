package coop.rchain.crypto.hash

import org.bouncycastle.crypto.digests.KeccakDigest

/**
  * Keccak256 hashing algorithm
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
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
