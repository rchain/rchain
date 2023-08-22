package coop.rchain.models.rholangn.parmanager.blake2

import coop.rchain.models.rholangn.parmanager.Constants.hashSize
import org.bouncycastle.crypto.digests.Blake2bDigest

object Blake2Hash {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def hash(input: Array[Byte]): Array[Byte] = {
    val digestFn = new Blake2bDigest(hashSize * 8)
    digestFn.update(input, 0, input.length)
    val res = new Array[Byte](hashSize)
    digestFn.doFinal(res, 0)
    res
  }
}
