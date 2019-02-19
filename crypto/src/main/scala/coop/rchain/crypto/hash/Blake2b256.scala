package coop.rchain.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.io.DigestOutputStream
import scodec.bits.ByteVector

import scala.collection.immutable.Seq

/**
  * Blake2b256 hashing algorithm
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object Blake2b256 {

  def hash(input: Array[Byte]): Array[Byte] = {
    val digestFn = new Blake2bDigest(256)
    digestFn.update(input, 0, input.length)
    val res = new Array[Byte](32)
    digestFn.doFinal(res, 0)
    res
  }

  def hash(inputs: ByteVector*): Array[Byte] = {
    val outStream = new DigestOutputStream(new Blake2bDigest(256))
    for (input <- inputs) {
      input.copyToStream(outStream)
    }
    outStream.getDigest
    //no calls to .close() since
    //DigestOutputStream doesn't use any closeable resources
  }
}
