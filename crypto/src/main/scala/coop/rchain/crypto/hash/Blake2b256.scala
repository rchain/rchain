package coop.rchain.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.io.DigestOutputStream
import scodec.bits.ByteVector

import scala.collection.immutable.Seq

/**
Blake2b256 hashing algorithm

  * {{{
  * >>> import coop.rchain.crypto.codec._
  *
  * >>> Base16.encode(Blake2b256.hash("".getBytes))
  * 0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8
  *
  * >>> Base16.encode(Blake2b256.hash("abc".getBytes))
  * bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319
  * }}}
**/
object Blake2b256 {

  def hash(input: Array[Byte]): Array[Byte] = {
    val digestFn = new Blake2bDigest(256)
    digestFn.update(input, 0, input.length)
    val res = new Array[Byte](32)
    digestFn.doFinal(res, 0)
    res
  }

  def hash(inputs: Seq[ByteVector]): Array[Byte] = {
    val outStream = new DigestOutputStream(new Blake2bDigest(256))
    for (input <- inputs) {
      input.copyToStream(outStream)
    }
    outStream.getDigest
    //no calls to .close() since
    //DigestOutputStream doesn't use any closeable resources
  }
}
