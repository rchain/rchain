package coop.rchain.crypto.hash

import com.google.protobuf.ByteString
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.Arrays
import org.bouncycastle.util.Pack
import org.scalacheck.{Arbitrary, Gen}
import scalapb.TypeMapper

/**
Block oriented Blake2b512 class.

Only supports hashes of data that is a positive number of whole blocks.

This class is an abbreviated version of Blake2bDigest.java from BouncyCastle
https://github.com/bcgit/bc-java/blob/master/core/src/main/java/org/bouncycastle/crypto/digests/Blake2bDigest.java
  */
class Blake2b512Block {
  import Blake2b512Block._
  val chainValue: Array[Long] = new Array[Long](CHAIN_VALUE_LENGTH)
  private var t0: Long        = 0
  private var t1: Long        = 0

  // block must be 128 bytes long
  def update(block: Array[Byte], offset: Int): Unit =
    compress(block, offset, chainValue, false)

  // gives the output as if block were the last block processed, but does not
  // invalidate or modify internal state
  def peekFinal(block: Array[Byte], inOffset: Int, output: Array[Byte], outOffset: Int): Unit = {
    val tempChainValue: Array[Long] = new Array[Long](8)
    compress(block, inOffset, tempChainValue, true)
    Pack.longToLittleEndian(tempChainValue, output, outOffset)
  }

  private[this] def compress(msg: Array[Byte],
                             offset: Int,
                             newChainValue: Array[Long],
                             peek: Boolean): Unit = {
    val internalState: Array[Long] = new Array[Long](BLOCK_LENGTH_LONGS)
    def g(m1: Long, m2: Long, posA: Int, posB: Int, posC: Int, posD: Int): Unit = {
      def rotr64(x: Long, rot: Int): Long =
        x >>> rot | (x << (64 - rot))
      internalState(posA) = internalState(posA) + internalState(posB) + m1;
      internalState(posD) = rotr64(internalState(posD) ^ internalState(posA), 32);
      internalState(posC) = internalState(posC) + internalState(posD);
      internalState(posB) = rotr64(internalState(posB) ^ internalState(posC), 24);
      internalState(posA) = internalState(posA) + internalState(posB) + m2;
      internalState(posD) = rotr64(internalState(posD) ^ internalState(posA), 16);
      internalState(posC) = internalState(posC) + internalState(posD);
      internalState(posB) = rotr64(internalState(posB) ^ internalState(posC), 63);
    }

    val newT0 = t0 + BLOCK_LENGTH_BYTES
    val newT1 = if (newT0 == 0) t1 + 1 else t1

    def init(): Unit = {
      Array.copy(chainValue, 0, internalState, 0, CHAIN_VALUE_LENGTH)
      Array.copy(IV, 0, internalState, CHAIN_VALUE_LENGTH, 4)
      val f0 = if (peek) 0xffffffffffffffffL else 0x0L
      internalState(12) = newT0 ^ IV(4)
      internalState(13) = newT1 ^ IV(5)
      internalState(14) = f0 ^ IV(6)
      internalState(15) = IV(7)
    }
    init()

    val m: Array[Long] = new Array(BLOCK_LENGTH_LONGS)
    for (i <- 0 until BLOCK_LENGTH_LONGS) {
      m(i) = Pack.littleEndianToLong(msg, offset + i * 8)
    }

    for (round <- 0 until ROUNDS) {
      // columns
      g(m(SIGMA(round)(0).toInt), m(SIGMA(round)(1).toInt), 0, 4, 8, 12);
      g(m(SIGMA(round)(2).toInt), m(SIGMA(round)(3).toInt), 1, 5, 9, 13);
      g(m(SIGMA(round)(4).toInt), m(SIGMA(round)(5).toInt), 2, 6, 10, 14);
      g(m(SIGMA(round)(6).toInt), m(SIGMA(round)(7).toInt), 3, 7, 11, 15);
      // diagonals
      g(m(SIGMA(round)(8).toInt), m(SIGMA(round)(9).toInt), 0, 5, 10, 15);
      g(m(SIGMA(round)(10).toInt), m(SIGMA(round)(11).toInt), 1, 6, 11, 12);
      g(m(SIGMA(round)(12).toInt), m(SIGMA(round)(13).toInt), 2, 7, 8, 13);
      g(m(SIGMA(round)(14).toInt), m(SIGMA(round)(15).toInt), 3, 4, 9, 14);
    }
    if (!peek) {
      t0 = newT0
      t1 = newT1
    }
    for (i <- 0 until CHAIN_VALUE_LENGTH) {
      newChainValue(i) = chainValue(i) ^ internalState(i) ^ internalState(i + 8)
    }
  }
}

object Blake2b512Block {
  def apply(): Blake2b512Block = {
    val result = new Blake2b512Block
    Array.copy(IV, 0, result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.chainValue(0) ^= PARAM_VALUE
    result
  }

  def apply(src: Blake2b512Block): Blake2b512Block = {
    val result = new Blake2b512Block
    Array.copy(src.chainValue, 0, result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.t0 = src.t0
    result.t1 = src.t1
    result
  }

  implicit val typeMapper = TypeMapper((byteStr: ByteString) => {
    val result     = new Blake2b512Block
    val longBuffer = byteStr.asReadOnlyByteBuffer().order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
    longBuffer.get(result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.t0 = longBuffer.get()
    result.t1 = longBuffer.get()
    result
  })((block: Blake2b512Block) => {
    val result: ByteBuffer = ByteBuffer.allocateDirect(80)
    fillByteBuffer(block, result)
    result.rewind()
    ByteString.copyFrom(result)
  })

  def fillByteBuffer(block: Blake2b512Block, buf: ByteBuffer): Unit = {
    val view = buf.duplicate().order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
    view.put(block.chainValue)
    view.put(block.t0)
    view.put(block.t1)
    buf.position(buf.position() + 80)
  }

  // Produced from the square root of primes 2, 3, 5, 7, 11, 13, 17, 19.
  // The same as SHA-512 IV.
  val IV: Array[Long] = Array(0x6a09e667f3bcc908L, 0xbb67ae8584caa73bL, 0x3c6ef372fe94f82bL,
    0xa54ff53a5f1d36f1L, 0x510e527fade682d1L, 0x9b05688c2b3e6c1fL, 0x1f83d9abfb41bd6bL,
    0x5be0cd19137e2179L)

  // Message word permutations:
  val SIGMA: Array[Array[Byte]] =
    Array(
      Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      Array(14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
      Array(11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
      Array(7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
      Array(9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
      Array(2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
      Array(12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
      Array(13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
      Array(6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
      Array(10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0),
      Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      Array(14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3)
    )

  val ROUNDS: Int              = 12
  val CHAIN_VALUE_LENGTH: Int  = 8
  val BLOCK_LENGTH_BYTES: Int  = 128
  val BLOCK_LENGTH_LONGS: Int  = 16
  val DIGEST_LENGTH_BYTES: Int = 64
  // Depth = 1, Fanout = 1, Keylength = 0, Digest length = 64 bytes
  val PARAM_VALUE: Long = 0x01010040L

  // This will give invalid results and is for testing only.
  def tweakT0(src: Blake2b512Block): Unit =
    src.t0 = -1

  implicit val arbitrary: Arbitrary[Blake2b512Block] = Arbitrary(for {
    chainValue <- Gen.containerOfN[Array, Long](8, Arbitrary.arbitrary[Long])
    t0         <- Arbitrary.arbitrary[Long]
    t1         <- Arbitrary.arbitrary[Long]
  } yield {
    val result = new Blake2b512Block
    Array.copy(chainValue, 0, result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.t0 = t0
    result.t1 = t1
    result
  })

  def same(b1: Blake2b512Block, b2: Blake2b512Block): Boolean =
    Arrays.equals(b1.chainValue, b2.chainValue) && b1.t0 == b2.t0 && b1.t1 == b2.t1

  def debugStr(b: Blake2b512Block): String =
    s"chainValue: ${b.chainValue.mkString(", ")}\n" +
      s"t0: ${b.t0}\n" +
      s"t1: ${b.t1}\n"
}
