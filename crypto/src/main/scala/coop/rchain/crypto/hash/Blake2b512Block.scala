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

We're using some of the tree hashing parameters to achieve online tree hashing,
where the structure of the tree is provided by the application. This precludes
using the depth counters. Also, because we are online, we can't know before-hand
that a node is the last in a level, or is offset. When we hash the root, we know
that it is the root, but since we are only publishing the root hash, and not the
sub-tree, this is sufficient protection against extension.

I've checked that we should be fine against the criteria here.
See: "Sufficient conditions for sound tree and sequential hashing modes" by
Guido Bertoni, Joan Daemen, Michaël Peeters, and Gilles Van Assche

    We also have data at every level, so we're using the fanout parameter to
distinguish child-hashes from data. To make it convenient for block-orientation,
we will null-pad an odd number of child hashes. This means that the fanout varies per-node
rather than being set for the whole instance. Where applicable, we are setting
the other tree-parameters accordingly. Namely, max-depth to 255 (unlimited) and
inner hash length to 64.

This class is an abbreviated version of Blake2bDigest.java from BouncyCastle
https://github.com/bcgit/bc-java/blob/master/core/src/main/java/org/bouncycastle/crypto/digests/Blake2bDigest.java
  */
class Blake2b512Block {
  import Blake2b512Block._
  private val chainValue: Array[Long] = new Array[Long](CHAIN_VALUE_LENGTH)
  private var t0: Long                = 0
  private var t1: Long                = 0

  // block must be 128 bytes long
  def update(block: Array[Byte], offset: Int): Unit =
    compress(block, offset, chainValue, false, false, false)

  // gives the output as if block were the last block processed, but does not
  // invalidate or modify internal state
  def peekFinalRoot(
      block: Array[Byte],
      inOffset: Int,
      output: Array[Byte],
      outOffset: Int
  ): Unit = {
    val tempChainValue: Array[Long] = new Array[Long](8)
    compress(block, inOffset, tempChainValue, true, true, true)
    Pack.longToLittleEndian(tempChainValue, output, outOffset)
  }

  def finalizeInternal(
      block: Array[Byte],
      inOffset: Int,
      output: Array[Byte],
      outOffset: Int
  ): Unit = {
    val tempChainValue: Array[Long] = new Array[Long](8)
    compress(block, inOffset, tempChainValue, true, true, false)
    for (i <- 0 until 4) {
      Pack.longToLittleEndian(tempChainValue(i), output, outOffset + i * 8)
    }
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Blake2b512Block =>
        Arrays.equals(chainValue, that.chainValue) && t0 == that.t0 && t1 == that.t1
      case _ => false
    }

  override def hashCode(): Int = {
    val collapsedCV =
      (chainValue(0) ^ (chainValue(0) >>> 32)).toInt ^
        (chainValue(1) ^ (chainValue(1) >>> 32)).toInt ^
        (chainValue(2) ^ (chainValue(2) >>> 32)).toInt ^
        (chainValue(3) ^ (chainValue(3) >>> 32)).toInt ^
        (chainValue(4) ^ (chainValue(4) >>> 32)).toInt ^
        (chainValue(5) ^ (chainValue(5) >>> 32)).toInt ^
        (chainValue(6) ^ (chainValue(6) >>> 32)).toInt ^
        (chainValue(7) ^ (chainValue(7) >>> 32)).toInt
    (collapsedCV * 31 + (t0 ^ (t0 >>> 32)).toInt) * 31 + (t1 ^ (t1 >>> 32)).toInt
  }

  private[this] def compress(
      msg: Array[Byte],
      offset: Int,
      newChainValue: Array[Long],
      peek: Boolean,
      finalize: Boolean,
      rootFinalize: Boolean
  ): Unit = {
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
      val f0 = if (finalize) 0XFFFFFFFFFFFFFFFFL else 0X0L
      val f1 = if (rootFinalize) 0XFFFFFFFFFFFFFFFFL else 0X0L
      internalState(12) = newT0 ^ IV(4)
      internalState(13) = newT1 ^ IV(5)
      internalState(14) = f0 ^ IV(6)
      internalState(15) = f1 ^ IV(7)
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
      for (i <- 0 until CHAIN_VALUE_LENGTH) {
        chainValue(i) = chainValue(i) ^ internalState(i) ^ internalState(i + 8)
      }
    } else {
      for (i <- 0 until CHAIN_VALUE_LENGTH) {
        newChainValue(i) = chainValue(i) ^ internalState(i) ^ internalState(i + 8)
      }
    }
  }
}

object Blake2b512Block {
  /*
  def apply(): Blake2b512Block = {
    val result = new Blake2b512Block
    Array.copy(IV, 0, result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.chainValue(0) ^= PARAM_VALUE
    result
  }
   */

  def apply(fanout: Byte): Blake2b512Block = {
    val param0WithFanout = PARAM_VALUE_0 | ((fanout.toLong & 0xff) << 16)
    val result           = new Blake2b512Block
    Array.copy(IV, 0, result.chainValue, 0, CHAIN_VALUE_LENGTH)
    result.chainValue(0) ^= param0WithFanout
    result.chainValue(2) ^= PARAM_VALUE_2
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
  val IV: Array[Long] = Array(0X6A09E667F3BCC908L, 0XBB67AE8584CAA73BL, 0X3C6EF372FE94F82BL,
    0XA54FF53A5F1D36F1L, 0X510E527FADE682D1L, 0X9B05688C2B3E6C1FL, 0X1F83D9ABFB41BD6BL,
    0X5BE0CD19137E2179L)

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
  // Depth = 255, Fanout = ??, Keylength = 0, Digest length = 64 bytes
  val PARAM_VALUE_0: Long = 0XFF000040L
  // Inner length = 32 bytes
  val PARAM_VALUE_2: Long = 0X2000L

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

  def debugStr(b: Blake2b512Block): String =
    s"chainValue: ${b.chainValue.mkString(", ")}\n" +
      s"t0: ${b.t0}\n" +
      s"t1: ${b.t1}\n"
}
