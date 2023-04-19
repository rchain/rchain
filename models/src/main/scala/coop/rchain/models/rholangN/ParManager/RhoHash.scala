package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._
import coop.rchain.rspace.hashing.Blake2b256Hash
import java.util.concurrent.atomic.AtomicInteger
import Constants._
import Sorting._

private[ParManager] object RhoHash {

  private class Hashable(val tag: Byte, val bodySize: Int) {

    private val arrSize: Int     = bodySize + tagSize
    private val arr: Array[Byte] = new Array[Byte](arrSize)
    private val pos              = new AtomicInteger(tagSize)

    arr(0) = tag // Fill the first element of arr with the tag

    def appendByte(b: Byte): Unit = {
      val currentPos = pos.getAndIncrement()
      assert(currentPos + 1 <= arrSize, "Array size exceeded")
      arr(currentPos) = b
    }

    def appendBytes(bytes: Array[Byte]): Unit = {
      val bytesLength = bytes.length
      val currentPos  = pos.getAndAdd(bytesLength)
      assert(currentPos + bytesLength <= arrSize, "Array size exceeded")
      Array.copy(bytes, 0, arr, currentPos, bytesLength)
    }

    def appendParHash(p: ParN): Unit = appendBytes(p.rhoHash.bytes.toArray)

    // Get the hash of the current array
    def calcHash: Blake2b256Hash = {
      val curSize = pos.get()

      if (curSize <= hashSize) {
        if (curSize == hashSize) {
          Blake2b256Hash.fromByteArray(arr)
        } else {
          val newBytes     = new Array[Byte](hashSize)
          val dataStartPos = hashSize - curSize

          for (i <- 0 until hashSize) {
            if (i < dataStartPos) newBytes(i) = 0x00.toByte // fill empty place with 0x00.toByte
            else newBytes(i) = arr(i - dataStartPos)
          }
          Blake2b256Hash.fromByteArray(newBytes)
        }
      } else {
        val hashData = arr.slice(0, curSize)
        Blake2b256Hash.create(hashData)
      }
    }
  }
  private object Hashable {
    def apply(tag: Byte, size: Int): Hashable = new Hashable(tag, size)
  }

  private def longToBytes(value: Long): Array[Byte] = {
    val byteArray = new Array[Byte](longSize)
    for (i <- 0 until longSize) {
      byteArray(longSize - 1 - i) = ((value >>> (i * longSize)) & 0xFF).toByte
    }
    byteArray
  }
  private def intToBytes(value: Int): Array[Byte] = {
    val byteArray = new Array[Byte](intSize)
    for (i <- 0 until intSize) {
      byteArray(intSize - 1 - i) = ((value >>> (i * 8)) & 0xFF).toByte
    }
    byteArray
  }
  private def booleanToByte(v: Boolean): Byte = if (v) 1 else 0

  /** Main types */
  def hashParProc(ps: Seq[ParN]): Blake2b256Hash = {
    val bodySize = hashSize * ps.size
    val hashable = Hashable(PARPROC, bodySize)
    sort(ps).foreach(hashable.appendParHash)
    hashable.calcHash
  }

  def hashSend(chan: ParN, data: Seq[ParN], persistent: Boolean): Blake2b256Hash = {
    val bodySize = hashSize * (data.size + 1) + booleanSize
    val hashable = Hashable(SEND, bodySize)
    hashable.appendParHash(chan)
    data.foreach(hashable.appendParHash)
    hashable.appendByte(booleanToByte(persistent))
    hashable.calcHash
  }

  /** Ground types */
  def hashGNil(): Blake2b256Hash = Hashable(GNIL, 0).calcHash

  def hashGInt(v: Long): Blake2b256Hash = {
    val hashable = Hashable(GINT, longSize)
    hashable.appendBytes(longToBytes(v))
    hashable.calcHash
  }

  /** Collections */
  def hashEList(ps: Seq[ParN]): Blake2b256Hash = {
    val bodySize = hashSize * ps.size
    val hashable = Hashable(ELIST, bodySize)
    ps.foreach(hashable.appendParHash)
    hashable.calcHash
  }

  /** Vars */
  def hashBoundVar(value: Int): Blake2b256Hash = {
    val hashable = Hashable(BOUND_VAR, intSize)
    hashable.appendBytes(intToBytes(value))
    hashable.calcHash
  }

  def hashFreeVar(value: Int): Blake2b256Hash = {
    val hashable = Hashable(FREE_VAR, intSize)
    hashable.appendBytes(intToBytes(value))
    hashable.calcHash
  }

  def hashWildcard(): Blake2b256Hash = Hashable(WILDCARD, 0).calcHash

  /** Expr */

  /** Bundle */

  /** Connective */

}
