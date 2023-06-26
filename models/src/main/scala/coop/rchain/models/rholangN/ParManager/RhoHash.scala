package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN.ParManager.Constants._
import coop.rchain.models.rholangN.ParManager.Sorting._
import coop.rchain.models.rholangN._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.unused

private[ParManager] object RhoHash {

  private class Hashable(val tag: Byte, val bodySize: Int) {
    import Hashable._

    private val arrSize: Int     = bodySize + tagSize
    private val arr: Array[Byte] = new Array[Byte](arrSize)
    private val pos              = new AtomicInteger(tagSize)

    arr(0) = tag // Fill the first element of arr with the tag

    /** Appending methods */
    private def append(b: Byte): Unit = {
      val currentPos = pos.getAndIncrement()
      assert(currentPos + 1 <= arrSize, "Array size exceeded")
      arr(currentPos) = b
    }
    private def append(bytes: Array[Byte]): Unit = {
      val bytesLength = bytes.length
      val currentPos  = pos.getAndAdd(bytesLength)
      assert(currentPos + bytesLength <= arrSize, "Array size exceeded")
      Array.copy(bytes, 0, arr, currentPos, bytesLength)
    }

    def append(v: Boolean): Unit = append(booleanToByte(v))
    def append(v: Int): Unit     = append(intToBytes(v))
    def append(v: Long): Unit    = append(longToBytes(v))

    def append(v: BigInt): Unit     = append(v.toByteArray)
    def append(v: String): Unit     = append(stringToBytes(v))
    def append(v: ByteVector): Unit = append(v.toArray)

    def append(p: RhoTypeN): Unit                 = append(p.rhoHash.bytes.toArray)
    def appendStrings(strings: Seq[String]): Unit = strings.foreach(append)
    def append(ps: Seq[RhoTypeN]): Unit           = ps.foreach(append)
    def append(pOpt: Option[RhoTypeN]): Unit      = pOpt.foreach(append)

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
    def apply(tag: Byte, size: Int = 0): Hashable = new Hashable(tag, size)

    private def booleanToByte(v: Boolean): Byte = if (v) 1 else 0

    private def intToBytes(value: Int): Array[Byte] = {
      val byteArray = new Array[Byte](intSize)
      for (i <- 0 until intSize) {
        byteArray(intSize - 1 - i) = ((value >>> (i * 8)) & 0xFF).toByte
      }
      byteArray
    }

    private def longToBytes(value: Long): Array[Byte] = {
      val byteArray = new Array[Byte](longSize)
      for (i <- 0 until longSize) {
        byteArray(longSize - 1 - i) = ((value >>> (i * longSize)) & 0xFF).toByte
      }
      byteArray
    }

    private def stringToBytes(v: String): Array[Byte] = v.getBytes("UTF-8")

    private def hSizeSeq[T](seq: Seq[T], f: T => Int): Int = seq.map(f).sum

    def hSize(bytes: Array[Byte]): Int = bytes.length

    def hSize(@unused v: Boolean): Int = booleanSize
    def hSize(@unused v: Int): Int     = intSize
    def hSize(@unused v: Long): Int    = longSize
    def hSize(v: BigInt): Int          = hSize(v.toByteArray)
    def hSize(v: String): Int          = stringToBytes(v).length
    def hSize(v: ByteVector): Int      = hSize(v.toArray)

    def hSize(@unused p: RhoTypeN): Int        = hashSize
    def hSize(ps: Seq[RhoTypeN]): Int          = hSizeSeq[RhoTypeN](ps, hSize)
    def hSizeString(strings: Seq[String]): Int = hSizeSeq[String](strings, hSize)
    def hSize(pOpt: Option[RhoTypeN]): Int     = if (pOpt.isDefined) hashSize else 0
  }

  import Hashable._
  def rhoHashFn(p: RhoTypeN): Blake2b256Hash = p match {

    /** Main types */
    case pProc: ParProcN =>
      val hs = Hashable(PARPROC, hSize(pProc.ps))
      hs.append(sortPars(pProc.ps))
      hs.calcHash

    case send: SendN =>
      val bodySize = hSize(send.chan) + hSize(send.data) + hSize(send.persistent)
      val hs       = Hashable(SEND, bodySize)
      hs.append(send.chan)
      hs.append(send.data)
      hs.append(send.persistent)
      hs.calcHash

    case receive: ReceiveN =>
      val bodySize = hSize(receive.binds) + hSize(receive.body) +
        hSize(receive.persistent) + hSize(receive.peek) + hSize(receive.bindCount)
      val hs = Hashable(RECEIVE, bodySize)
      hs.append(receive.binds)
      hs.append(receive.body)
      hs.append(receive.persistent)
      hs.append(receive.peek)
      hs.append(receive.bindCount)
      hs.calcHash

    case m: MatchN =>
      val bodySize = hSize(m.target) + hSize(m.cases)
      val hs       = Hashable(MATCH, bodySize)
      hs.append(m.target)
      hs.append(m.cases)
      hs.calcHash

    case n: NewN =>
      val bodySize = hSize(n.bindCount) + hSize(n.p) + hSizeString(n.uri)
      val hs       = Hashable(NEW, bodySize)
      hs.append(n.bindCount)
      hs.append(n.p)
      hs.appendStrings(sortStrings(n.uri))
      hs.calcHash

    /** Ground types */
    case _: GNilN => Hashable(GNIL).calcHash

    case gBool: GBoolN =>
      val hs = Hashable(GBOOL, hSize(gBool.v))
      hs.append(gBool.v)
      hs.calcHash

    case gInt: GIntN =>
      val hs = Hashable(GINT, hSize(gInt.v))
      hs.append(gInt.v)
      hs.calcHash

    case gBigInt: GBigIntN =>
      val hs = Hashable(GBIG_INT, hSize(gBigInt.v))
      hs.append(gBigInt.v)
      hs.calcHash

    case gString: GStringN =>
      val hs = Hashable(GSTRING, hSize(gString.v))
      hs.append(gString.v)
      hs.calcHash

    case gUri: GUriN =>
      val hs = Hashable(GURI, hSize(gUri.v))
      hs.append(gUri.v)
      hs.calcHash

    /** Collections */
    case list: EListN =>
      val bodySize = hSize(list.ps) + hSize(list.remainder)
      val hs       = Hashable(ELIST, bodySize)
      hs.append(list.ps)
      hs.append(list.remainder)
      hs.calcHash

    /** Vars */
    case bv: BoundVarN =>
      val hs = Hashable(BOUND_VAR, hSize(bv.idx))
      hs.append(bv.idx)
      hs.calcHash

    case fv: FreeVarN =>
      val hs = Hashable(FREE_VAR, hSize(fv.idx))
      hs.append(fv.idx)
      hs.calcHash

    case _: WildcardN => Hashable(WILDCARD).calcHash

    /** Unforgeable names */
    case unf: UnforgeableN =>
      val bodySize = hSize(unf.v)
      val t = unf match {
        case _: UPrivateN    => UPRIVATE
        case _: UDeployIdN   => UDEPLOY_ID
        case _: UDeployerIdN => UDEPLOYER_ID
      }
      val hs = Hashable(t, bodySize)
      hs.append(unf.v)
      hs.calcHash

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN =>
      val bodySize = hSize(bind.patterns) + hSize(bind.source) +
        hSize(bind.remainder) + hSize(bind.freeCount)
      val hs = Hashable(RECEIVE_BIND, bodySize)
      hs.append(bind.patterns)
      hs.append(bind.source)
      hs.append(bind.remainder)
      hs.append(bind.freeCount)
      hs.calcHash

    case mCase: MatchCaseN =>
      val bodySize = hSize(mCase.pattern) + hSize(mCase.source) + hSize(mCase.freeCount)
      val hs       = Hashable(MATCH_CASE, bodySize)
      hs.append(mCase.pattern)
      hs.append(mCase.source)
      hs.append(mCase.freeCount)
      hs.calcHash

    case _ =>
      assert(assertion = false, "Not defined type")
      Blake2b256Hash.fromByteArray(Array())
  }
}
