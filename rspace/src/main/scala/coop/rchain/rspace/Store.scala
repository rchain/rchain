package coop.rchain.rspace

import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.collection.immutable.Seq

import coop.rchain.rspace.internal._
import coop.rchain.rspace.util._

import coop.rchain.rspace.internal.scodecs._
import scodec.Codec
import scodec.bits._

trait KeyValueAmbry {
  private[rspace] type H = ByteBuffer

  private[rspace] type T

  trait KeyValueTable {
    def iterateKeys(txn: T): Iterator[H]
    def put(txn: T, keyBuffer: H, valueBuffer: ByteBuffer): Unit
    def get(txn: T, keyBuffer: H): Option[ByteBuffer]
    def delete(txn: T, keyBuffer: H): Unit
    def isEmpty(txn: T): Boolean
    def drop(txn: T): Unit
  }

  val _dbKeys: KeyValueTable
  val _dbData: KeyValueTable
  val _dbWaitingContinuations: KeyValueTable
  val _dbJoins: KeyValueTable
}

/**
  * We need different allocation strategies for LMDBStore that accepts only allocateDirect'ed buffers,
  * while pure-Scala stores work fine with zero-copy wrappers
  */
trait IBufferAllocator {
  def toByteBuffer(bytes: Array[Byte]): ByteBuffer
  def toByteBuffer(bitVector: BitVector): ByteBuffer
}

/**
  * Common store implementation, takes care of store logic and data searialization
  */
abstract class Store[C, P, A, K]()(implicit
                                   sc: Serialize[C],
                                   sp: Serialize[P],
                                   sa: Serialize[A],
                                   sk: Serialize[K],
                                   allocator: IBufferAllocator)
    extends KeyValueAmbry
    with IStore[C, P, A, K]
    with ITestableStore[C, P] {

  import coop.rchain.rspace.Store._

  private[rspace] def hashCs(cs: Seq[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(cs)(st, allocator))

  private[rspace] def getKey(txn: T, s: H): Seq[C] =
    _dbKeys.get(txn, s).map(fromByteBuffer[C]).getOrElse(Seq.empty[C])

  private[rspace] def putCsH(txn: T, channels: Seq[C]): H = {
    val packedCs = toByteBuffer(channels)
    val keyCs    = hashBytes(packedCs)
    _dbKeys.put(txn, keyCs, packedCs)
    keyCs
  }

  private[this] def readDatumBytesList(txn: T, keyCs: H): Option[Seq[DatumBytes]] =
    _dbData.get(txn, keyCs).map(fromByteBuffer(_, dataBytesListCodec))

  private[this] def writeDatumBytesList(txn: T, keyCs: H, values: Seq[DatumBytes]): Unit =
    if (values.nonEmpty) {
      _dbData.put(txn, keyCs, toByteBuffer(values, dataBytesListCodec))
    } else {
      _dbData.delete(txn, keyCs)
      collectGarbage(txn, keyCs, wcsCollected = true)
    }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val keyCs         = putCsH(txn, channels)
    val datumBytes    = DatumBytes(toByteVector(datum.a), datum.persist)
    val datumBytesSeq = readDatumBytesList(txn, keyCs).getOrElse(Seq.empty[DatumBytes])
    writeDatumBytesList(txn, keyCs, datumBytes +: datumBytesSeq)
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] = {
    val keyCs = hashCs(channels)
    readDatumBytesList(txn, keyCs)
      .map(_.map(bytes => Datum(fromByteVector[A](bytes.datumBytes), bytes.persist)))
      .getOrElse(Seq.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     keyCs: H,
                     dataCollected: Boolean = false,
                     wcsCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(kvTable: KeyValueTable): Boolean = kvTable.get(txn, keyCs).isEmpty

    val readyToCollect = (dataCollected || isEmpty(_dbData)) &&
      (wcsCollected || isEmpty(_dbWaitingContinuations)) &&
      (joinsCollected || isEmpty(_dbJoins))

    if (readyToCollect) {
      _dbKeys.delete(txn, keyCs)
    }
  }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readDatumBytesList(txn, keyCs) match {
      case Some(datumBytesList) =>
        writeDatumBytesList(txn, keyCs, util.dropIndex(datumBytesList, index))
      case None => throw new IllegalArgumentException(s"removeDatum: no values at $channels")
    }
  }

  private[this] def readWaitingContinuationBytesList(
      txn: T,
      keyCs: H): Option[Seq[WaitingContinuationBytes]] =
    _dbWaitingContinuations
      .get(txn, keyCs)
      .map(fromByteBuffer(_, waitingContinuationBytesListCodec))

  private[this] def writeWaitingContinuationBytesList(txn: T,
                                                      keyCs: H,
                                                      values: Seq[WaitingContinuationBytes]): Unit =
    if (values.nonEmpty) {
      _dbWaitingContinuations.put(txn,
                                  keyCs,
                                  toByteBuffer(values, waitingContinuationBytesListCodec))
    } else {
      _dbWaitingContinuations.delete(txn, keyCs)
      collectGarbage(txn, keyCs, wcsCollected = true)
    }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val keyCs = putCsH(txn, channels)
    val waitingContinuationBytes =
      WaitingContinuationBytes(toBytesList(continuation.patterns),
                               toByteVector(continuation.continuation),
                               continuation.persist)
    val waitingContinuationsList =
      readWaitingContinuationBytesList(txn, keyCs).getOrElse(Seq.empty[WaitingContinuationBytes])
    writeWaitingContinuationBytesList(txn,
                                      keyCs,
                                      waitingContinuationBytes +: waitingContinuationsList)
  }

  private[rspace] def getWaitingContinuations(txn: T,
                                              curr: Seq[C]): Seq[WaitingContinuation[P, K]] = {
    val keyCs = hashCs(curr)
    readWaitingContinuationBytesList(txn, keyCs)
      .map(
        _.map(
          wcs =>
            WaitingContinuation(fromBytesList[P](wcs.patterns),
                                fromByteVector[K](wcs.kvalue),
                                wcs.persist)))
      .getOrElse(Seq.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removeWaitingContinuations(txn: T, channels: Seq[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readWaitingContinuationBytesList(txn, keyCs) match {
      case Some(wcs) => writeWaitingContinuationBytesList(txn, keyCs, util.dropIndex(wcs, index))
      case None =>
        throw new IllegalArgumentException(s"removeWaitingContinuations: no values at $channels")
    }
  }

  private[rspace] def removeAll(txn: T, channels: Seq[C]): Unit = {
    val keyCs = hashCs(channels)
    readWaitingContinuationBytesList(txn, keyCs).foreach { _ =>
      writeWaitingContinuationBytesList(txn, keyCs, Seq.empty)
    }
    readDatumBytesList(txn, keyCs).foreach { _ =>
      writeDatumBytesList(txn, keyCs, Seq.empty)
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashCs(Seq(c))
    val oldCsList =
      _dbJoins
        .get(txn, joinKey)
        .map(toBytesLists)
        .getOrElse(Seq.empty[Seq[ByteVector]])

    val addBl = toBytesList(cs)
    if (!oldCsList.contains(addBl)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(addBl +: oldCsList))
    }
  }

  private[rspace] def getJoin(txn: T, c: C): Seq[Seq[C]] = {
    val joinKey = hashCs(Seq(c))
    _dbJoins
      .get(txn, joinKey)
      .map(toBytesLists)
      .map(_.map(fromBytesList[C]))
      .getOrElse(Seq.empty[Seq[C]])
  }

  private[rspace] def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashCs(Seq(c))
    _dbJoins
      .get(txn, joinKey)
      .map(toBytesLists)
      .map(_.map(fromBytesList[C]))
      .map(exList => (exList, exList.indexOf(cs)))
      .map {
        case (exList, idx) =>
          if (idx >= 0) {
            val csKey = hashCs(cs)
            if (_dbWaitingContinuations.get(txn, csKey).isEmpty) {
              val resList = dropIndex(exList, idx)
              if (resList.nonEmpty) {
                _dbJoins.put(txn, joinKey, toByteBuffer(resList.map(toBytesList(_))))
              } else {
                _dbJoins.delete(txn, joinKey)
                collectGarbage(txn, joinKey, joinsCollected = true)
              }
            }
          } else {
            throw new IllegalArgumentException(s"removeJoin: $cs is not a member of $exList")
          }
      }
      .getOrElse(())
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    val joinKey = hashCs(Seq(c))
    _dbJoins.delete(txn, joinKey)
    collectGarbage(txn, joinKey)
  }

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      _dbKeys.isEmpty(txn) &&
      _dbData.isEmpty(txn) &&
      _dbWaitingContinuations.isEmpty(txn) &&
      _dbJoins.isEmpty(txn)
    }

  private[rspace] def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbData.drop(txn)
      _dbWaitingContinuations.drop(txn)
      _dbJoins.drop(txn)
    }

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getWaitingContinuations(txn, channels).map(_.patterns)

  def toMap: Map[Seq[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      optionalWithResource(_dbKeys.iterateKeys(txn)) { it =>
        it.map(channelKey => {
            val channels: Seq[C] = getKey(txn, channelKey)
            val data             = getData(txn, channels)
            val wcs              = getWaitingContinuations(txn, channels)
            (channels, Row(data, wcs))
          })
          .toMap
      }
    }
}

object Store {
  private[rspace] def toByteVector[T](value: T)(implicit st: Serialize[T]): ByteVector =
    ByteVector(st.encode(value))

  private[rspace] def fromByteVector[T](vector: ByteVector)(implicit st: Serialize[T]): T =
    st.decode(vector.toArray) match {
      case Left(err)     => throw new Exception(err)
      case Right(result) => result
    }

  private[rspace] def toByteBuffer[T](value: T, codec: Codec[T])(
      implicit allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(toBitVector(value, codec))

  private[rspace] def toByteBuffer[T](values: Seq[T], codec: Codec[Seq[T]])(
      implicit allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(toBitVector(values, codec))

  private[rspace] def toByteBuffer[T](values: Seq[T])(implicit st: Serialize[T],
                                                      allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(toBitVector(toBytesList(values), bytesListCodec))

  private[rspace] def toBytesList[T](values: Seq[T])(implicit st: Serialize[T]): Seq[ByteVector] =
    values.map(st.encode).map(ByteVector(_))

  private[rspace] def fromBytesList[T](bytesList: Seq[ByteVector])(
      implicit st: Serialize[T]): Seq[T] =
    bytesList
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def toBytesLists(byteBuffer: ByteBuffer): Seq[Seq[ByteVector]] =
    fromBitVector(BitVector(byteBuffer), bytesListCodec)
      .map(vec => fromBitVector(vec.bits, bytesListCodec))

  private[rspace] def toByteBuffer(lists: Seq[Seq[ByteVector]])(
      implicit allocator: IBufferAllocator): ByteBuffer = {
    val bl = lists.map(vec => toBitVector(vec, bytesListCodec).toByteVector)
    toByteBuffer(bl, bytesListCodec)
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(implicit st: Serialize[T]): Seq[T] =
    fromBitVector(BitVector(byteBuffer), bytesListCodec)
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer, codec: Codec[T]): T =
    fromBitVector(BitVector(byteBuffer), codec)

  private[rspace] def hashBytes(byteBuffer: ByteBuffer)(
      implicit allocator: IBufferAllocator): ByteBuffer = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    byteBuffer.reset()
    hashBytes(fetched)
  }

  private[rspace] def hashBytes(bytes: Array[Byte])(
      implicit allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(MessageDigest.getInstance("SHA-256").digest(bytes))
}
