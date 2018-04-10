package coop.rchain.rspace

import java.nio.ByteBuffer
import java.security.MessageDigest

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
  val _dbAs: KeyValueTable
  val _dbPsKs: KeyValueTable
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

  private[rspace] def hashCs(cs: List[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(cs)(st, allocator))

  private[rspace] def getKey(txn: T, s: H): List[C] =
    _dbKeys.get(txn, s).map(fromByteBuffer[C]).getOrElse(List.empty[C])

  private[rspace] def putCsH(txn: T, channels: List[C]): H = {
    val packedCs = toByteBuffer(channels)
    val keyCs    = hashBytes(packedCs)
    _dbKeys.put(txn, keyCs, packedCs)
    keyCs
  }

  private[this] def readAsBytesList(txn: T, keyCs: H): Option[List[DatumBytes]] =
    _dbAs.get(txn, keyCs).map(fromByteBuffer(_, asBytesListCodec))

  private[this] def writeAsBytesList(txn: T, keyCs: H, values: List[DatumBytes]): Unit =
    if (values.nonEmpty) {
      _dbAs.put(txn, keyCs, toByteBuffer(values, asBytesListCodec))
    } else {
      _dbAs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, psKsCollected = true)
    }

  private[rspace] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit = {
    val keyCs         = putCsH(txn, channels)
    val datumBytes    = DatumBytes(toByteVector(datum.a), datum.persist)
    val datumBytesOld = readAsBytesList(txn, keyCs).getOrElse(List.empty[DatumBytes])
    writeAsBytesList(txn, keyCs, datumBytes +: datumBytesOld)
  }

  private[rspace] def getAs(txn: T, channels: List[C]): List[Datum[A]] = {
    val keyCs = hashCs(channels)
    readAsBytesList(txn, keyCs)
      .map(_.map(bytes => Datum(fromByteVector[A](bytes.datumBytes), bytes.persist)))
      .getOrElse(List.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     keyCs: H,
                     asCollected: Boolean = false,
                     psKsCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(kvTable: KeyValueTable): Boolean = kvTable.get(txn, keyCs).isEmpty

    val readyToCollect = (asCollected || isEmpty(_dbAs)) &&
      (psKsCollected || isEmpty(_dbPsKs)) &&
      (joinsCollected || isEmpty(_dbJoins))

    if (readyToCollect) {
      _dbKeys.delete(txn, keyCs)
    }
  }

  private[rspace] def removeA(txn: T, channel: C, index: Int): Unit =
    removeA(txn, List(channel), index)

  private[rspace] def removeA(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readAsBytesList(txn, keyCs) match {
      case Some(loadedBytesList) =>
        writeAsBytesList(txn, keyCs, util.dropIndex(loadedBytesList, index))
      case None => throw new IllegalArgumentException(s"removeDatum: no values at $channels")
    }
  }

  private[this] def readPsKsBytesList(txn: T, keyCs: H): Option[List[WaitingContinuationBytes]] =
    _dbPsKs
      .get(txn, keyCs)
      .map(fromByteBuffer(_, waitingContinuationsBytesListCodec))

  private[this] def writePsKsBytesList(txn: T,
                                       keyCs: H,
                                       values: List[WaitingContinuationBytes]): Unit =
    if (values.nonEmpty) {
      _dbPsKs.put(txn, keyCs, toByteBuffer(values, waitingContinuationsBytesListCodec))
    } else {
      _dbPsKs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, psKsCollected = true)
    }

  private[rspace] def putK(txn: T,
                           channels: List[C],
                           continuation: WaitingContinuation[P, K]): Unit = {
    val keyCs = putCsH(txn, channels)
    val waitingContinuationBytes =
      WaitingContinuationBytes(toBytesList(continuation.patterns),
                               toByteVector(continuation.continuation),
                               continuation.persist)
    val waitingContinuationsList =
      readPsKsBytesList(txn, keyCs).getOrElse(List.empty[WaitingContinuationBytes])
    writePsKsBytesList(txn, keyCs, waitingContinuationBytes +: waitingContinuationsList)
  }

  private[rspace] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]] = {
    val keyCs = hashCs(curr)
    readPsKsBytesList(txn, keyCs)
      .map(
        _.map(
          wcs =>
            WaitingContinuation(fromBytesList[P](wcs.patterns),
                                fromByteVector[K](wcs.kvalue),
                                wcs.persist)))
      .getOrElse(List.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readPsKsBytesList(txn, keyCs) match {
      case Some(wcs) => writePsKsBytesList(txn, keyCs, util.dropIndex(wcs, index))
      case None =>
        throw new IllegalArgumentException(s"removeWaitingContinuations: no values at $channels")
    }
  }

  private[rspace] def removeAll(txn: T, channels: List[C]): Unit = {
    val keyCs = hashCs(channels)
    readPsKsBytesList(txn, keyCs).foreach { _ =>
      writePsKsBytesList(txn, keyCs, List.empty)
    }
    readAsBytesList(txn, keyCs).foreach { _ =>
      writeAsBytesList(txn, keyCs, List.empty)
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val oldCsList =
      _dbJoins
        .get(txn, joinKey)
        .map(toBytesLists)
        .getOrElse(List.empty[List[ByteVector]])

    val addBl = toBytesList(cs)
    if (!oldCsList.contains(addBl)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(addBl +: oldCsList))
    }
  }

  private[rspace] def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey = hashCs(List(c))
    _dbJoins
      .get(txn, joinKey)
      .map(toBytesLists)
      .map(_.map(fromBytesList[C]))
      .getOrElse(List.empty[List[C]])
  }

  private[rspace] def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    _dbJoins
      .get(txn, joinKey)
      .map(toBytesLists)
      .map(_.map(fromBytesList[C]))
      .map(exList => (exList, exList.indexOf(cs)))
      .map {
        case (exList, idx) =>
          if (idx >= 0) {
            val csKey = hashCs(cs)
            if (_dbPsKs.get(txn, csKey).isEmpty) {
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
    val joinKey = hashCs(List(c))
    _dbJoins.delete(txn, joinKey)
    collectGarbage(txn, joinKey)
  }

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      _dbKeys.isEmpty(txn) &&
      _dbAs.isEmpty(txn) &&
      _dbPsKs.isEmpty(txn) &&
      _dbJoins.isEmpty(txn)
    }

  private[rspace] def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbAs.drop(txn)
      _dbPsKs.drop(txn)
      _dbJoins.drop(txn)
    }

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    getPsK(txn, channels).map(_.patterns)

  def toMap: Map[List[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      optionalWithResource(_dbKeys.iterateKeys(txn)) { it =>
        it.map(channelKey => {
            val channels: List[C] = getKey(txn, channelKey)
            val data              = getAs(txn, channels)
            val wcs               = getPsK(txn, channels)
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

  private[rspace] def toByteBuffer[T](values: List[T], codec: Codec[List[T]])(
      implicit allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(toBitVector(values, codec))

  private[rspace] def toByteBuffer[T](values: List[T])(implicit st: Serialize[T],
                                                       allocator: IBufferAllocator): ByteBuffer =
    allocator.toByteBuffer(toBitVector(toBytesList(values), bytesListCodec))

  private[rspace] def toBytesList[T](values: List[T])(implicit st: Serialize[T]): List[ByteVector] =
    values.map(st.encode).map(ByteVector(_))

  private[rspace] def fromBytesList[T](bytesList: List[ByteVector])(
      implicit st: Serialize[T]): List[T] =
    bytesList
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def toBytesLists(byteBuffer: ByteBuffer): List[List[ByteVector]] =
    fromBitVector(BitVector(byteBuffer), bytesListCodec)
      .map(vec => fromBitVector(vec.bits, bytesListCodec))

  private[rspace] def toByteBuffer(lists: List[List[ByteVector]])(
      implicit allocator: IBufferAllocator): ByteBuffer = {
    val bl = lists.map(vec => toBitVector(vec, bytesListCodec).toByteVector)
    toByteBuffer(bl, bytesListCodec)
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(
      implicit st: Serialize[T]): List[T] =
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
