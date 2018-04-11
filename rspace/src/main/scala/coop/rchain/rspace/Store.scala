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

  private[rspace] def hashChannels(channels: Seq[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(channels)(st, allocator))

  private[rspace] def getChannels(txn: T, channelsHash: H): Seq[C] =
    _dbKeys.get(txn, channelsHash).map(fromByteBuffer[C]).getOrElse(Seq.empty[C])

  private[rspace] def putChannels(txn: T, channels: Seq[C]): H = {
    val channelsBytes = toByteBuffer(channels)
    val channelsHash  = hashBytes(channelsBytes)
    _dbKeys.put(txn, channelsHash, channelsBytes)
    channelsHash
  }

  private[this] def readDatumByteses(txn: T, channelsHash: H): Option[Seq[DatumBytes]] =
    _dbData.get(txn, channelsHash).map(fromByteBuffer(_, datumBytesesCodec))

  private[this] def writeDatumByteses(txn: T, channelsHash: H, values: Seq[DatumBytes]): Unit =
    if (values.nonEmpty) {
      _dbData.put(txn, channelsHash, toByteBuffer(values, datumBytesesCodec))
    } else {
      _dbData.delete(txn, channelsHash)
      collectGarbage(txn, channelsHash, waitingContinuationsCollected = true)
    }

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val channelsHash    = putChannels(txn, channels)
    val newDatumBytes   = DatumBytes(toByteVector(datum.a), datum.persist)
    val oldDatumByteses = readDatumByteses(txn, channelsHash).getOrElse(Seq.empty[DatumBytes])
    writeDatumByteses(txn, channelsHash, newDatumBytes +: oldDatumByteses)
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] = {
    val channelsHash = hashChannels(channels)
    readDatumByteses(txn, channelsHash)
      .map(_.map(bytes => Datum(fromByteVector[A](bytes.datumBytes), bytes.persist)))
      .getOrElse(Seq.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     channelsHash: H,
                     dataCollected: Boolean = false,
                     waitingContinuationsCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(kvTable: KeyValueTable): Boolean = kvTable.get(txn, channelsHash).isEmpty

    val readyToCollect = (dataCollected || isEmpty(_dbData)) &&
      (waitingContinuationsCollected || isEmpty(_dbWaitingContinuations)) &&
      (joinsCollected || isEmpty(_dbJoins))

    if (readyToCollect) {
      _dbKeys.delete(txn, channelsHash)
    }
  }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    readDatumByteses(txn, channelsHash) match {
      case Some(datumByteses) =>
        writeDatumByteses(txn, channelsHash, util.dropIndex(datumByteses, index))
      case None => throw new IllegalArgumentException(s"removeDatum: no values at $channels")
    }
  }

  private[this] def readWaitingContinuationByteses(
      txn: T,
      channelsHash: H): Option[Seq[WaitingContinuationBytes]] =
    _dbWaitingContinuations
      .get(txn, channelsHash)
      .map(fromByteBuffer(_, waitingContinuationsSeqCodec))

  private[this] def writeWaitingContinuationByteses(txn: T,
                                                    channelsHash: H,
                                                    values: Seq[WaitingContinuationBytes]): Unit =
    if (values.nonEmpty) {
      _dbWaitingContinuations.put(txn,
                                  channelsHash,
                                  toByteBuffer(values, waitingContinuationsSeqCodec))
    } else {
      _dbWaitingContinuations.delete(txn, channelsHash)
      collectGarbage(txn, channelsHash, waitingContinuationsCollected = true)
    }

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val channelsHash = putChannels(txn, channels)
    val waitingContinuationBytes =
      WaitingContinuationBytes(toByteVectorSeq(continuation.patterns),
                               toByteVector(continuation.continuation),
                               continuation.persist)
    val waitingContinuationByteses =
      readWaitingContinuationByteses(txn, channelsHash).getOrElse(
        Seq.empty[WaitingContinuationBytes])
    writeWaitingContinuationByteses(txn,
                                    channelsHash,
                                    waitingContinuationBytes +: waitingContinuationByteses)
  }

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]] = {
    val channelsHash = hashChannels(channels)
    readWaitingContinuationByteses(txn, channelsHash)
      .map(
        _.map(
          waitingContinuations =>
            WaitingContinuation(fromByteVectors[P](waitingContinuations.patterns),
                                fromByteVector[K](waitingContinuations.kvalue),
                                waitingContinuations.persist)))
      .getOrElse(Seq.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    readWaitingContinuationByteses(txn, channelsHash) match {
      case Some(waitingContinuationByteses) =>
        writeWaitingContinuationByteses(txn,
                                        channelsHash,
                                        util.dropIndex(waitingContinuationByteses, index))
      case None =>
        throw new IllegalArgumentException(s"removeWaitingContinuations: no values at $channels")
    }
  }

  private[rspace] def removeAll(txn: T, channels: Seq[C]): Unit = {
    val channelsHash = hashChannels(channels)
    readWaitingContinuationByteses(txn, channelsHash).foreach { _ =>
      writeWaitingContinuationByteses(txn, channelsHash, Seq.empty)
    }
    readDatumByteses(txn, channelsHash).foreach { _ =>
      writeDatumByteses(txn, channelsHash, Seq.empty)
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(channel))
    val oldJoins =
      _dbJoins
        .get(txn, joinKey)
        .map(toByteVectors)
        .getOrElse(Seq.empty[Seq[ByteVector]])

    val newJoin = toByteVectorSeq(channels)
    if (!oldJoins.contains(newJoin)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(newJoin +: oldJoins))
    }
  }

  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]] = {
    val joinKey = hashChannels(Seq(channel))
    _dbJoins
      .get(txn, joinKey)
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .getOrElse(Seq.empty[Seq[C]])
  }

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(channel))
    _dbJoins
      .get(txn, joinKey)
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .map(exSeq => (exSeq, exSeq.indexOf(channels)))
      .map {
        case (exSeq, idx) =>
          if (idx >= 0) {
            val channelsHash = hashChannels(channels)
            if (_dbWaitingContinuations.get(txn, channelsHash).isEmpty) {
              val resSeq = dropIndex(exSeq, idx)
              if (resSeq.nonEmpty) {
                _dbJoins.put(txn, joinKey, toByteBuffer(resSeq.map(toByteVectorSeq(_))))
              } else {
                _dbJoins.delete(txn, joinKey)
                collectGarbage(txn, joinKey, joinsCollected = true)
              }
            }
          } else {
            throw new IllegalArgumentException(s"removeJoin: $channels is not a member of $exSeq")
          }
      }
      .getOrElse(())
  }

  private[rspace] def removeAllJoins(txn: T, channel: C): Unit = {
    val joinKey = hashChannels(Seq(channel))
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
    getWaitingContinuation(txn, channels).map(_.patterns)

  def toMap: Map[Seq[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      optionalWithResource(_dbKeys.iterateKeys(txn)) { it =>
        it.map(channelKey => {
            val channels: Seq[C]     = getChannels(txn, channelKey)
            val data                 = getData(txn, channels)
            val waitingContinuations = getWaitingContinuation(txn, channels)
            (channels, Row(data, waitingContinuations))
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
    allocator.toByteBuffer(toBitVector(toByteVectorSeq(values), byteVectorsCodec))

  private[rspace] def toByteVectorSeq[T](values: Seq[T])(
      implicit st: Serialize[T]): Seq[ByteVector] =
    values.map(st.encode).map(ByteVector(_))

  private[rspace] def fromByteVectors[T](bytesList: Seq[ByteVector])(
      implicit st: Serialize[T]): Seq[T] =
    bytesList
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def toByteVectors(byteBuffer: ByteBuffer): Seq[Seq[ByteVector]] =
    fromBitVector(BitVector(byteBuffer), byteVectorsCodec)
      .map(vec => fromBitVector(vec.bits, byteVectorsCodec))

  private[rspace] def toByteBuffer(vectors: Seq[Seq[ByteVector]])(
      implicit allocator: IBufferAllocator): ByteBuffer = {
    val bl = vectors.map(vec => toBitVector(vec, byteVectorsCodec).toByteVector)
    toByteBuffer(bl, byteVectorsCodec)
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(implicit st: Serialize[T]): Seq[T] =
    fromBitVector(BitVector(byteBuffer), byteVectorsCodec)
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
