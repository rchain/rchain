package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rspace.internal._
import coop.rchain.rspace.internal.scodecs._
import coop.rchain.rspace.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits._

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import coop.rchain.shared.PathOps.RichPath

/**
  * The main store class.
  *
  * To create an instance, use [[LMDBStore.create]].
  */
class LMDBStore[C, P, A, K] private (env: Env[ByteBuffer],
                                     databasePath: Path,
                                     _dbKeys: Dbi[ByteBuffer],
                                     _dbWaitingContinuations: Dbi[ByteBuffer],
                                     _dbData: Dbi[ByteBuffer],
                                     _dbJoins: Dbi[ByteBuffer])(implicit
                                                                sc: Serialize[C],
                                                                sp: Serialize[P],
                                                                sa: Serialize[A],
                                                                sk: Serialize[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  private implicit val codecC: Codec[C] = sc.toCodec
  private implicit val codecP: Codec[P] = sp.toCodec
  private implicit val codecA: Codec[A] = sa.toCodec
  private implicit val codecK: Codec[K] = sk.toCodec

  import coop.rchain.rspace.LMDBStore._

  private[rspace] type H = ByteBuffer

  private[rspace] type T = Txn[ByteBuffer]

  private[rspace] def hashChannels(channels: Seq[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(channels)(st))

  private[rspace] def getChannels(txn: T, channelsHash: H): Seq[C] =
    Option(_dbKeys.get(txn, channelsHash)).map(fromByteBuffer[C]).getOrElse(Seq.empty[C])

  private[rspace] def putChannels(txn: T, channels: Seq[C]): H = {
    val channelsBytes = toByteBuffer(channels)
    val channelsHash  = hashBytes(channelsBytes)
    _dbKeys.put(txn, channelsHash, channelsBytes)
    channelsHash
  }

  private[rspace] def createTxnRead(): T = env.txnRead

  private[rspace] def createTxnWrite(): T = env.txnWrite

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
    try {
      val ret: R = f(txn)
      txn.commit()
      ret
    } catch {
      case ex: Throwable =>
        txn.abort()
        throw ex
    } finally {
      txn.close()
    }

  private[this] def readDatumByteses(txn: T, channelsHash: H): Option[Seq[DatumBytes]] =
    Option(_dbData.get(txn, channelsHash)).map(fromByteBuffer(_, datumBytesesCodec))

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

  private[rspace] def collectGarbage(txn: T,
                                     channelsHash: H,
                                     dataCollected: Boolean = false,
                                     waitingContinuationsCollected: Boolean = false,
                                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(dbi: Dbi[ByteBuffer]): Boolean =
      dbi.get(txn, channelsHash) == null

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
    Option(_dbWaitingContinuations.get(txn, channelsHash))
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
        throw new IllegalArgumentException(s"removeWaitingContinuation: no values at $channels")
    }
  }

  private[rspace] def removeAll(txn: Txn[ByteBuffer], channels: Seq[C]): Unit = {
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
    val oldJoinsBv =
      Option(_dbJoins.get(txn, joinKey))
        .map(toByteVectors)
        .getOrElse(Seq.empty[Seq[ByteVector]])

    val newJoin = toByteVectorSeq(channels)
    if (!oldJoinsBv.contains(newJoin)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(newJoin +: oldJoinsBv))
    }
  }

  private[rspace] def getJoin(txn: T, channel: C): Seq[Seq[C]] = {
    val joinKey = hashChannels(Seq(channel))
    Option(_dbJoins.get(txn, joinKey))
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .getOrElse(Seq.empty[Seq[C]])
  }

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(channel))
    Option(_dbJoins.get(txn, joinKey))
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .map(exSeq => (exSeq, exSeq.indexOf(channels)))
      .map {
        case (exSeq, idx) =>
          if (idx >= 0) {
            val channelsHash = hashChannels(channels)
            if (_dbWaitingContinuations.get(txn, channelsHash) == null) {
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

  private[rspace] def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbData.drop(txn)
      _dbWaitingContinuations.drop(txn)
      _dbJoins.drop(txn)
    }

  def close(): Unit = {
    _dbKeys.close()
    _dbData.close()
    _dbWaitingContinuations.close()
    _dbJoins.close()
    env.close()
  }

  def getStoreSize: StoreSize =
    StoreSize(databasePath.folderSize, env.stat().entries)

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      !_dbKeys.iterate(txn).hasNext &&
      !_dbData.iterate(txn).hasNext &&
      !_dbWaitingContinuations.iterate(txn).hasNext &&
      !_dbJoins.iterate(txn).hasNext
    }

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getWaitingContinuation(txn, channels).map(_.patterns)

  def toMap: Map[Seq[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      val keyRange: KeyRange[ByteBuffer] = KeyRange.all()
      withResource(_dbKeys.iterate(txn, keyRange)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala.map { (x: CursorIterator.KeyVal[ByteBuffer]) =>
          val channels: Seq[C]     = getChannels(txn, x.`key`())
          val data                 = getData(txn, channels)
          val waitingContinuations = getWaitingContinuation(txn, channels)
          (channels, Row(data, waitingContinuations))
        }.toMap
      }
    }
}

object LMDBStore {
  private[this] val keysTableName: String                 = "Keys"
  private[this] val waitingContinuationsTableName: String = "WaitingContinuations"
  private[this] val dataTableName: String                 = "Data"
  private[this] val joinsTableName: String                = "Joins"

  /**
    * Creates an instance of [[LMDBStore]]
    *
    * @param path    Path to the database files
    * @param mapSize Maximum size of the database, in bytes
    * @tparam C A type representing a channel
    * @tparam P A type representing a pattern
    * @tparam A A type representing a piece of data
    * @tparam K A type representing a continuation
    */
  def create[C, P, A, K](path: Path, mapSize: Long)(implicit sc: Serialize[C],
                                                    sp: Serialize[P],
                                                    sa: Serialize[A],
                                                    sk: Serialize[K]): LMDBStore[C, P, A, K] = {

    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(8)
        .setMaxReaders(126)
        .open(path.toFile)

    val dbKeys: Dbi[ByteBuffer] = env.openDbi(keysTableName, MDB_CREATE)
    val dbWaitingContinuations: Dbi[ByteBuffer] =
      env.openDbi(waitingContinuationsTableName, MDB_CREATE)
    val dbData: Dbi[ByteBuffer]  = env.openDbi(dataTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBStore[C, P, A, K](env, path, dbKeys, dbWaitingContinuations, dbData, dbJoins)(sc,
                                                                                          sp,
                                                                                          sa,
                                                                                          sk)
  }

  private[rspace] def toByteVector[T](value: T)(implicit st: Serialize[T]): ByteVector =
    ByteVector(st.encode(value))

  private[rspace] def fromByteVector[T](vector: ByteVector)(implicit st: Serialize[T]): T =
    st.decode(vector.toArray) match {
      case Left(err)     => throw new Exception(err)
      case Right(result) => result
    }

  private[rspace] def toByteBuffer[T](value: T, codec: Codec[T]): ByteBuffer =
    toByteBuffer(toBitVector(value, codec))

  private[rspace] def toByteBuffer[T](values: Seq[T])(implicit st: Serialize[T]): ByteBuffer =
    toByteBuffer(toBitVector(toByteVectorSeq(values), byteVectorsCodec))

  private[rspace] def toByteBuffer(byteVector: ByteVector): ByteBuffer = {
    val buffer: ByteBuffer = ByteBuffer.allocateDirect(byteVector.size.toInt)
    byteVector.copyToBuffer(buffer)
    buffer.flip()
    buffer
  }

  private[rspace] def toByteBuffer(bitVector: BitVector): ByteBuffer =
    toByteBuffer(bitVector.bytes)

  private[rspace] def toByteVectorSeq[T](values: Seq[T])(
      implicit st: Serialize[T]): Seq[ByteVector] =
    values.map(st.encode).map(ByteVector(_))

  private[rspace] def fromByteVectors[T](vectors: Seq[ByteVector])(
      implicit st: Serialize[T]): Seq[T] =
    vectors
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def toByteVectors(byteBuffer: ByteBuffer): Seq[Seq[ByteVector]] =
    fromBitVector(BitVector(byteBuffer), byteVectorsCodec)
      .map(x => fromBitVector(x.bits, byteVectorsCodec))

  private[rspace] def toByteBuffer(vectors: Seq[Seq[ByteVector]]): ByteBuffer = {
    val bl = vectors.map(toBitVector(_, byteVectorsCodec).toByteVector)
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

  private[rspace] def hashBytes(byteBuffer: ByteBuffer): ByteBuffer = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    byteBuffer.reset()
    hashBytes(fetched)
  }

  private[rspace] def hashBytes(bytes: Array[Byte]): ByteBuffer = {
    val dataArr    = Blake2b256.hash(bytes)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }
}
