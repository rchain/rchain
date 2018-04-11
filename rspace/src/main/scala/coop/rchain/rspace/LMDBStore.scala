package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.immutable.Seq

import coop.rchain.rspace.internal._
import coop.rchain.rspace.internal.scodecs._
import coop.rchain.rspace.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits._

import scala.collection.JavaConverters._

/**
  * The main store class.
  *
  * To create an instance, use [[LMDBStore.create]].
  */
class LMDBStore[C, P, A, K] private (env: Env[ByteBuffer],
                                     _dbKeys: Dbi[ByteBuffer],
                                     _dbPsKs: Dbi[ByteBuffer],
                                     _dbAs: Dbi[ByteBuffer],
                                     _dbJoins: Dbi[ByteBuffer])(implicit
                                                                sc: Serialize[C],
                                                                sp: Serialize[P],
                                                                sa: Serialize[A],
                                                                sk: Serialize[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  import coop.rchain.rspace.LMDBStore._

  private[rspace] type H = ByteBuffer

  private[rspace] type T = Txn[ByteBuffer]

  private[rspace] def hashChannels(cs: Seq[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(cs)(st))

  private[rspace] def getChannels(txn: T, s: H): Seq[C] =
    Option(_dbKeys.get(txn, s)).map(fromByteBuffer[C]).getOrElse(Seq.empty[C])

  private[rspace] def putChannels(txn: T, channels: Seq[C]): H = {
    val packedCs = toByteBuffer(channels)
    val keyCs    = hashBytes(packedCs)
    _dbKeys.put(txn, keyCs, packedCs)
    keyCs
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

  private[this] def readDatumByteses(txn: T, keyCs: H): Option[Seq[DatumBytes]] =
    Option(_dbAs.get(txn, keyCs)).map(fromByteBuffer(_, datumBytesesCodec))

  private[this] def writeDatumByteses(txn: T, keyCs: H, values: Seq[DatumBytes]): Unit =
    if (values.nonEmpty) {
      _dbAs.put(txn, keyCs, toByteBuffer(values, datumBytesesCodec))
    } else {
      _dbAs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, wcsCollected = true)
    }

  private[rspace] def putA(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val keyCs   = putChannels(txn, channels)
    val binAs   = DatumBytes(toByteVector(datum.a), datum.persist)
    val asksSeq = readDatumByteses(txn, keyCs).getOrElse(Seq.empty[DatumBytes])
    writeDatumByteses(txn, keyCs, binAs +: asksSeq)
  }

  private[rspace] def getAs(txn: T, channels: Seq[C]): Seq[Datum[A]] = {
    val keyCs = hashChannels(channels)
    readDatumByteses(txn, keyCs)
      .map { (byteses: Seq[DatumBytes]) =>
        byteses.map((bytes: DatumBytes) =>
          Datum(fromByteVector[A](bytes.datumBytes), bytes.persist))
      }
      .getOrElse(Seq.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     keyCs: H,
                     asCollected: Boolean = false,
                     wcsCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(dbi: Dbi[ByteBuffer]): Boolean =
      dbi.get(txn, keyCs) == null

    val readyToCollect = (asCollected || isEmpty(_dbAs)) &&
      (wcsCollected || isEmpty(_dbPsKs)) &&
      (joinsCollected || isEmpty(_dbJoins))

    if (readyToCollect) {
      _dbKeys.delete(txn, keyCs)
    }
  }

  private[rspace] def removeA(txn: T, channel: C, index: Int): Unit =
    removeA(txn, Seq(channel), index)

  private[rspace] def removeA(txn: T, channels: Seq[C], index: Int): Unit = {
    val keyCs = hashChannels(channels)
    readDatumByteses(txn, keyCs) match {
      case Some(as) => writeDatumByteses(txn, keyCs, util.dropIndex(as, index))
      case None     => throw new IllegalArgumentException(s"removeA: no values at $channels")
    }
  }

  private[this] def readWaitingContinuationByteses(txn: T,
                                             keyCs: H): Option[Seq[WaitingContinuationBytes]] =
    Option(_dbPsKs.get(txn, keyCs)).map(fromByteBuffer(_, waitingContinuationsSeqCodec))

  private[this] def writeWaitingContinuationByteses(txn: T,
                                              keyCs: H,
                                              values: Seq[WaitingContinuationBytes]): Unit =
    if (values.nonEmpty) {
      _dbPsKs.put(txn, keyCs, toByteBuffer(values, waitingContinuationsSeqCodec))
    } else {
      _dbPsKs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, wcsCollected = true)
    }

  private[rspace] def putK(txn: T,
                           channels: Seq[C],
                           continuation: WaitingContinuation[P, K]): Unit = {
    val keyCs = putChannels(txn, channels)
    val binWcs =
      WaitingContinuationBytes(toByteVectorSeq(continuation.patterns),
                               toByteVector(continuation.continuation),
                               continuation.persist)
    val wcsLst =
      readWaitingContinuationByteses(txn, keyCs).getOrElse(Seq.empty[WaitingContinuationBytes])
    writeWaitingContinuationByteses(txn, keyCs, binWcs +: wcsLst)
  }

  private[rspace] def getPsK(txn: T, curr: Seq[C]): Seq[WaitingContinuation[P, K]] = {
    val keyCs = hashChannels(curr)
    readWaitingContinuationByteses(txn, keyCs)
      .map(
        _.map(
          wcs =>
            WaitingContinuation(fromByteVectors[P](wcs.patterns),
                                fromByteVector[K](wcs.kvalue),
                                wcs.persist)))
      .getOrElse(Seq.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removePsK(txn: T, channels: Seq[C], index: Int): Unit = {
    val keyCs = hashChannels(channels)
    readWaitingContinuationByteses(txn, keyCs) match {
      case Some(wcs) => writeWaitingContinuationByteses(txn, keyCs, util.dropIndex(wcs, index))
      case None      => throw new IllegalArgumentException(s"removePsK: no values at $channels")
    }
  }

  private[rspace] def removeAll(txn: Txn[ByteBuffer], channels: Seq[C]): Unit = {
    val keyCs = hashChannels(channels)
    readWaitingContinuationByteses(txn, keyCs).foreach { _ =>
      writeWaitingContinuationByteses(txn, keyCs, Seq.empty)
    }
    readDatumByteses(txn, keyCs).foreach { _ =>
      writeDatumByteses(txn, keyCs, Seq.empty)
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  private[rspace] def addJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(c))
    val oldJoinsBv =
      Option(_dbJoins.get(txn, joinKey))
        .map(toByteVectors)
        .getOrElse(Seq.empty[Seq[ByteVector]])

    val newJoin = toByteVectorSeq(cs)
    if (!oldJoinsBv.contains(newJoin)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(newJoin +: oldJoinsBv))
    }
  }

  private[rspace] def getJoin(txn: T, c: C): Seq[Seq[C]] = {
    val joinKey = hashChannels(Seq(c))
    Option(_dbJoins.get(txn, joinKey))
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .getOrElse(Seq.empty[Seq[C]])
  }

  private[rspace] def removeJoin(txn: T, c: C, cs: Seq[C]): Unit = {
    val joinKey = hashChannels(Seq(c))
    Option(_dbJoins.get(txn, joinKey))
      .map(toByteVectors)
      .map(_.map(fromByteVectors[C]))
      .map(exSeq => (exSeq, exSeq.indexOf(cs)))
      .map {
        case (exSeq, idx) =>
          if (idx >= 0) {
            val csKey = hashChannels(cs)
            if (_dbPsKs.get(txn, csKey) == null) {
              val resSeq = dropIndex(exSeq, idx)
              if (resSeq.nonEmpty) {
                _dbJoins.put(txn, joinKey, toByteBuffer(resSeq.map(toByteVectorSeq(_))))
              } else {
                _dbJoins.delete(txn, joinKey)
                collectGarbage(txn, joinKey, joinsCollected = true)
              }
            }
          } else {
            throw new IllegalArgumentException(s"removeJoin: $cs is not a member of $exSeq")
          }
      }
      .getOrElse(())
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    val joinKey = hashChannels(Seq(c))
    _dbJoins.delete(txn, joinKey)
    collectGarbage(txn, joinKey)
  }

  private[rspace] def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbAs.drop(txn)
      _dbPsKs.drop(txn)
      _dbJoins.drop(txn)
    }

  def close(): Unit = {
    _dbKeys.close()
    _dbAs.close()
    _dbPsKs.close()
    _dbJoins.close()
    env.close()
  }

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      !_dbKeys.iterate(txn).hasNext &&
      !_dbAs.iterate(txn).hasNext &&
      !_dbPsKs.iterate(txn).hasNext &&
      !_dbJoins.iterate(txn).hasNext
    }

  def getPs(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getPsK(txn, channels).map(_.patterns)

  def toMap: Map[Seq[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      val keyRange: KeyRange[ByteBuffer] = KeyRange.all()
      withResource(_dbKeys.iterate(txn, keyRange)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala.map { (x: CursorIterator.KeyVal[ByteBuffer]) =>
          val channels: Seq[C] = getChannels(txn, x.`key`())
          val data             = getAs(txn, channels)
          val wks              = getPsK(txn, channels)
          (channels, Row(data, wks))
        }.toMap
      }
    }
}

object LMDBStore {
  private[this] val keysTableName: String  = "Keys"
  private[this] val psksTableName: String  = "PsKs"
  private[this] val asTableName: String    = "As"
  private[this] val joinsTableName: String = "Joins"

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
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    val dbKeys: Dbi[ByteBuffer]  = env.openDbi(keysTableName, MDB_CREATE)
    val dbPsKs: Dbi[ByteBuffer]  = env.openDbi(psksTableName, MDB_CREATE)
    val dbAs: Dbi[ByteBuffer]    = env.openDbi(asTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBStore[C, P, A, K](env, dbKeys, dbPsKs, dbAs, dbJoins)(sc, sp, sa, sk)
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

  private[rspace] def toByteBuffer(vector: BitVector): ByteBuffer = {
    val bytes          = vector.bytes
    val bb: ByteBuffer = ByteBuffer.allocateDirect(bytes.size.toInt)
    bytes.copyToBuffer(bb)
    bb.flip()
    bb
  }

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
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(bytes)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }
}
