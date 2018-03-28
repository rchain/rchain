package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path
import java.security.MessageDigest

import scala.collection.JavaConverters._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.rspace.Serialize.mkProtobufInstance
import coop.rchain.rspace.datamodels._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._

import scala.collection.mutable

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
                                                                sk: Serialize[K],
                                                                sbl: Serialize[BytesList])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  import coop.rchain.rspace.LMDBStore._

  private[rspace] type H = ByteBuffer

  private[rspace] type T = Txn[ByteBuffer]

  private[rspace] def hashCs(cs: List[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(cs)(st))

  private[rspace] def getKey(txn: T, s: H): List[C] =
    Option(_dbKeys.get(txn, s)).map(fromByteBuffer[C]).getOrElse(List.empty[C])

  private[rspace] def putCsH(txn: T, channels: List[C]): H = {
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

  private[this] def readAsBytesList(txn: T, keyCs: H): Option[List[AsBytes]] =
    Option(_dbAs.get(txn, keyCs)).map(bytes => {
      val fetched = new Array[Byte](bytes.remaining())
      ignore {
        bytes.get(fetched)
      }
      AsBytesList.parseFrom(fetched).values.toList
    })

  private[this] def writeAsBytesList(txn: T, keyCs: H, values: List[AsBytes]): Unit =
    if (values.nonEmpty) {
      val toWrite        = AsBytesList().withValues(values).toByteArray
      val bb: ByteBuffer = ByteBuffer.allocateDirect(toWrite.length)
      bb.put(toWrite).flip()
      _dbAs.put(txn, keyCs, bb)
    } else {
      _dbAs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, psksCollected = true)
    }

  private[rspace] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit = {
    val keyCs   = putCsH(txn, channels)
    val binAs   = AsBytes().withAvalue(toByteString(datum.a)).withPersist(datum.persist)
    val asksLst = readAsBytesList(txn, keyCs).getOrElse(List.empty[AsBytes])
    writeAsBytesList(txn, keyCs, binAs :: asksLst)
  }

  private[rspace] def getAs(txn: T, channels: List[C]): List[Datum[A]] = {
    val keyCs = hashCs(channels)
    readAsBytesList(txn, keyCs)
      .map { (byteses: List[AsBytes]) =>
        byteses.map((bytes: AsBytes) => Datum(fromByteString[A](bytes.avalue), bytes.persist))
      }
      .getOrElse(List.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     keyCs: H,
                     asCollected: Boolean = false,
                     psksCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(dbi: Dbi[ByteBuffer]): Boolean =
      dbi.get(txn, keyCs) == null

    val readyToCollect = (asCollected || isEmpty(_dbAs)) &&
      (psksCollected || isEmpty(_dbPsKs)) &&
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
      case Some(as) => writeAsBytesList(txn, keyCs, util.dropIndex(as, index))
      case None     => throw new IllegalArgumentException(s"removeA: no values at $channels")
    }
  }

  private[this] def readPsKsBytesList(txn: T, keyCs: H): Option[List[PsKsBytes]] =
    Option(_dbPsKs.get(txn, keyCs)).map(bytes => {
      val fetched = new Array[Byte](bytes.remaining())
      ignore {
        bytes.get(fetched)
      }
      PsKsBytesList.parseFrom(fetched).values.toList
    })

  private[this] def writePsKsBytesList(txn: T, keyCs: H, values: List[PsKsBytes]): Unit =
    if (values.nonEmpty) {
      val toWrite        = PsKsBytesList().withValues(values).toByteArray
      val bb: ByteBuffer = ByteBuffer.allocateDirect(toWrite.length)
      bb.put(toWrite).flip()
      _dbPsKs.put(txn, keyCs, bb)
    } else {
      _dbPsKs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, psksCollected = true)
    }

  private[rspace] def putK(txn: T,
                           channels: List[C],
                           continuation: WaitingContinuation[P, K]): Unit = {
    val keyCs = putCsH(txn, channels)
    val binPsKs =
      PsKsBytes()
        .withKvalue(toByteString(continuation.continuation))
        .withPatterns(toBytesList(continuation.patterns))
        .withPersist(continuation.persist)
    val psksLst = readPsKsBytesList(txn, keyCs).getOrElse(List.empty[PsKsBytes])
    writePsKsBytesList(txn, keyCs, binPsKs +: psksLst)
  }

  private[rspace] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]] = {
    val keyCs = hashCs(curr)
    readPsKsBytesList(txn, keyCs)
      .flatMap { (psKsByteses: List[PsKsBytes]) =>
        psKsByteses
          .map { (psks: PsKsBytes) =>
            psks.patterns.map { (bl: BytesList) =>
              WaitingContinuation(fromBytesList[P](bl),
                                  fromByteString[K](psks.kvalue),
                                  psks.persist)
            }
          }
          .sequence[Option, WaitingContinuation[P, K]]
      }
      .getOrElse(List.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readPsKsBytesList(txn, keyCs) match {
      case Some(psks) => writePsKsBytesList(txn, keyCs, util.dropIndex(psks, index))
      case None       => throw new IllegalArgumentException(s"removePsK: no values at $channels")
    }
  }

  private[rspace] def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val oldCsList =
      Option(_dbJoins.get(txn, joinKey))
        .map(fromByteBuffer[BytesList])
        .getOrElse(List.empty[BytesList])

    val addBl = toBytesList(cs)
    if (!oldCsList.contains(addBl)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(addBl :: oldCsList))
    }
  }

  private[rspace] def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey = hashCs(List(c))
    Option(_dbJoins.get(txn, joinKey))
      .map(fromByteBuffer[BytesList])
      .map(_.map(fromBytesList[C]))
      .getOrElse(List.empty[List[C]])
  }

  private[rspace] def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val exList =
      Option(_dbJoins.get(txn, joinKey))
        .map(fromByteBuffer[BytesList])
        .map(_.map(fromBytesList[C]))
        .getOrElse(List.empty[List[C]])
    val idx = exList.indexOf(cs)
    if (idx >= 0) {
      val csKey = hashCs(cs)
      if (_dbPsKs.get(txn, csKey) == null) {
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

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    val joinKey = hashCs(List(c))
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

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    getPsK(txn, channels).map(_.patterns)

  def toMap: Map[List[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      val keyRange: KeyRange[ByteBuffer] = KeyRange.all()
      withResource(_dbKeys.iterate(txn, keyRange)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala.map { (x: CursorIterator.KeyVal[ByteBuffer]) =>
          val channels: List[C] = getKey(txn, x.`key`())
          val data              = getAs(txn, channels)
          val wks               = getPsK(txn, channels)
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

    val bytesListInstance: Serialize[BytesList] = mkProtobufInstance(BytesList)

    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    val dbKeys: Dbi[ByteBuffer]  = env.openDbi(keysTableName, MDB_CREATE)
    val dbPsKs: Dbi[ByteBuffer]  = env.openDbi(psksTableName, MDB_CREATE)
    val dbAs: Dbi[ByteBuffer]    = env.openDbi(asTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBStore[C, P, A, K](env, dbKeys, dbPsKs, dbAs, dbJoins)(sc, sp, sa, sk, bytesListInstance)
  }

  private[rspace] def toByteString[T](value: T)(implicit st: Serialize[T]): ByteString =
    ByteString.copyFrom(st.encode(value))

  private[rspace] def fromByteString[T](bytes: ByteString)(implicit st: Serialize[T]): T =
    st.decode(bytes.toByteArray) match {
      case Left(err)    => throw new Exception(err)
      case Right(value) => value
    }

  private[rspace] def toBytesList[T](values: List[T])(implicit st: Serialize[T]): BytesList =
    BytesList().withValues(values.map(st.encode).map(ByteString.copyFrom))

  private[rspace] def fromBytesList[T](bl: BytesList)(implicit st: Serialize[T]): List[T] = {
    val x: Either[Throwable, List[T]] = bl.values
      .map(x => st.decode(x.toByteArray))
      .toList
      .sequence[Either[Throwable, ?], T]
    x match {
      case Left(err)     => throw new Exception(err)
      case Right(values) => values
    }
  }

  private[rspace] def toByteBuffer[T](values: List[T])(implicit st: Serialize[T]): ByteBuffer = {
    val bl             = toBytesList(values).toByteArray
    val bb: ByteBuffer = ByteBuffer.allocateDirect(bl.length)
    bb.put(bl).flip()
    bb
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(
      implicit st: Serialize[T]): List[T] = {
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    val bl = BytesList.parseFrom(fetched)
    fromBytesList[T](bl)
  }

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
