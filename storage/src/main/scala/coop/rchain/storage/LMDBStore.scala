package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.file.Path
import java.security.MessageDigest

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.models.Serialize
import coop.rchain.models.implicits.rhoInstanceWrapper
import coop.rchain.storage.datamodels.{BytesList, PsKsBytes, PsKsBytesList}
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, Txn}

class LMDBStore[C, P, A, K] private (env: Env[ByteBuffer],
                                     _dbKeys: Dbi[ByteBuffer],
                                     _dbPsKs: Dbi[ByteBuffer],
                                     _dbAs: Dbi[ByteBuffer],
                                     _dbJoins: Dbi[ByteBuffer])(implicit sc: Serialize[C],
                                                                pc: Serialize[P],
                                                                ac: Serialize[A],
                                                                kc: Serialize[K],
                                                                bl: Serialize[BytesList])
    extends IStore[C, P, A, K] {

  import coop.rchain.storage.LMDBStore._

  type H = ByteBuffer

  private[storage] def hashCs(cs: List[C])(implicit st: Serialize[C]): H =
    hashBytes(toByteBuffer(cs)(st))

  private[storage] def getKey(txn: T, s: H): List[C] =
    Option(_dbKeys.get(txn, s)).map(fromByteBuffer[C]).getOrElse(List.empty[C])

  private[storage] def putCs(txn: T, channels: List[C]): Unit =
    putCsH(txn, channels)

  private[storage] def putCsH(txn: T, channels: List[C]): H = {
    val packedCs = toByteBuffer(channels)
    val keyCs    = hashBytes(packedCs)
    _dbKeys.put(txn, keyCs, packedCs)
    keyCs
  }

  type T = Txn[ByteBuffer]

  def createTxnRead(): T = env.txnWrite

  def createTxnWrite(): T = env.txnWrite

  def withTxn[R](txn: T)(f: T => R): R =
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

  def putA(txn: T, channels: List[C], a: A): Unit = {
    val keyCs = putCsH(txn, channels)
    val as    = Option(_dbAs.get(txn, keyCs)).map(fromByteBuffer[A]).getOrElse(List.empty[A])
    _dbAs.put(txn, keyCs, toByteBuffer(a :: as))
  }

  def getAs(txn: T, channels: List[C]): List[A] = {
    val keyCs = hashCs(channels)
    Option(_dbAs.get(txn, keyCs)).map(fromByteBuffer[A]).getOrElse(List.empty[A])
  }

  def removeA(txn: T, channel: C, index: Int): Unit = {
    val keyCs = hashCs(List(channel))
    Option(_dbAs.get(txn, keyCs)).map(fromByteBuffer[A]) match {
      case Some(as) => _dbAs.put(txn, keyCs, toByteBuffer(util.dropIndex(as, index)))
      case None     => throw new IllegalArgumentException(s"removeA: no values at $channel")
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

  private[this] def writePsKsBytesList(txn: T, keyCs: H, values: List[PsKsBytes]): Unit = {
    val toWrite        = PsKsBytesList().withValues(values).toByteArray
    val bb: ByteBuffer = ByteBuffer.allocateDirect(toWrite.length)
    bb.put(toWrite).flip()
    _dbPsKs.put(txn, keyCs, bb)
  }

  def putK(txn: T, channels: List[C], patterns: List[P], k: K): Unit = {
    val keyCs   = putCsH(txn, channels)
    val binPsKs = PsKsBytes().withKvalue(toByteString(k)).withPatterns(toBytesList(patterns))
    val psksLst = readPsKsBytesList(txn, keyCs).getOrElse(List.empty[PsKsBytes])
    writePsKsBytesList(txn, keyCs, binPsKs +: psksLst)
  }

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    getPsK(txn, channels).map(_._1)

  def getPsK(txn: T, curr: List[C]): List[(List[P], K)] = {
    val keyCs = hashCs(curr)
    readPsKsBytesList(txn, keyCs)
      .flatMap { (psKsByteses: List[PsKsBytes]) =>
        psKsByteses
          .map { (psks: PsKsBytes) =>
            psks.patterns
              .map((bl: BytesList) => fromBytesList[P](bl))
              .map((ps: List[P]) => (ps, fromByteString[K](psks.kvalue)))
          }
          .sequence[Option, (List[P], K)]
      }
      .getOrElse(List.empty[(List[P], K)])
  }

  def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readPsKsBytesList(txn, keyCs) match {
      case Some(psks) => writePsKsBytesList(txn, keyCs, util.dropIndex(psks, index))
      case None       => throw new IllegalArgumentException(s"removePsK: no values at $channels")
    }
  }

  def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val oldCsList =
      Option(_dbJoins.get(txn, joinKey))
        .map(fromByteBuffer[BytesList])
        .getOrElse(List.empty[BytesList])
    _dbJoins.put(txn, joinKey, toByteBuffer(toBytesList(cs) :: oldCsList))
  }

  def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey = hashCs(List(c))
    Option(_dbJoins.get(txn, joinKey))
      .map(fromByteBuffer[BytesList])
      .map(_.map(fromBytesList[C]))
      .getOrElse(List.empty[List[C]])
  }

  def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val exList =
      Option(_dbJoins.get(txn, joinKey))
        .map(fromByteBuffer[BytesList])
        .map(_.map(fromBytesList[C]))
        .getOrElse(List.empty[List[C]])
    val idx = exList.indexOf(cs)
    if (idx >= 0) {
      val resList = dropIndex(exList, idx)
      if (resList.nonEmpty)
        _dbJoins.put(txn, joinKey, toByteBuffer(resList.map(toBytesList(_))))
      else
        _dbJoins.delete(txn, joinKey)
    } else {
      throw new IllegalArgumentException(s"removeJoin: $cs is not a member of $exList")
    }
  }

  def removeAllJoins(txn: T, c: C): Unit =
    _dbJoins.delete(txn, hashCs(List(c)))

  def close(): Unit = {
    _dbKeys.close()
    _dbAs.close()
    _dbPsKs.close()
    _dbJoins.close()
    env.close()
  }

  def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbAs.drop(txn)
      _dbPsKs.drop(txn)
      _dbJoins.drop(txn)
    }
}

object LMDBStore {
  private[this] val keysTableName: String  = "Keys"
  private[this] val psksTableName: String  = "PsKs"
  private[this] val asTableName: String    = "As"
  private[this] val joinsTableName: String = "Joins"

  implicit object bytesListInstance extends rhoInstanceWrapper(BytesList)

  /**
    * Creates an instance of [[IStore]]
    *
    * @param path    Path to the database files
    * @param mapSize Maximum size of the database, in bytes
    */
  def create[C, P, A, K](path: Path, mapSize: Long)(implicit sc: Serialize[C],
                                                    pc: Serialize[P],
                                                    ac: Serialize[A],
                                                    kc: Serialize[K]): LMDBStore[C, P, A, K] = {
    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    val dbKeys: Dbi[ByteBuffer]  = env.openDbi(keysTableName, MDB_CREATE)
    val dbPsKs: Dbi[ByteBuffer]  = env.openDbi(psksTableName, MDB_CREATE)
    val dbAs: Dbi[ByteBuffer]    = env.openDbi(asTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBStore[C, P, A, K](env, dbKeys, dbPsKs, dbAs, dbJoins)
  }

  private[storage] def toByteString[T](value: T)(implicit st: Serialize[T]): ByteString =
    ByteString.copyFrom(st.encode(value))

  private[storage] def fromByteString[T](bytes: ByteString)(implicit st: Serialize[T]): T =
    st.decode(bytes.toByteArray) match {
      case Left(err)    => throw new Exception(err)
      case Right(value) => value
    }

  private[storage] def toBytesList[T](values: List[T])(implicit st: Serialize[T]): BytesList =
    BytesList().withValues(values.map(st.encode).map(ByteString.copyFrom))

  private[storage] def fromBytesList[T](bl: BytesList)(implicit st: Serialize[T]): List[T] = {
    val x: Either[Throwable, List[T]] = bl.values
      .map(x => st.decode(x.toByteArray))
      .toList
      .sequence[Either[Throwable, ?], T]
    x match {
      case Left(err)     => throw new Exception(err)
      case Right(values) => values
    }
  }

  private[storage] def toByteBuffer[T](values: List[T])(implicit st: Serialize[T]): ByteBuffer = {
    val bl             = toBytesList(values).toByteArray
    val bb: ByteBuffer = ByteBuffer.allocateDirect(bl.length)
    bb.put(bl).flip()
    bb
  }

  private[storage] def fromByteBuffer[T](byteBuffer: ByteBuffer)(
      implicit st: Serialize[T]): List[T] = {
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    val bl = BytesList.parseFrom(fetched)
    fromBytesList[T](bl)
  }

  def hashBytes(byteBuffer: ByteBuffer): ByteBuffer = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    byteBuffer.reset()
    hashBytes(fetched)
  }

  def hashBytes(bytes: Array[Byte]): ByteBuffer = {
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(bytes)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }
}
