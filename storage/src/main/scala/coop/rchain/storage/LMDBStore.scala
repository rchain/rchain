package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.file.Path
import java.security.MessageDigest

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, Txn}
import coop.rchain.models.Serialize
import coop.rchain.models.implicits.rhoInstanceWrapper
import coop.rchain.storage.LMDBStore.{fromBB, fromBL, fromBS, hashBytes, toBB, toBL, toBS}
import coop.rchain.storage.datamodels.{BytesList, PsKsBytes, PsKsBytesList}

trait IClearableStore {
  /**
    *  clears database, used mostly to speedup unit-tests, and implemented by LMDBStore only
    */
  def clear(): Unit
}

class LMDBStore[C, P, A, K] private (env: Env[ByteBuffer],
                                     _dbKeys: Dbi[ByteBuffer],
                                     _dbPsKs: Dbi[ByteBuffer],
                                     _dbAs: Dbi[ByteBuffer],
                                     _dbJoins: Dbi[ByteBuffer])(implicit sc: Serialize[C],
                                                                pc: Serialize[P],
                                                                ac: Serialize[A],
                                                                kc: Serialize[K],
                                                                bl: Serialize[BytesList])
    extends IStore[C, P, A, K]
    with IClearableStore {

  type H = ByteBuffer

  private[storage] def hashC(packedCs: H): H =
    hashBytes(packedCs)

  private[storage] def hashC(c: C)(implicit serialize: Serialize[C]): H =
    hashC(List(c))(serialize)

  private[storage] def hashC(cs: List[C])(implicit serialize: Serialize[C]): H =
    hashC(toBB(cs)(serialize))

  private[storage] def getKey(txn: T, s: H): List[C] =
    fromBB[C](Option(_dbKeys.get(txn, s))).getOrElse(List.empty)

  private[storage] def putCs(txn: T, channels: List[C]): Unit =
    putCsH(txn, channels)

  private[storage] def putCsH(txn: T, channels: List[C]): H = {
    val packedCs = toBB(channels)
    val hashCs   = hashC(packedCs)
    _dbKeys.put(txn, hashCs, packedCs)
    hashCs
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
    val hashCs = putCsH(txn, channels)
    val as     = fromBB[A](Option(_dbAs.get(txn, hashCs))).getOrElse(List.empty[A])
    _dbAs.put(txn, hashCs, toBB(a :: as))
  }

  def getAs(txn: T, channels: List[C]): List[A] = {
    val hashCs = hashC(channels)
    fromBB[A](Option(_dbAs.get(txn, hashCs))).getOrElse(List.empty[A])
  }

  def removeA(txn: T, channel: C, index: Int): Unit = {
    val hashCs = hashC(List(channel))
    //shouldn't we need to throw exception if as doesn't exists?
    for (as <- fromBB[A](Option(_dbAs.get(txn, hashCs)))) {
      val newAs = util.dropIndex(as, index)
      _dbAs.put(txn, hashCs, toBB(newAs))
    }
  }

  private[this] def readPsKsBytesList(txn: T, hashCs: H): Option[List[PsKsBytes]] =
    Option(_dbPsKs.get(txn, hashCs)).map(bytes => {
      val fetched = new Array[Byte](bytes.remaining())
      ignore { bytes.get(fetched) }
      PsKsBytesList.parseFrom(fetched).values.toList
    })

  private[this] def writePsKsBytesList(txn: T, hashCs: H, values: List[PsKsBytes]): Unit = {
    val toWrite        = PsKsBytesList().withValues(values).toByteArray
    val bb: ByteBuffer = ByteBuffer.allocateDirect(toWrite.length)
    bb.put(toWrite).flip()
    _dbPsKs.put(txn, hashCs, bb)
  }

  def putK(txn: T, channels: List[C], patterns: List[P], k: K): Unit = {
    val hashCs  = putCsH(txn, channels)
    val binPsKs = PsKsBytes().withKvalue(toBS(k)).withPatterns(toBL(patterns))
    val psksLst = readPsKsBytesList(txn, hashCs).getOrElse(List.empty[PsKsBytes])
    writePsKsBytesList(txn, hashCs, binPsKs +: psksLst)
  }

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    getPsK(txn, channels).map(_._1)

  def getPsK(txn: T, curr: List[C]): List[(List[P], K)] = {
    val hashCs = hashC(curr)
    readPsKsBytesList(txn, hashCs)
      .map(_.map(psks => {
        val patterns =
          psks.patterns.map(patternBytes => fromBL[P](patternBytes)).getOrElse(List.empty[P])
        (patterns, fromBS[K](psks.kvalue))
      }))
      .getOrElse(List.empty[(List[P], K)])
  }

  def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val hashCs = hashC(channels)
    //shouldn't we need to throw exception if psks doesn't exists?
    for (psks <- readPsKsBytesList(txn, hashCs)) {
      val resValues = util.dropIndex(psks, index)
      writePsKsBytesList(txn, hashCs, resValues)
    }
  }

  def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashC(c)
    val oldCsList =
      fromBB[BytesList](Option(_dbJoins.get(txn, joinKey))).getOrElse(List.empty)
    _dbJoins.put(txn, joinKey, toBB(toBL(cs) :: oldCsList))
  }

  def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey   = hashC(c)
    val oldCsList = fromBB[BytesList](Option(_dbJoins.get(txn, joinKey)))
    oldCsList.getOrElse(Nil).flatMap(x => fromBB[C](Some(ByteBuffer.wrap(x.toByteArray))))
  }

  def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashC(c)
    val exList =
      fromBB[BytesList](Option(_dbJoins.get(txn, joinKey)))
        .getOrElse(List.empty[BytesList])
        .flatMap(x => fromBB[C](Some(ByteBuffer.wrap(x.toByteArray))))
    val idx = exList.indexOf(cs)
    //shouldn't we need to throw exception if cs not found?
    if (idx >= 0) {
      val resList = dropIndex(exList, idx)
      if (resList.nonEmpty)
        _dbJoins.put(txn, joinKey, toBB(resList.map(toBL(_))))
      else
        _dbJoins.delete(txn, joinKey)
    }
  }

  def removeAllJoins(txn: T, c: C): Unit =
    _dbJoins.delete(txn, hashC(c))

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
    * @param path Path to the database files
    * @param mapSize Maximum size of the database, in bytes
    */
  def create[C, P, A, K](path: Path, mapSize: Long)(implicit sc: Serialize[C],
                                                    pc: Serialize[P],
                                                    ac: Serialize[A],
                                                    kc: Serialize[K]): IStore[C, P, A, K] = {
    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    val dbKeys: Dbi[ByteBuffer]  = env.openDbi(keysTableName, MDB_CREATE)
    val dbPsKs: Dbi[ByteBuffer]  = env.openDbi(psksTableName, MDB_CREATE)
    val dbAs: Dbi[ByteBuffer]    = env.openDbi(asTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBStore[C, P, A, K](env, dbKeys, dbPsKs, dbAs, dbJoins)
  }

  private[storage] def toBS[TItem](value: TItem)(
      implicit serialize: Serialize[TItem]): ByteString = {
    val encoded = serialize.encode(value)
    ByteString.copyFrom(encoded)
  }

  private[storage] def fromBS[TItem](bytes: ByteString)(
      implicit serialize: Serialize[TItem]): TItem =
    serialize.decode(bytes.toByteArray) match {
      case Left(err)    => throw new Exception(err)
      case Right(value) => value
    }

  private[storage] def toBL[TItem](values: List[TItem])(
      implicit serialize: Serialize[TItem]): BytesList = {
    val encoded = values.map(serialize.encode)
    BytesList().withValues(encoded.map(ByteString.copyFrom))
  }

  private[storage] def fromBL[TItem](bl: BytesList)(
      implicit serialize: Serialize[TItem]): List[TItem] = {
    val x: Either[Throwable, List[TItem]] = bl.values
      .map(x => serialize.decode(x.toByteArray))
      .toList
      .sequence[Either[Throwable, ?], TItem]
    x match {
      case Left(err)     => throw new Exception(err)
      case Right(values) => values
    }
  }

  private[storage] def toBB[TItem](values: List[TItem])(
      implicit serialize: Serialize[TItem]): ByteBuffer = {
    val bl             = toBL(values).toByteArray
    val bb: ByteBuffer = ByteBuffer.allocateDirect(bl.length)
    bb.put(bl).flip()
    bb
  }

  private[storage] def fromBB[TItem](bytesOpt: Option[ByteBuffer])(
      implicit serialize: Serialize[TItem]): Option[List[TItem]] =
    bytesOpt.map(bytes => {
      val fetched = new Array[Byte](bytes.remaining())
      ignore { bytes.get(fetched) }
      val bl = BytesList.parseFrom(fetched)
      fromBL[TItem](bl)
    })

  def hashBytes(byteBuffer: ByteBuffer): ByteBuffer = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore { byteBuffer.get(fetched) }
    byteBuffer.reset()
    hashBytes(fetched)
  }

  def hashBytes(bs: Array[Byte]): ByteBuffer = {
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(bs)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }
}
