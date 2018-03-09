package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest

import cats.implicits._
import com.google.protobuf.{ByteString, MessageLite}
import cats.syntax.either._
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, EnvFlags, Txn}
import coop.rchain.models.Serialize
import coop.rchain.models.implicits.rhoInstanceWrapper
import coop.rchain.storage.LMDBStore.{fromBB, hashBytes, toBB, toBL, toBS}
import coop.rchain.storage.datamodels.{BytesList, PsKsBytes}

class LMDBStore[C, P, A, K] private (env: Env[ByteBuffer],
                                     _dbKeys: Dbi[ByteBuffer],
                                     _dbPsKs: Dbi[ByteBuffer],
                                     _dbAs: Dbi[ByteBuffer],
                                     _dbJoins: Dbi[ByteBuffer])(implicit sc: Serialize[C],
                                                                pc: Serialize[P],
                                                                ac: Serialize[A],
                                                                kc: Serialize[K])
    extends IStore[C, P, A, K] {

  implicit object bytesListInstance extends rhoInstanceWrapper(BytesList)

  type H = ByteBuffer

  private[storage] def hashC(packedCs: H): H =
    hashBytes(packedCs)

  private[storage] def hashC(c: C)(implicit serialize: Serialize[C]): H =
    hashBytes(serialize.encode(c))

  private[storage] def hashC(cs: List[C])(implicit serialize: Serialize[C]): H =
    hashBytes(toBB(cs)(serialize))

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
    val as     = fromBB[A](Option(_dbAs.get(txn, hashCs))).getOrElse(List.empty)
    _dbAs.put(txn, hashCs, toBB(a :: as))
  }

  def getAs(txn: T, channels: List[C]): List[A] = {
    val hashCs = hashC(channels)
    fromBB[A](Option(_dbAs.get(txn, hashCs))).getOrElse(List.empty)
  }

  def removeA(txn: T, channels: List[C], index: Int): Unit = {
    val hashCs = hashC(channels)
    for (as <- fromBB[A](Option(_dbAs.get(txn, hashCs)))) {
      val newAs = util.dropIndex(as, index)
      _dbAs.put(txn, hashCs, toBB(newAs))
    }
  }

  private[this] def parsePsKsBytes(txn: T, hashCs : H) : List[PsKsBytes] = {
    val psKsBin = Option(_dbPsKs.get(txn, hashCs))

    psKsBin
  }

  def putK(txn: T, channels: List[C], patterns: List[P], k: K): Unit = {
    val hashCs = putCsH(txn, channels)



    exPsKs.map(bb => PsKsBytes.parseFrom(bb.array()))


    val binPsKs = PsKsBytes().withKvalue(toBS(k)).withPatterns(toBL(patterns))


      fromBB[P]().getOrElse(List.empty[(List[P], K)])
//    val ps     = fromBB[P](Option(_dbPsKs.get(txn, hashCs))).getOrElse(List.empty[(List[P], K)])
//    _dbPsKs.put(txn, hashCs, toBB(patterns ++ ps))
  }

  def getPsK(txn: T, curr: List[C]): List[(List[P], K)] = {
    //    val hashCs = hashC(curr)
    //    for {
    //      ps <- fromBB[P](Option(_dbPs.get(txn, hashCs)))
    //      k  <- fromBB[K](Option(_dbK.get(txn, hashCs))).flatMap(_.headOption)
    //    } yield (ps, k)
    throw new NotImplementedError("TODO")
  }

  def getPs(txn: T, channels: List[C]): List[List[P]] = {
    val hashCs = hashC(channels)


    //    fromBB[P](Option(_dbPsKs.get(txn, hashCs))).getOrElse(List.empty)
  }

  def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val hashCs = hashC(channels)
    for (psks <- fromBB[P](Option(_dbPsKs.get(txn, hashCs)))) {
      val newPs = util.dropIndex(psks, index)
      _dbPsKs.put(txn, hashCs, toBB(newPs))
    }
  }

  def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashC(c)
    val csKey   = toBL(cs)
    val oldCsList =
      fromBB[BytesList](Option(_dbJoins.get(txn, joinKey))).getOrElse(List.empty)
    _dbJoins.put(txn, joinKey, toBB(csKey :: oldCsList))
  }

  def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey   = hashC(c)
    val oldCsList = fromBB[BytesList](Option(_dbJoins.get(txn, joinKey)))
    oldCsList.getOrElse(Nil).flatMap(x => fromBB[C](Some(ByteBuffer.wrap(x.toByteArray))))
  }

  def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey   = hashC(c)
    val oldCsList = fromBB[BytesList](Option(_dbJoins.get(txn, joinKey)))
    val exList =
      oldCsList.getOrElse(Nil).flatMap(x => fromBB[C](Some(ByteBuffer.wrap(x.toByteArray))))
    val idx = exList.indexOf(cs)
    if (idx >= 0) {
      val resList = dropIndex(exList, idx)
      if (resList.nonEmpty)
        _dbJoins.put(txn, joinKey, toBB(resList.map(toBL(_))))
      else
        _dbJoins.delete(txn, joinKey)
    }
  }

  def removeAllJoins(txn: T, c: C): Unit = {
    val joinKey = hashC(c)
    _dbJoins.delete(txn, joinKey)
  }

  def close(): Unit = {
    _dbKeys.close()
    _dbAs.close()
    _dbPsKs.close()
    _dbJoins.close()
  }
}

object LMDBStore {
  def hashBytes(bs: ByteBuffer): ByteBuffer = {
    val fetched = new Array[Byte](bs.remaining())
    ignore { bs.get(fetched) }
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(fetched)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }

  def hashBytes(bs: Array[Byte]): ByteBuffer = {
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(bs)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }

  def hashString(s: String): ByteBuffer =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

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

    val dbKeys: Dbi[ByteBuffer]  = env.openDbi("Keys", MDB_CREATE)
    val dbPsKs: Dbi[ByteBuffer]  = env.openDbi("PsKs", MDB_CREATE)
    val dbAs: Dbi[ByteBuffer]    = env.openDbi("As", MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi("Joins", MDB_CREATE)
    new LMDBStore[C, P, A, K](env, dbKeys, dbPsKs, dbAs, dbJoins)
  }

  private[storage] def toBS[TItem](value: TItem)(
    implicit serialize: Serialize[TItem]): ByteString = {
    val encoded = serialize.encode(value)
    ByteString.copyFrom(encoded)
  }

  private[storage] def toBL[TItem](values: List[TItem])(
      implicit serialize: Serialize[TItem]): BytesList = {
    val encoded = values.map(serialize.encode)
    BytesList().withValues(encoded.map(ByteString.copyFrom))
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
      val x: Either[Throwable, List[TItem]] = bl.values
        .map(x => serialize.decode(x.toByteArray))
        .toList
        .sequence[Either[Throwable, ?], TItem]
      x match {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }
    })
}

/*
class Storage private (env: Env[ByteBuffer], db: Dbi[ByteBuffer])
    extends IStorage
    with AutoCloseable {

  def put[A](key: Key, value: A)(implicit s: Serialize[A]): Either[Error, Unit] =
    Either
      .catchNonFatal {
        val encoded = s.encode(value)
        val keyBuff = ByteBuffer.allocateDirect(env.getMaxKeySize)
        val valBuff = ByteBuffer.allocateDirect(encoded.length)
        ignore { keyBuff.put(key.bytes).flip() }
        ignore { valBuff.put(encoded).flip() }
        db.put(keyBuff, valBuff)
      }
      .leftMap(StorageError.apply)

  def get[A](key: Key)(implicit s: Serialize[A]): Either[Error, A] =
    Either
      .catchNonFatal {
        val keyBuff = ByteBuffer.allocateDirect(env.getMaxKeySize)
        ignore { keyBuff.put(key.bytes).flip() }
        withResource(env.txnRead()) { (txn: Txn[ByteBuffer]) =>
          if (db.get(txn, keyBuff) != null) {
            val fetchedBuff = txn.`val`()
            val fetched     = new Array[Byte](fetchedBuff.remaining())
            ignore { fetchedBuff.get(fetched) }
            s.decode(fetched)
          } else {
            Left[Error, A](NotFound)
          }
        }
      }
      .leftMap(StorageError.apply)
      .joinRight

  def remove(key: Key): Either[Error, Boolean] =
    Either
      .catchNonFatal {
        val keyBuff = ByteBuffer.allocateDirect(env.getMaxKeySize)
        ignore { keyBuff.put(key.bytes).flip() }
        db.delete(keyBuff)
      }
      .leftMap(StorageError.apply)

  def close(): Unit = {
    db.close()
    env.close()
  }
}

object Storage {

  /**
 * Creates an instance of [[Storage]]
 *
 * @param path Path to the database files
 * @param name Name of the database
 * @param mapSize Maximum size of the database, in bytes
 */
  def create(path: Path, name: String, mapSize: Long): Storage = {
    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(1).open(path.toFile)
    val db: Dbi[ByteBuffer] = env.openDbi(name, MDB_CREATE)
    new Storage(env, db)
  }

 */
