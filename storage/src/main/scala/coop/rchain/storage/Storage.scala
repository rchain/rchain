package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.file.Path

import cats.syntax.either._
import coop.rchain.models.{Error, NotFound, Serialize, StorageError}
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, Txn}

class Storage[K, V] private (env: Env[ByteBuffer], db: Dbi[ByteBuffer])(implicit sa: Serialize[K],
                                                                        sb: Serialize[V])
    extends IStorage[K, V]
    with AutoCloseable {

  def put(key: K, value: V): Either[Error, Unit] =
    Either
      .catchNonFatal {
        val encodedKey   = sa.encode(key)
        val encodedValue = sb.encode(value)
        val keyBuff      = ByteBuffer.allocateDirect(env.getMaxKeySize)
        val valBuff      = ByteBuffer.allocateDirect(encodedValue.length)
        ignore { keyBuff.put(encodedKey).flip() }
        ignore { valBuff.put(encodedValue).flip() }
        db.put(keyBuff, valBuff)
      }
      .leftMap(StorageError.apply)

  def get(key: K): Either[Error, V] =
    Either
      .catchNonFatal[Either[Error, V]] {
        val encodedKey = sa.encode(key)
        val keyBuff    = ByteBuffer.allocateDirect(env.getMaxKeySize)
        ignore { keyBuff.put(encodedKey).flip() }
        withResource(env.txnRead()) { (txn: Txn[ByteBuffer]) =>
          if (db.get(txn, keyBuff) != null) {
            val fetchedBuff = txn.`val`()
            val fetched     = new Array[Byte](fetchedBuff.remaining())
            ignore { fetchedBuff.get(fetched) }
            sb.decode(fetched)
          } else {
            Left[Error, V](NotFound)
          }
        }
      }
      .leftMap(StorageError.apply)
      .joinRight

  def remove(key: K): Either[Error, Boolean] =
    Either
      .catchNonFatal {
        val encodedKey = sa.encode(key)
        val keyBuff    = ByteBuffer.allocateDirect(env.getMaxKeySize)
        ignore { keyBuff.put(encodedKey).flip() }
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
  def create[K, V](path: Path, name: String, mapSize: Long)(implicit sk: Serialize[K],
                                                            sv: Serialize[V]): Storage[K, V] = {
    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(1).open(path.toFile)
    val db: Dbi[ByteBuffer] = env.openDbi(name, MDB_CREATE)
    new Storage[K, V](env, db)
  }
}
