package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.file.Path

import cats.syntax.either._
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, Txn}

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
}
