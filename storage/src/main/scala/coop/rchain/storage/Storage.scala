package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.file.Path

import cats.syntax.either._
import coop.rchain.storage.util._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, Txn}

class Storage private (env: Env[ByteBuffer], dbs: Map[String, Dbi[ByteBuffer]], rootName: String)
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
        Storage.getDbi(dbs, rootName, key).put(keyBuff, valBuff)
      }
      .leftMap(StorageError.apply)

  def get[A](key: Key)(implicit s: Serialize[A]): Either[Error, A] =
    Either
      .catchNonFatal {
        val keyBuff = ByteBuffer.allocateDirect(env.getMaxKeySize)
        ignore { keyBuff.put(key.bytes).flip() }
        withResource(env.txnRead()) { (txn: Txn[ByteBuffer]) =>
          if (Storage.getDbi(dbs, rootName, key).get(txn, keyBuff) != null) {
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
        Storage.getDbi(dbs, rootName, key).delete(keyBuff)
      }
      .leftMap(StorageError.apply)

  def close(): Unit = {
    dbs.foreach {
      case (_: String, value: Dbi[ByteBuffer]) => value.close()
    }
    env.close()
  }
}

object Storage {

  private def createBlocksName(rootName: String): String =
    s"$rootName-blocks"

  private def createContractsName(rootName: String): String =
    s"$rootName-contracts"

  private def createSystemContractsName(rootName: String): String =
    s"$rootName-systemContracts"

  protected def createDbis[A](env: Env[A], rootName: String): Map[String, Dbi[A]] = {
    val blocks          = createBlocksName(rootName)
    val contracts       = createContractsName(rootName)
    val systemContracts = createSystemContractsName(rootName)
    Map(
      blocks          -> env.openDbi(blocks, MDB_CREATE),
      contracts       -> env.openDbi(contracts, MDB_CREATE),
      systemContracts -> env.openDbi(systemContracts, MDB_CREATE)
    )
  }

  protected def getDbi(dbs: Map[String, Dbi[ByteBuffer]],
                       rootName: String,
                       key: Key): Dbi[ByteBuffer] =
    key match {
      case Hash(_)    => dbs(createBlocksName(rootName))
      case Flat(_)    => dbs(createContractsName(rootName))
      case FlatSys(_) => dbs(createSystemContractsName(rootName))
    }

  /**
    * Creates an instance of [[Storage]]
    *
    * @param path Path to the database files
    * @param name Name of the database
    * @param mapSize Maximum size of the database, in bytes
    */
  def create(path: Path, name: String, mapSize: Long): Storage = {
    val env: Env[ByteBuffer]              = Env.create().setMapSize(mapSize).setMaxDbs(3).open(path.toFile)
    val dbs: Map[String, Dbi[ByteBuffer]] = createDbis(env, name)
    new Storage(env, dbs, name)
  }
}
