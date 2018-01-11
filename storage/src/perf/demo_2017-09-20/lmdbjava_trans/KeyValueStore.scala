package KeyValueStore

import scala.collection.mutable._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.lang.Integer.BYTES
import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.Charset

import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.SeekOp._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.DbiFlags.MDB_DUPSORT
import org.lmdbjava.Env.create


// KeyValueStore maps Keys as Strings to ValueLists

class KeyValueStore(largestKey: Int, keyValuesCount: Int)
{
  val mdb_valSize = 16

  val charset = Charset.forName("UTF-8")
  val encoder = charset.newEncoder()
  val decoder = charset.newDecoder()

  val currentDirectory = System.getProperty("user.dir")
  val testDirectory = currentDirectory + "/" + "lmdbtest"
  val path = new File(testDirectory)
  KvsTools.deleteFile(path + "/" + "data.mdb")
  KvsTools.deleteFile(path + "/" + "lock.mdb")
  if (!path.exists) {
    val successful = path.mkdir()
    assert(successful)
  }
  // these commands don't seem to work
  path.setReadable(true, true)
  path.setWritable(true, true)

  val mapSize = largestKey * keyValuesCount * 3 * mdb_valSize

  val env = create()
              .setMaxDbs(1)
              .setMapSize(mapSize)
              .open(path, MDB_WRITEMAP)
  val db = env.openDbi("lmdbtest1", MDB_CREATE, MDB_DUPSORT)
  var txn:org.lmdbjava.Txn[ByteBuffer] = null
  var cursor:org.lmdbjava.Cursor[ByteBuffer] = null

  def add(key: Int, value: Int): Unit =
  {
    txn = env.txnWrite()
    cursor = db.openCursor(txn)

    cursor.put(KvsTools.intToBb(key), KvsTools.intToBb(value))

    cursor.close()
    txn.commit()
  }

  def get(key: Int): Array[Int] =
  {
    val ab = new ArrayBuffer[Int]()

    txn = env.txnRead()
    cursor = db.openCursor(txn)

    var outcome = cursor.get(KvsTools.intToBb(key), MDB_SET_KEY)
    if (!outcome)
    {
      cursor.close()
      txn.close()
      return Array[Int]()
    }

    outcome = cursor.seek(MDB_FIRST_DUP)
    if (!outcome)
    {
      cursor.close()
      txn.close()
      return Array[Int]()
    }

    while (outcome)
    {
      val n = cursor.`val`.getInt
      ab += n
      outcome = cursor.seek(MDB_NEXT_DUP)
    }

    cursor.close()
    txn.close()

    ab.toArray
  }

  def close(): Unit =
  {
    db.close()
    env.close
  }
}

object KvsTools
{
  def intToBb(value: Int): ByteBuffer =
  {
    val bb = ByteBuffer.allocateDirect(BYTES)
    bb.putInt(value).flip
    bb
  }

  def deleteFile(path: String) =
  {
    val fileTemp = new File(path)
    if (fileTemp.exists)
      fileTemp.delete()
  }
}
