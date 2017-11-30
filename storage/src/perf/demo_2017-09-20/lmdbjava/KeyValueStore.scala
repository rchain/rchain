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

//  val mapSize = largestKey * keyValuesCount * 2 * mdb_valSize
val mapSize:Long = 17592186040320L

  val env = create()
              .setMaxDbs(1)
              .setMapSize(mapSize)
              .open(path, MDB_WRITEMAP)
  val db = env.openDbi("lmdbtest1", MDB_CREATE, MDB_DUPSORT)
  val txn = env.txnWrite()
  var cursor = db.openCursor(txn)

  def add(key: Int, value: Int): Unit =
  {
    cursor.put(KvsTools.intToBb(key), KvsTools.intToBb(value))
  }

  def get(key: Int): Array[Int] =
  {
    val valuesArray = new ArrayBuffer[Int]()

    var outcome = cursor.get(KvsTools.intToBb(key), MDB_SET_KEY)
    if (!outcome) return Array[Int]()

    if (!outcome) return Array[Int]()

    while (outcome)
    {
      val n = cursor.`val`.getInt
      valuesArray += n
      outcome = cursor.seek(MDB_NEXT_DUP)
    }
    
    valuesArray.toArray
  }

  def close(): Unit =
  {
    cursor.close()
    txn.close()
    // db.close()
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
