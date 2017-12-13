import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.lang.Integer.BYTES
import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.Charset
import java.util.Calendar

import BlobTest.{db, txn}
import org.lmdbjava._
import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.SeekOp._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.Env.create


object BlobTest
{
  val display = false

  val largestKey:Int = 10000

  val digits = 3

  val blobLength = (1048576*10)
  val zero = nines("", digits)
  val one = nines("1", digits)
  val two = nines("2", digits)
  val three = nines("3", digits)
  val four = nines("4", digits)
  val five = nines("5", digits)
  val six = nines("6", digits)
  val seven = nines("7", digits)
  val eight = nines("8", digits)
  val nine = nines("9", digits)


  val mapSize:Long = 17592186040320L

  val blobs = new Array[String](10)

  val fileName = "lmdbjava_blob_stat.txt"

  val usedKeys = new Array[Boolean](largestKey)
  var nextBlobKey:Int = largestKey + 1
  val mdb_val_size = 16 // sizeof(mdb_val)
  val rgen = scala.util.Random

  val dbDirName = "lmdbtest"
  val dbName = "lmdbtest1"

  val currentDirectory:String = System.getProperty("user.dir")
  val testDirectory:String = currentDirectory + "/" + dbDirName
  val path = new File(testDirectory)
  deleteFile(path + "/" + "data.mdb")
  deleteFile(path + "/" + "lock.mdb")
  if (!path.exists) {
    val successful = path.mkdir()
    assert(successful)
  }
  // these commands don't seem to work
  path.setReadable(true, true)
  path.setWritable(true, true)

  val env:Env[ByteBuffer] = create()
    .setMaxDbs(1)
    .setMapSize(mapSize)
    .open(path, MDB_WRITEMAP)
  val db:Dbi[ByteBuffer] = env.openDbi(dbName, MDB_CREATE)
  var txn:Txn[ByteBuffer] = env.txnWrite()
  var cursor:Cursor[ByteBuffer] = db.openCursor(txn)


  val charset:Charset = Charset.forName("UTF-8")
  val encoder = charset.newEncoder()
  val decoder = charset.newDecoder()


  def main(args: Array[String]): Unit =
  {
    // println(s"$zero, $one, $two, $three, $four, $five, $six, $seven, $eight, $nine\n")

    val lines = ArrayBuffer[String]()
    lines += "number of keys: " + largestKey
    lines += "values/key: 1"
    lines += "blob length: " + blobLength
    lines += "map size: " + mapSize
    Output(lines.toArray)

    JvmWarmUp()

    InitBlobs()
    ResetUsedKeys()

    println("\nBegin");
    DisplayEnvStat(env, db, txn);

    val startPut = MillisSinceEpoch()

    var putPrev = startPut

    for (keyCount <- 0 until largestKey)
    {
      val key = NextKey()
      if (display)
        println(s"put key: $key, blob: " + blobs(key % 10).substring(0,1))

      add(key, blobs(key % 10))

      if (keyCount == zero || keyCount == one || keyCount == two
        || keyCount == three || keyCount == four || keyCount == five
        || keyCount == six || keyCount == seven || keyCount == eight
        || keyCount == nine)
      {
        val putNow = MillisSinceEpoch()

        val lines = ArrayBuffer[String]()
        lines += "after put: " + keyCount + ": tree size: " + TreeSizeDb(db, txn) + ": time: " + (putNow - putPrev)
        Output(lines.toArray)

        // DisplayTreeInfo(env);

        putPrev = putNow
      }
    }
    if (display) println()

    val stopPut = MillisSinceEpoch()
Output(Array[String](""))

    ResetUsedKeys()

    println("After puts, before gets")
    DisplayEnvStat(env, db, txn)

    val startGet = MillisSinceEpoch()

    for (keyCount <- 0 until largestKey)
    {
      val key = NextKey()
      val value = get(key)
      assert(value.length == blobLength)
      if (display)
        println(s"get key: $key, blob: " + value.substring(0,1))
    }
    if (display) println()

    val stopGet = MillisSinceEpoch()
println()
    println("End")
    DisplayEnvStat(env, db, txn)

    Outcome(startPut, stopPut, startGet, stopGet)
  }


  def nines(digit:String, count:Int): Long =
  {
    val sb = new StringBuilder(digit)
    for (i <- 0 until count)
      sb.append("9")
    sb.toLong
  }

  def add(key: Int, blob:String): Unit =
  {
    val bbInt = intToBb(key)
    val bbStr = strToBb(blob)
    cursor.put(bbInt, bbStr)
  }

  def get(key: Int): String =
  {
    var outcome = cursor.get(intToBb(key), MDB_SET_KEY)
    assert(outcome)

    outcome = cursor.seek(MDB_GET_CURRENT)
    assert(outcome)

    bbToStr(cursor.`val`)
  }

/*
  def byteToBbArray(value: Byte, size: Int): ByteBuffer =
  {
    val bb = ByteBuffer.allocateDirect(size)
    for (i <- 0 until size)
      bb.put(i, value)
    bb.flip()
    bb
  }
*/

  def byteToBbArray2(value: Byte, size: Int): ByteBuffer = {
    val ba = new Array[Byte](size)
    for (i <- 0 until size)
      ba(i) = value

    val bb = ByteBuffer.wrap(ba) // not direct
    // flip necessary?
    bb.flip()
    bb
  }

  def longToBb(value: Long): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(2*BYTES)
    bb.putLong(value).flip
    bb
  }

  def intToBb(value: Int): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(BYTES)
    bb.putInt(value).flip
    bb
  }

  def bbToStr(buffer: ByteBuffer): String = {
    /*
    https://worldmodscode.wordpress.com/2012/12/14/the-java-bytebuffer-a-crash-course/
    suggests that the use of position() introduces a race condition a bug.
    Search for the third instance of "// NOT RECOMMENDED, don't do this"
    */
    val old_position = buffer.position
    val str = UTF_8.decode(buffer).toString()
    // reset buffer's position to its original so it is not altered:
    buffer.position(old_position)
    // my addition, doesn't appear to change anything
    buffer.flip
    str
  }

  def strToBb(str: String): ByteBuffer = {
    var bb = allocateDirect(str.length)
    bb.put(str.getBytes(UTF_8)).flip
    bb
  }


  def putStr(keyStr: String, keyLength: Int, valueStr: String, valueLength: Int,
             db: Dbi[ByteBuffer]): Unit = {
    putKeyValueStr(keyStr, valueStr, db, keyLength, valueLength)
  }

  def putKeyValueStr(keyStr: String, valueStr: String,
                     db: Dbi[ByteBuffer],
                     keySize: Int, valueSize: Int)
  : Unit = {
    println(s"putKeyValueStr('$keyStr', '$valueStr')")
    var key = strToBb(keyStr)
    var value = strToBb(valueStr)
    db.put(key, value)
  }


  def NextKey(): Int = {
    var r = rgen.nextInt(largestKey)
    while (usedKeys(r))
      r = rgen.nextInt(largestKey)
    usedKeys(r) = true
    r
  }

  def ResetUsedKeys(): Unit = {
    for (i <- 0 until largestKey)
      usedKeys(i) = false
  }

  def NextBlobKey(): Int =
  {
    assert(nextBlobKey <= mapSize)
    val nextKey = nextBlobKey
    nextBlobKey += 1
    nextKey
  }

  def MillisSinceEpoch(): Long = {
    Calendar.getInstance().getTimeInMillis()
  }

  def deleteFile(path: String): Unit =
  {
    val fileTemp = new File(path)
    if (fileTemp.exists)
      fileTemp.delete()
  }

  def InitBlobs(): Unit =
  {
    for (iStr <- 0 until 10)
    {
      val strBuf = new StringBuffer(blobLength)
      for (i <- 0 until blobLength)
        strBuf.append(iStr.toString)
      blobs(iStr) = strBuf.toString
    }
    /* works
    for (iStr <- 0 until 10)
    {
      val bbStr = strToBb(blobs(iStr))
      val str = bbToStr(bbStr)
      println(s"$iStr: $str")
    }
    */
  }

  def JvmWarmUp(): Unit =
  {
    val currentDirectory = System.getProperty("user.dir")
    val testDirectory = currentDirectory + "/" + dbDirName
    val path = new File(testDirectory)
    deleteFile(path + "/" + "data.mdb")
    deleteFile(path + "/" + "lock.mdb")
    if (!path.exists) {
      val successful = path.mkdir()
      assert(successful)
    }
    // these commands don't seem to work
    path.setReadable(true, true)
    path.setWritable(true, true)

    val env = create()
      .setMaxDbs(1)
      .open(path, MDB_WRITEMAP)
    val db = env.openDbi(dbName + "_warmup", MDB_CREATE)
    val txn = env.txnWrite()
    val cursor = db.openCursor(txn)

    for (key <- 0 until 100000)
      add(key, key.toString)

    for (key <- 0 until 100000)
      get(key)

    cursor.close()
    txn.abort()
    env.close()
  }

  def Outcome(startPut:Long, stopPut:Long, startGet:Long, stopGet:Long) : Unit =
  {
    val putDuration = stopPut - startPut
    val getDuration = stopGet - startGet

    val duration = putDuration + getDuration

    var putPerKey:Long = -1
    if (0 < putDuration) putPerKey = largestKey / putDuration
    var getPerKey:Long = -1
    if (0 < getDuration) getPerKey = largestKey / getDuration

    val lines = new ArrayBuffer[String]()
    lines += s"duration: $duration"
    lines += s"put: $putDuration"
    lines += s"put/ms: $putPerKey"
    lines += s"get: $getDuration"
    lines += s"gets/ms: $getPerKey"
    lines += ""

    Output(lines.toArray)
  }

  def Output(lines:Array[String]) : Unit =
  {
    for (line <- lines)
      println(line)
    FileTools.append(lines, fileName)
  }



  def TreeSizeEnv(env:Env[ByteBuffer]): Long =
  {
    val stat = env.stat()
    val size = 4096 * (stat.branchPages + stat.leafPages + stat.overflowPages)
    size
  }

  def TreeSizeDb(db:Dbi[ByteBuffer], txn:Txn[ByteBuffer]): Long =
  {
    val stat = db.stat(txn)
    val size = 4096 * (stat.branchPages + stat.leafPages + stat.overflowPages)
    size
  }

  def DisplayEnvStat(env:Env[ByteBuffer], db:Dbi[ByteBuffer], txn:Txn[ByteBuffer]):Unit =
  {
    if (env == null)
      return

    val lines = ArrayBuffer[String]()

    def envInfo = env.info()
    lines += "envinfo.me_mapsize: " + envInfo.mapSize
    lines += "envinfo.me_last_pgno: " + envInfo.lastPageNumber
    lines += "envinfo.me_last_txnid: " + envInfo.lastTransactionId
    lines += "envinfo.me_maxreaders: " + envInfo.maxReaders
    lines += "envinfo.me_numreaders: " + envInfo.numReaders

    val stat = env.stat()
    lines += "stat.ms_psize: " + stat.pageSize
    lines += "stat.ms_depth: " + stat.depth
    lines += "stat.ms_branch_pages: " + stat.branchPages
    lines += "stat.ms_leaf_pages: " + stat.leafPages
    lines += "stat.ms_overflow_pages: " + stat.overflowPages
    lines += "stat.ms_entries: " + stat.entries
    lines += "tree size env: " + TreeSizeEnv(env)
    lines += "tree size db: " + TreeSizeDb(db, txn)
    lines += ""

    Output(lines.toArray)
  }

  def DisplayTreeInfo(env:Env[ByteBuffer]): Unit =
  {
    val stat = env.stat()
    println("stat.ms_depth: " + stat.depth)
    println("stat.ms_branch_pages: " + stat.branchPages)
    println("stat.ms_leaf_pages: " + stat.leafPages)
    println("stat.ms_overflow_pages: " + stat.overflowPages)
    println("")
  }
}


import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException

object FileTools
{
  def append(lines:Array[String], fileName:String) : Unit =
  {
    var bw:java.io.BufferedWriter = null
    var fw:java.io.FileWriter = null
    try
    {
      val file = new File(fileName)
      // if file doesnt exists, then create it
      if (!file.exists) file.createNewFile
      // true = append file
      fw = new FileWriter(file.getAbsoluteFile, true)
      bw = new BufferedWriter(fw)
      for (line <- lines)
        bw.write(line + "\n")
    }
    catch
    {
      case e: IOException =>
        e.printStackTrace()
    }
    finally try
    {
      if (bw != null) bw.close()
      if (fw != null) fw.close()
    }
    catch
    {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }
}
