import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.io.File
import java.lang.Integer.BYTES
import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.Charset
import java.util.Calendar

// import BlobTest.{cursor, largestKey}
import org.lmdbjava._
import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.SeekOp._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.DbiFlags.MDB_DUPSORT
import org.lmdbjava.Env.create


/*
number of keys: 500
blob length (bytes): 10485760

val mapSize:Long = (largestKey*2) * (mdb_val_size + blobLength)
map size: 1895841408

MapFullException: Environment mapsize reached (-30792)
*/
/*
Spelling error!  ConstantDerviedException

number of keys: 500
blob length (bytes): 10485760

val mapSize:Long = (largestKey*3) * (mdb_val_size + blobLength)

ConstantDerviedException: Platform constant error code: EINVAL (22)
*/

/*
number of keys: 1300
blob length (bytes): 1048576
val mapSize:Long = (largestKey + largestKey/2) * (mdb_val_size + blobLength)
map size: 2044754400
duration: 4928 ms
put: 1991 ms, 0 puts/ms
get: 2937 ms, 0 gets/ms
*/
/*
val largestKey:Int = 1400
blob length (bytes): 1048576

val mapSize:Long = (largestKey + largestKey/2) * (mdb_val_size + blobLength)

ConstantDerviedException: Platform constant error code: EINVAL (22)
*/


object BlobTest
{
  val display = false

  val largestKey:Int = 1300

  val blobLength = 1048576
  val blobs = new Array[String](10)

  val fileName = "lmdbjava_blob.txt"

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

  val mapSize:Long = (largestKey + largestKey/2) * (mdb_val_size + blobLength)

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
    val lines = ArrayBuffer[String]()
    lines += "number of keys: " + largestKey
    lines += "values/key: 1"
    lines += "blob length: " + blobLength
    lines += "map size: " + mapSize
    Output(lines.toArray)

    JvmWarmUp()

    InitBlobs()
    ResetUsedKeys()

    val startPut = MillisSinceEpoch()

    for (keyCount <- 0 until largestKey)
    {
      val key = NextKey()
      if (display)
        println(s"put key: $key, blob: " + blobs(key % 10).substring(0,1))
      add(key, blobs(key % 10))
    }
    if (display) println()

    val stopPut = MillisSinceEpoch()

    ResetUsedKeys()

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

    Outcome(startPut, stopPut, startGet, stopGet)
  }


  def add(key: Int, blob:String): Unit =
  {
    cursor.put(intToBb(key), strToBb(blob))
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
      var strBuf = new StringBuffer(blobLength)
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