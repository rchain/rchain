// If you search on a keyInt, the results you get back don't
// tell you what the data is in the cursor.
// Is there a way I can wrap the data such that the type
// of each datum can be determined?  If not, then
// all the data associated with a key must of one type.

import KeyValueStore._

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

import org.lmdbjava._
import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.SeekOp._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.DbiFlags.MDB_DUPSORT
import org.lmdbjava.Env.create


object KvsLmdbTest
{
  val display = false

  // Exception in thread "main" org.lmdbjava.Env$MapFullException: Environment mapsize reached (-30792)
  // mapSize: 1920
  //
  // val largestKey = 8000000
  // val keyValuesCount = 4
  // mapSize: 2048000000
  // duration: 58879 millis
  // duration: 57894 millis
  //
  // val largestKey = 10000000
  // val keyValuesCount = 4
  // org.lmdbjava.LmdbNativeException$ConstantDerviedException: Platform constant error code: EINVAL (22)

  val largestKey = 10000

  val keyValuesCount = 4

  val fileName = "lmdbjava_values_trans.txt"

  val usedKeys = new Array[Boolean](largestKey)

  // https://stackoverflow.com/questions/1252468/java-converting-string-to-and-from-bytebuffer-and-associated-problems
  val charset = Charset.forName("UTF-8")
  val encoder = charset.newEncoder()
  val decoder = charset.newDecoder()

  val rgen = scala.util.Random


  def main(args: Array[String]): Unit =
  {
    try
    {
      val kvs = new KeyValueStore(largestKey, keyValuesCount)
      val lines = ArrayBuffer[String]()
      lines += "number of keys: " + largestKey
      lines += "values/key: " + keyValuesCount
      lines += "map size: " + kvs.mapSize
      Output(lines.toArray)

      rgen.setSeed(MillisSinceEpoch())

      ResetUsedKeys()

      JvmWarmUp(new KeyValueStore(largestKey, keyValuesCount))

      val startPut = MillisSinceEpoch()

      for (i <- 0 until largestKey)
      {
        val key = NextKey()
        for (iValue <- 0 until keyValuesCount)
        {
          val value = key + 1 + iValue
          if (display) println(s"key: $key, value: $value")
          kvs.add(key, value)
        }
      }
      if (display) println()

      val stopPut = MillisSinceEpoch()

      ResetUsedKeys()

      val startGet = MillisSinceEpoch()

      for (i <- 0 until largestKey)
      {
        val key = NextKey()
        val values = kvs.get(key)
        for (iValue <- 0 until values.length)
        {
          val value = values(iValue)
          assert(value == key + 1 + iValue)
          if (display) println(s"key: $key, value: $value")
        }
      }
      if (display) println()

      val stopGet = MillisSinceEpoch()
      
      Outcome(startPut, stopPut, startGet, stopGet)
    }
    catch {
      case e: Throwable => {
        println("Exception: " + e)
        e.printStackTrace()
      }
    }
  }

  def NextKey(): Int =
  {
    var r = rgen.nextInt(largestKey)
    while (usedKeys(r))
      r = rgen.nextInt(largestKey)
    usedKeys(r) = true
    r
  }

  def ResetUsedKeys(): Unit =
  {
    for (i <- 0 until largestKey)
      usedKeys(i) = false
  }

  def MillisSinceEpoch(): Long =
  {
    Calendar.getInstance().getTimeInMillis()
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

  def JvmWarmUp(kvs:KeyValueStore): Unit =
  {
    for (key <- 0 until largestKey)
      kvs.add(key, key)
    for (key <- 0 until largestKey)
      kvs.get(key)
    kvs.close()
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


