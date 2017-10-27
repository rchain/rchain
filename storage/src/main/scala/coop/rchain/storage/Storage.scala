/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.Storage

import java.io.File
import java.nio.ByteBuffer
import org.lmdbjava.Txn
import scala.io.Source


class Storage(storConf:StorageConfig) {

  if (!storConf.isValid())
    throw new RChainException("Storage: StrorageConfig is invalid")

  protected[Storage] val lmdb =
    new Lmdb(storConf.dirName, storConf.name,
      storConf.isKeyToValues, storConf.isWritable,
      storConf.baseDir, storConf.maxSize)

  val termTreeKeysSorted = new java.util.TreeSet[Key]
  def termTreeKeys: java.util.Iterator[Key] = {
    termTreeKeysSorted.iterator
  }


  def put[K, V](key: K, value: V,
                txn: Option[Txn[ByteBuffer]] = None): Unit = {

    if (key.isInstanceOf[Key]) {
      termTreeKeysSorted.add(key.asInstanceOf[Key])
      lmdb.put(key.asInstanceOf[Key].toString, value, txn)
    }
    else {
      if (key.isInstanceOf[String]) {
        try {
          val k = new Key(key.asInstanceOf[String])
          termTreeKeysSorted.add(k)
        }
        catch {
          case e:Throwable => { }
        }
      }
      lmdb.put(key, value, txn)
    }
  }

  def getInts[K](key: K, txn: Option[Txn[ByteBuffer]] = None)
  : Option[Array[Int]] = {
    lmdb.getInts(key)
  }
  def getStrings[K](key: K, txn: Option[Txn[ByteBuffer]] = None)
  : Option[Array[String]] = {
    lmdb.getStrings(key)
  }
  def getFloats[K](key: K, txn: Option[Txn[ByteBuffer]] = None)
  : Option[Array[Float]] = {
    lmdb.getFloats(key, txn)
  }
  def getDoubles[K](key: K, txn: Option[Txn[ByteBuffer]] = None)
  : Option[Array[Double]] = {
    lmdb.getDoubles(key, txn)
  }


  def deleteKey[K](key: K, txn: Option[Txn[ByteBuffer]] = None): Boolean = {

    if (key.isInstanceOf[Key]) {
      termTreeKeysSorted.remove(key.asInstanceOf[Key])
      lmdb.deleteKey(key.asInstanceOf[Key].toString, txn)
    }
    else {
      if (key.isInstanceOf[String]) {
        try {
          val k = new Key(key.asInstanceOf[String])
          termTreeKeysSorted.remove(k)
        }
        catch {
          case e:Throwable => { }
        }
      }
      lmdb.deleteKey(key, txn)
    }
  }

  def delete[K, V](key: K, value: V,
                   txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    lmdb.delete(key, value, txn)
  }

  def update[K, V](key: K, valueToBeReplaced: V, valueReplaceWith: V,
                   txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    lmdb.update(key, valueToBeReplaced, valueReplaceWith, txn)
  }


  def getMaxValueKeySize(): Int = {
    lmdb.getMaxKeySize()
  }
  def memoryInUse(txn: Option[Txn[ByteBuffer]] = None): Long = {
    lmdb.memoryInUse(txn)
  }

  def storageInUse: Long = ???

  def close(): Unit = {
    lmdb.close()
  }

  def deleteFiles(): Unit = {
    lmdb.deleteFiles()
  }

  def displayStrings: Unit = {

    val keyIter = termTreeKeys
    while (keyIter.hasNext()) {
      val key = keyIter.next()
      val keyStr = key.term
      val valueStrs = lmdb.getStrings(keyStr)
      print(keyStr +": ")
      for (value <- valueStrs.get) {
        print(value +", ")
      }
      println()
    }
  }

  def loadFile(filePath: String, display: Boolean = false): Unit = {
    if (display) {
      println(s"Load file: $filePath")
      println
    }

    val source = Source.fromFile(filePath)
    val lineIterator = source.getLines

    for (lineOriginal <- lineIterator) {
      val line = lineOriginal.trim

      if (!line.isEmpty && line.slice(0, 2) != "//") {
        val (key, value) = line.splitAt(line.lastIndexOf(' '))
        val keyNoWhites = key.replaceAll("\\s", "")
        val valueNoWhites = value.replaceAll("\\s", "")
        put(keyNoWhites, valueNoWhites)
      }
    }
  }
}


class StorageConfig {

  var isKeyToValues:Boolean = false
  var isWritable:Boolean = false
  var baseDir:Option[String] = None
  var dirName:Option[String] = None
  var name:Option[String] = None
  var maxSize:Long = Lmdb.minDbSize

  def isValid():Boolean = {

    if (baseDir.isDefined && !new File(baseDir.get).isDirectory)
      return false

    if (!name.isDefined || name.get.isEmpty)
      return false
    if (!dirName.isDefined || dirName.get.isEmpty)
      return false
    if (maxSize < Lmdb.minDbSize || Lmdb.maxDbSize < maxSize)
      return false
    true
  }
}
