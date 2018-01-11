/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

/*
Storage is a key-value database that provides a Prolog-like unification
mechanism for querying.  More specifically, it stores data in an Lmdb object.

Storage accepts keys and values of type:
 * primitive Java data types (and thus no unsigned types)
 * Java String
 * Key (defined in this package)

Only keys of type Key are eligible for unification.

In this implementation, all keys that can be unified must be of type Key.

Just like the Lmdb class, the Storage class can be configured:
 * read-only or writable
 * a key is associated with a single key or multiple keys

 */

package coop.rchain.storage

import coop.rchain.storage.Bb.Bbable
import java.io.File
import java.nio.ByteBuffer

import coop.rchain.storage.QueryTools.{createKeySubstition}
import org.lmdbjava.Txn

import scala.collection.mutable.{ArrayBuffer, LinkedHashSet}
import scala.io.Source

class Storage(storConf: StorageConfig) {

  if (!storConf.isValid())
    throw new RChainException("Storage: StrorageConfig is invalid")

  protected[storage] val lmdb =
    new Lmdb(storConf.dirName,
             storConf.name,
             storConf.isKeyToValues,
             storConf.isWritable,
             storConf.baseDir,
             storConf.maxSize)

  // uniKeys returns a list of all keys (of type Key) in storage.
  def uniKeys: java.util.Iterator[Key] = {
    termTreeKeysSorted.iterator
  }
  protected[storage] val termTreeKeysSorted = new java.util.TreeSet[Key]

  // returns an array of keys that unify with the query
  def unifyQuery(query: Key): Array[Key] = {
    var bindings = LinkedHashSet[Array[Binding]]()

    val keyIter = uniKeys
    while (keyIter.hasNext()) {
      val key = keyIter.next()
      val keyName = key.name
      val keyParams = key.params
      if (query.name == keyName && query.arity == key.arity) {
        val (success, bindingsToAdd) =
          QueryTools.unifyParams(query.params.params, keyParams.params)
        if (success) {
          bindings += bindingsToAdd
        }
      }
    }

    var keys = new ArrayBuffer[Key]()
    for (bindingsArray <- bindings) {
      val keySub = createKeySubstition(query, bindingsArray)
      val valuesOption = getStrings(keySub.term)
      if (valuesOption.isDefined) {
        keys += keySub
      }
    }
    assert(keys.size == bindings.size)

    keys.toArray
  }

  // add a key-value row to the database
  def put[K: Bbable, V: Bbable](key: K,
                                value: V,
                                txn: Option[Txn[ByteBuffer]] = None): Unit = {

    if (key.isInstanceOf[Key]) {
      termTreeKeysSorted.add(key.asInstanceOf[Key])
      lmdb.put(key.asInstanceOf[Key].toString, value, txn)
    } else {
      if (key.isInstanceOf[String]) {
        try {
          val k = new Key(key.asInstanceOf[String])
          termTreeKeysSorted.add(k)
        } catch {
          case e: Throwable => {}
        }
      }
      lmdb.put(key, value, txn)
    }
  }

  // The getXx[K](key:K) returns an array of Xs.
  // This set of methods resist being expressed as a
  // single generic method due to the lack of genericity
  // of ByteBuffer.

  def getInts[K: Bbable](
      key: K,
      txn: Option[Txn[ByteBuffer]] = None): Option[Array[Int]] = {
    lmdb.getInts(key)
  }
  def getStrings[K: Bbable](
      key: K,
      txn: Option[Txn[ByteBuffer]] = None): Option[Array[String]] = {
    lmdb.getStrings(key)
  }
  def getFloats[K: Bbable](
      key: K,
      txn: Option[Txn[ByteBuffer]] = None): Option[Array[Float]] = {
    lmdb.getFloats(key, txn)
  }
  def getDoubles[K: Bbable](
      key: K,
      txn: Option[Txn[ByteBuffer]] = None): Option[Array[Double]] = {
    lmdb.getDoubles(key, txn)
  }

  // delete a key and the value associated with it
  def deleteKey[K: Bbable](key: K,
                           txn: Option[Txn[ByteBuffer]] = None): Boolean = {

    if (key.isInstanceOf[Key]) {
      termTreeKeysSorted.remove(key.asInstanceOf[Key])
      lmdb.deleteKey(key.asInstanceOf[Key].toString, txn)
    } else {
      if (key.isInstanceOf[String]) {
        try {
          val k = new Key(key.asInstanceOf[String])
          termTreeKeysSorted.remove(k)
        } catch {
          case e: Throwable => {}
        }
      }
      lmdb.deleteKey(key, txn)
    }
  }

  // delete a value associated with a key
  def delete[K: Bbable, V: Bbable](
      key: K,
      value: V,
      txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    lmdb.delete(key, value, txn)
  }

  // update a value associated with a key
  def update[K: Bbable, V: Bbable](
      key: K,
      valueToBeReplaced: V,
      valueReplaceWith: V,
      txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    lmdb.update(key, valueToBeReplaced, valueReplaceWith, txn)
  }

  // return the size limitation imposed on keys by lmdb
  def getMaxValueKeySize(): Int = {
    lmdb.getMaxKeySize()
  }

  // Return the memory in use by the lmdb database, excluding
  // other memory that supports the lmdb database
  def memoryInUse(txn: Option[Txn[ByteBuffer]] = None): Long = {
    lmdb.memoryInUse(txn)
  }

  def close(): Unit = {
    lmdb.close()
  }

  // delete the data.mdb and lock.mdb files
  def deleteFiles(): Unit = {
    lmdb.deleteFiles()
  }

  protected[storage] def dumpUniKeys: Array[Key] = {
    val arrBuf = new ArrayBuffer[Key]
    val keyIter = uniKeys
    while (keyIter.hasNext()) {
      val key = keyIter.next()
      arrBuf += key
    }
    arrBuf.toArray
  }

  protected[storage] def displayUniKeys: Unit = {

    val uniKeys = dumpUniKeys
    for (uniKey <- uniKeys) {
      val keyStr = uniKey.term
      val valueStrs = lmdb.getStrings(keyStr)
      print(keyStr + ": ")
      for (value <- valueStrs.get) {
        print(value + ", ")
      }
      println()
    }
  }

  // Load a file of Keys and values to add to the database.
  // Each line of the file must be of the form:
  //     key value
  // For example:
  //     a(b(c(A), d(e(B))), f(C)) Y
  def loadFile(filePath: String, display: Boolean = false): Unit = {
    if (display) {
      lmdb.Log(s"Load file: $filePath")
      displayUniKeys
      lmdb.Log("")
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
    if (display) {
      displayUniKeys
      lmdb.Log("")
    }
  }
}

// StorageConfig is a class to configure a Storage object.
// These parameters are explained in the Lmdb constructor.
class StorageConfig {

  var isKeyToValues: Boolean = false
  var isWritable: Boolean = false
  var baseDir: Option[String] = None
  var dirName: Option[String] = None
  var name: Option[String] = None
  var maxSize: Long = Lmdb.minDbSize

  def isValid(): Boolean = {
    baseDir.isDefined && new File(baseDir.get).isDirectory &&
    name.isDefined && !name.get.isEmpty &&
    dirName.isDefined && !dirName.get.isEmpty &&
    Lmdb.minDbSize <= maxSize && maxSize <= Lmdb.maxDbSize
  }
}
