/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package Storage

import java.io.File
import java.nio.ByteBuffer
import org.lmdbjava.DbiFlags.{MDB_CREATE, MDB_DUPSORT}
import org.lmdbjava.Env.create
import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.PutFlags.MDB_NODUPDATA
import org.lmdbjava.SeekOp._
import org.lmdbjava._
import scala.collection.mutable.ArrayBuffer

class Lmdb(_dirNameIn: Option[String],
           _nameIn: Option[String],
           _isKeyToValuesIn: Boolean = false,
           _isWritableIn: Boolean = false,
           _baseDirIn: Option[String] = None,
           _maxSizeIn: Long = Lmdb.minDbSize,
           _maxStrValueLength: Int = 0) {

  assert(_dirNameIn.isDefined && !_dirNameIn.get.isEmpty)
  assert(_nameIn.isDefined && !_nameIn.get.isEmpty)
  assert(!_baseDirIn.isDefined || !_baseDirIn.get.isEmpty)
  assert(Lmdb.minDbSize <= _maxSizeIn && _maxSizeIn <= Lmdb.maxDbSize)

  protected[Storage] val dirNameIn =
    if (_dirNameIn.isDefined && !_dirNameIn.get.isEmpty) _dirNameIn.get
    else { throw new RChainException("DB Directory is invalid") }
  protected[Storage] val nameIn =
    if (_nameIn.isDefined && !_nameIn.get.isEmpty) { _nameIn.get } else {
      throw new RChainException("Name is invalid")
    }
  protected[Storage] val isKeyToValuesIn = _isKeyToValuesIn
  protected[Storage] val isWritableIn = _isWritableIn
  protected[Storage] val baseDirIn =
    if (_baseDirIn.isDefined) {
      if (new File(_baseDirIn.get).isDirectory) { _baseDirIn.get } else {
        throw new RChainException("Base Directory is invalid")
      }
    } else System.getProperty("user.dir") // not our problem if throws
  protected[Storage] val mapSizeIn: Long =
    if (Lmdb.maxDbSize < _maxSizeIn) {
      Lmdb.maxDbSize
    } else if (_maxSizeIn < Lmdb.minDbSize) {
      Lmdb.minDbSize
    } else {
      _maxSizeIn
    }

  protected[Storage] val dbDir = new File(baseDirIn + "/" + dirNameIn)
  private var success = false
  if (!dbDir.exists) {
    success = dbDir.mkdir()
    assert(success)
    success = dbDir.setReadable(true, true)
    assert(success)
    success = dbDir.setWritable(true, true)
    assert(success)
  }

  protected[Storage] val dbDataFile = new File(dbDir.toPath + "/" + "data.mdb")
  protected[Storage] val dbLockFile = new File(dbDir.toPath + "/" + "lock.mdb")
  // TODO: "scala-2.12" is implementation dependent
  protected[Storage] val dbDataFileTarget =
    new File(
      dbDir.toPath + "/target/scala-2.12/classes/" + dirNameIn + "/" + "data.mdb")
  protected[Storage] val dbLockFileTarget =
    new File(
      dbDir.toPath + "/target/scala-2.12/classes/" + dirNameIn + "/" + "lock.mdb")

  val env: Env[ByteBuffer] =
    if (isWritableIn)
      create()
        .setMaxDbs(1)
        .setMapSize(mapSizeIn)
        .open(dbDir, MDB_WRITEMAP)
    else
      create()
        .setMaxDbs(1)
        .setMapSize(mapSizeIn)
        .open(dbDir)
  val db: Dbi[ByteBuffer] =
    if (isKeyToValuesIn) { env.openDbi(nameIn, MDB_CREATE, MDB_DUPSORT) } else {
      env.openDbi(nameIn, MDB_CREATE)
    }

  def put[K, V](key: K,
                value: V,
                txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }

    if (key.isInstanceOf[String]
        && getMaxKeySize < key.asInstanceOf[String].length) {
      throw new RChainException("put: key string is too long")
    }
    val bbKey = Bb.toBb(key)
    val bbValue = Bb.toBb(value)
    if (!bbKey.isDefined || !bbValue.isDefined) {
      return false
    }
    putByteBuffer(bbKey.get, bbValue.get, txn)
  }

  def putByteBuffer(key: ByteBuffer,
                    value: ByteBuffer,
                    txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)
    cursor.put(key, value)
    cursor.close()
    if (txnIn == None) { txn.commit() }
    true
  }

  def getInts[K](key: K,
                 txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Int]] = {

    if (key.isInstanceOf[Key]) {
      return getInts(key.asInstanceOf[Key].term, txnIn)
    }

    if (!Lmdb.isStringOrPrimitive(key))
      throw new RChainException("getInts[K](): key is not primitive or string")

    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.toBb(key)
    } else {
      bbKey = Some(Bb.strToBb(key.asInstanceOf[String]))
    }
    if (!bbKey.isDefined) {
      throw new RChainException("getInts[K](): key fails translation to Bb")
    }

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return None
      }

      if (isKeyToValues) {
        val blobsBuf = new ArrayBuffer[Int]()

        outcome = cursor.seek(MDB_FIRST_DUP)
        while (outcome) {
          blobsBuf += cursor.`val`.getInt
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
        if (blobsBuf.isEmpty) {
          return None
        }

        return Some(blobsBuf.toArray)
      } else {
        outcome = cursor.seek(MDB_GET_CURRENT)
        if (!outcome) {
          throw new RChainException(
            "TODO: cursor.get(ByteBuffer, MDB_GET_CURRENT) returned false")
        }

        val valueArray = new Array[Int](1)
        valueArray(0) = cursor.`val`.getInt
        return Some(valueArray)
      }
    } catch {
      case e: RChainException =>
        println("get(ByteBuffer): Option[Int]: " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
    None
  }

  // string s paired with a key are sorted
  def getStrings[K](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[String]] = {

    if (key.isInstanceOf[Key]) {
      return getStrings(key.asInstanceOf[Key].term, txnIn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException(
        "getStrings[K](): key is not primitive or string")
    }

    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.toBb(key)
    } else {
      bbKey = Some(Bb.strToBb(key.asInstanceOf[String]))
    }
    if (!bbKey.isDefined) {
      throw new RChainException("getStrings[K](): key fails translation to Bb")
    }

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return None
      }

      if (isKeyToValues) {
        val blobsBuf = new ArrayBuffer[String]()

        outcome = cursor.seek(MDB_FIRST_DUP)
        if (!outcome) {
          throw new RChainException(
            "getStrings: cursor.seek(MDB_FIRST_DUP) returned false")
        }
        while (outcome) {
          blobsBuf += Bb.bbToStr(cursor.`val`)
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
        if (blobsBuf.isEmpty) {
          return None
        }

        return Some(blobsBuf.toArray)
      } else {
        outcome = cursor.seek(MDB_GET_CURRENT)
        if (!outcome) {
          throw new RChainException(
            "cursor.getStrings(ByteBuffer, MDB_GET_CURRENT) returned false")
        }

        val valueArray = new Array[String](1)
        valueArray(0) = Bb.bbToStr(cursor.`val`)
        return Some(valueArray)
      }
    } catch {
      case e: RChainException =>
        println("getIntStrs(ByteBuffer): Option[Int]: " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
    None
  }

  def getLongs[K](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Long]] = {

    if (key.isInstanceOf[Key]) {
      return getLongs(key.asInstanceOf[Key].term, txnIn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getLongs[K](): key is not primitive or string")
    }

    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.toBb(key)
    } else {
      bbKey = Some(Bb.strToBb(key.asInstanceOf[String]))
    }
    if (!bbKey.isDefined) {
      throw new RChainException("getLongs[K](): key fails translation to Bb")
    }

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return None
      }

      if (isKeyToValues) {
        val blobsBuf = new ArrayBuffer[Long]()

        outcome = cursor.seek(MDB_FIRST_DUP)
        if (!outcome) {
          throw new RChainException(
            "getLongs: cursor.seek(MDB_FIRST_DUP) returned false")
        }
        while (outcome) {
          blobsBuf += cursor.`val`.getLong()
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
        if (blobsBuf.isEmpty) {
          return None
        }

        return Some(blobsBuf.toArray)
      } else {
        outcome = cursor.seek(MDB_GET_CURRENT)
        if (!outcome) {
          throw new RChainException(
            "cursor.getLongs(ByteBuffer, MDB_GET_CURRENT) returned false")
        }

        val valueArray = new Array[Long](1)
        valueArray(0) = cursor.`val`.getLong()
        return Some(valueArray)
      }
    } catch {
      case e: RChainException =>
        println("getLongs(ByteBuffer): Option[Long]: " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
    None
  }

  def getFloats[K](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Float]] = {

    if (key.isInstanceOf[Key]) {
      return getFloats(key.asInstanceOf[Key].term, txnIn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException(
        "getFloats[K](): key is not primitive or string")
    }

    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.toBb(key)
    } else {
      bbKey = Some(Bb.strToBb(key.asInstanceOf[String]))
    }
    if (!bbKey.isDefined) {
      throw new RChainException("getFloats[K](): key fails translation to Bb")
    }

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return None
      }

      if (isKeyToValues) {
        val blobsBuf = new ArrayBuffer[Float]()

        outcome = cursor.seek(MDB_FIRST_DUP)
        if (!outcome) {
          throw new RChainException(
            "getFloats: cursor.seek(MDB_FIRST_DUP) returned false")
        }
        while (outcome) {
          blobsBuf += cursor.`val`.getFloat()
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
        if (blobsBuf.isEmpty) {
          return None
        }

        return Some(blobsBuf.toArray)
      } else {
        outcome = cursor.seek(MDB_GET_CURRENT)
        if (!outcome) {
          throw new RChainException(
            "cursor.getFloats(ByteBuffer, MDB_GET_CURRENT) returned false")
        }

        val valueArray = new Array[Float](1)
        valueArray(0) = cursor.`val`.getFloat()
        return Some(valueArray)
      }
    } catch {
      case e: RChainException =>
        println("getLongs(ByteBuffer): Option[Long]: " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
    None
  }

  def getDoubles[K](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Double]] = {

    if (key.isInstanceOf[Key]) {
      return getDoubles(key.asInstanceOf[Key].term, txnIn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException(
        "getDoubles[K](): key is not primitive or string")
    }

    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.toBb(key)
    } else {
      bbKey = Some(Bb.strToBb(key.asInstanceOf[String]))
    }
    if (!bbKey.isDefined) {
      throw new RChainException("getDoubles[K](): key fails translation to Bb")
    }

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return None
      }

      if (isKeyToValues) {
        val blobsBuf = new ArrayBuffer[Double]()

        outcome = cursor.seek(MDB_FIRST_DUP)
        if (!outcome) {
          throw new RChainException(
            " getDoubles: cursor.seek(MDB_FIRST_DUP) returned false")
        }
        while (outcome) {
          blobsBuf += cursor.`val`.getDouble()
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
        if (blobsBuf.isEmpty) {
          return None
        }

        return Some(blobsBuf.toArray)
      } else {
        outcome = cursor.seek(MDB_GET_CURRENT)
        if (!outcome) {
          throw new RChainException(
            "cursor.getDoubles(ByteBuffer, MDB_GET_CURRENT) returned false")
        }

        val valueArray = new Array[Double](1)
        valueArray(0) = cursor.`val`.getDouble()
        return Some(valueArray)
      }
    } catch {
      case e: RChainException =>
        println("getDoubles(ByteBuffer): " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
    None
  }

  def getShorts[K](key: K,
                   txn: Option[Txn[ByteBuffer]] = None): Option[Array[Short]] =
    ???

  def getChars[K](key: K,
                  txn: Option[Txn[ByteBuffer]] = None): Option[Array[Char]] =
    ???

  def getBooleans[K](
      key: K,
      txn: Option[Txn[ByteBuffer]] = None): Option[Array[Boolean]] = ???

  def getBytes[K](key: K,
                  txn: Option[Txn[ByteBuffer]] = None): Option[Array[Byte]] =
    ???

  def deleteKey[K](key: K, txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }

    if (key.isInstanceOf[Key]) {
      return deleteKey(key.asInstanceOf[Key].term, txnIn)
    }
    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException(
        "deleteKey: key or value is not a string or primitive")
    }

    val bbKey = Bb.toBb(key)
    if (!bbKey.isDefined) {
      return false
    }

    var deleted = false

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      if (isKeyToValues) {
        cursor.delete(MDB_NODUPDATA)
      } else {
        cursor.delete()
      }

      deleted = true
    } catch {
      case e: RChainException =>
        println("deleteKey(ByteBuffer): " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  def delete[K, V](key: K,
                   value: V,
                   txn: Option[Txn[ByteBuffer]] = None): Boolean = {

    if (!isWritable) { return false }

    if (key.isInstanceOf[Key]) {
      return delete(key.asInstanceOf[Key].term, value, txn)
    }
    if (value.isInstanceOf[Key]) {
      return delete(key, value.asInstanceOf[Key].term, txn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("delete: key is not a string or primitive")
    }
    if (!Lmdb.isStringOrPrimitive(value)) {
      throw new RChainException("delete: value is not a string or primitive")
    }

    val bbKey = Bb.toBb(key)
    if (!bbKey.isDefined) { return false }

    if (value.isInstanceOf[Byte]) {
      ???
    } else if (value.isInstanceOf[Boolean]) {
      ???
    } else if (value.isInstanceOf[Char]) {
      ???
    } else if (value.isInstanceOf[Short]) {
      ???
    } else if (value.isInstanceOf[Int]) {
      return deleteBbValueInt(bbKey.get, Bb.toBb(value), txn)
    } else if (value.isInstanceOf[Long]) {
      return deleteBbValueLong(bbKey.get, Bb.toBb(value), txn)
    } else if (value.isInstanceOf[Float]) {
      return deleteBbValueFloat(bbKey.get, Bb.toBb(value), txn)
    } else if (value.isInstanceOf[Double]) {
      return deleteBbValueDouble(bbKey.get, Bb.toBb(value), txn)
    } else if (value.isInstanceOf[String]) {
      return deleteBbValueString(bbKey.get, Bb.toBb(value), txn)
    }

    throw new RChainException("delete: value is not a string or primitive")
  }

  protected[Storage] def deleteBbValueInt(key: ByteBuffer,
                       optionValue: Option[ByteBuffer],
                       txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }
    if (!isKeyToValues) { return false } // use deleteKey()
    if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueInt: value is invalid")
    }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    var deleted = false

    try {
      val bbValue = optionValue.get
      val value = bbValue.getInt

      var outcome = cursor.get(key, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      outcome = cursor.seek(MDB_FIRST_DUP)
      while (outcome) {
        val cursorValue: ByteBuffer = cursor.`val`
        val v = cursorValue.getInt
        if (value == v) {
          cursor.delete()
          deleted = true
          outcome = false
        } else {
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
      }
    } catch {
      case e: RChainException =>
        println("deleteBbValueInt: " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  protected[Storage] def deleteBbValueLong(key: ByteBuffer,
                        optionValue: Option[ByteBuffer],
                        txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }
    if (!isKeyToValues) { return false } // use deleteKey()
    if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueLong: value is invalid")
    }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    var deleted = false

    try {
      val bbValue = optionValue.get
      val value = bbValue.getLong

      var outcome = cursor.get(key, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      outcome = cursor.seek(MDB_FIRST_DUP)
      while (outcome) {
        val cursorValue: ByteBuffer = cursor.`val`
        val v = cursorValue.getLong
        if (value == v) {
          cursor.delete()
          deleted = true
          outcome = false
        } else {
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
      }
    } catch {
      case e: RChainException =>
        println("deleteBbValueLong: " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  protected[Storage] def deleteBbValueFloat(key: ByteBuffer,
                         optionValue: Option[ByteBuffer],
                         txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }
    if (!isKeyToValues) { return false } // use deleteKey()
    if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueFloat: value is invalid")
    }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    var deleted = false

    try {
      val bbValue = optionValue.get
      val value = bbValue.getFloat

      var outcome = cursor.get(key, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      outcome = cursor.seek(MDB_FIRST_DUP)
      while (outcome) {
        val cursorValue: ByteBuffer = cursor.`val`
        val v = cursorValue.getFloat
        if (value == v) {
          cursor.delete()
          deleted = true
          outcome = false
        } else {
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
      }
    } catch {
      case e: RChainException =>
        println("deleteBbValueFloat: " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  protected[Storage] def deleteBbValueDouble(key: ByteBuffer,
                          optionValue: Option[ByteBuffer],
                          txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }
    if (!isKeyToValues) { return false } // use deleteKey()
    if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueDouble: value is invalid")
    }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    var deleted = false

    try {
      val bbValue = optionValue.get
      val value = bbValue.getDouble

      var outcome = cursor.get(key, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      outcome = cursor.seek(MDB_FIRST_DUP)
      while (outcome) {
        val cursorValue: ByteBuffer = cursor.`val`
        val v = cursorValue.getDouble
        if (value == v) {
          cursor.delete()
          deleted = true
          outcome = false
        } else {
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
      }
    } catch {
      case e: RChainException =>
        println("deleteBbValueDouble: " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  protected[Storage] def deleteBbValueString(key: ByteBuffer,
                          optionValue: Option[ByteBuffer],
                          txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }
    if (!isKeyToValues) { return false } // use deleteKey()
    if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueString: value is invalid")
    }

    val txn =
      if (txnIn == None) { env.txnWrite() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    var deleted = false

    try {
      var outcome = cursor.get(key, MDB_SET_KEY)
      if (!outcome) {
        return false
      }

      val bbValue = optionValue.get
      val value = Bb.bbToStr(bbValue)

      outcome = cursor.seek(MDB_FIRST_DUP)
      while (outcome) {
        val v = Bb.bbToStr(cursor.`val`)
        if (value == v) {
          cursor.delete()
          deleted = true
          outcome = false
        } else {
          outcome = cursor.seek(MDB_NEXT_DUP)
        }
      }
    } catch {
      case e: RChainException =>
        println("deleteBbValueInt: " + e)
        return false
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        if (deleted) { txn.commit() } else { txn.close() }
      }
    }
    deleted
  }

  protected[Storage] def deleteBbValueByte(key: ByteBuffer,
                        optionValue: Option[ByteBuffer],
                        txn: Option[Txn[ByteBuffer]] = None): Boolean = ???
  protected[Storage] def deleteBbValueBoolean(key: ByteBuffer,
                           optionValue: Option[ByteBuffer],
                           txn: Option[Txn[ByteBuffer]] = None): Boolean = ???
  protected[Storage] def deleteBbValueChar(key: ByteBuffer,
                        optionValue: Option[ByteBuffer],
                        txn: Option[Txn[ByteBuffer]] = None): Boolean = ???
  protected[Storage] def deleteBbValueShort(key: ByteBuffer,
                         optionValue: Option[ByteBuffer],
                         txn: Option[Txn[ByteBuffer]] = None): Boolean = ???

  def update[K, V](key: K,
                   valueToBeReplaced: V,
                   valueReplaceWith: V,
                   txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }

    if (key.isInstanceOf[Key]) {
      return update(key.asInstanceOf[Key].term,
                    valueToBeReplaced,
                    valueReplaceWith,
                    txn)
    }
    if (valueToBeReplaced.isInstanceOf[Key] && valueReplaceWith
          .isInstanceOf[Key]) {
      return update(key,
                    valueToBeReplaced.asInstanceOf[Key].term,
                    valueReplaceWith.asInstanceOf[Key].term,
                    txn)
    }

    if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("update: key is not a string or primitive")
    }
    if (!Lmdb.isStringOrPrimitive(valueToBeReplaced)) {
      throw new RChainException(
        "update: valueToBeReplaced is not a string or primitive")
    }
    if (!Lmdb.isStringOrPrimitive(valueReplaceWith)) {
      throw new RChainException(
        "update: valueReplaceWith is not a string or primitive")
    }
    val bbKey = Bb.toBb(key)
    if (!bbKey.isDefined) {
      return false
    }

    if (valueToBeReplaced.isInstanceOf[Byte]) {
      ???
    } else if (valueToBeReplaced.isInstanceOf[Boolean]) {
      ???
    } else if (valueToBeReplaced.isInstanceOf[Char]) {
      ???
    } else if (valueToBeReplaced.isInstanceOf[Short]) {
      ???
    } else if (valueToBeReplaced.isInstanceOf[Int]) {
      return updateBbValueInt(bbKey.get,
                              valueToBeReplaced.asInstanceOf[Int],
                              valueReplaceWith.asInstanceOf[Int],
                              txn)
    } else if (valueToBeReplaced.isInstanceOf[Long]) {
      return updateBbValueLong(bbKey.get,
                               valueToBeReplaced.asInstanceOf[Long],
                               valueReplaceWith.asInstanceOf[Long],
                               txn)
    } else if (valueToBeReplaced.isInstanceOf[Float]) {
      return updateBbValueFloat(bbKey.get,
                                valueToBeReplaced.asInstanceOf[Float],
                                valueReplaceWith.asInstanceOf[Float],
                                txn)
    } else if (valueToBeReplaced.isInstanceOf[Double]) {
      return updateBbValueDouble(bbKey.get,
                                 valueToBeReplaced.asInstanceOf[Double],
                                 valueReplaceWith.asInstanceOf[Double],
                                 txn)
    } else if (valueToBeReplaced.isInstanceOf[String]) {
      return updateBbValueString(bbKey.get,
                                 valueToBeReplaced.asInstanceOf[String],
                                 valueReplaceWith.asInstanceOf[String],
                                 txn)
    }

    throw new RChainException("delete: value is not a string or primitive")
  }

  protected[Storage] def updateBbValueInt(key: ByteBuffer,
                       valueToBeReplaced: Int,
                       valueReplaceWith: Int,
                       txn: Option[Txn[ByteBuffer]] = None): Boolean = {

    try {
      val bbToBeReplaced = Bb.toBb(valueToBeReplaced)
      val bbReplaceWith = Bb.toBb(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueInt: bbToInt failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueInt(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getInt, txn)
      }
      if (!outcome) { return false }
      return putByteBuffer(key, bbReplaceWith.get, txn)
    } catch {
      case e: RChainException =>
        println("updateBbValueInt(ByteBuffer): " + e)
        return false
      case e: Throwable =>
        throw e
    }
    false
  }

  protected[Storage]  def updateBbValueLong(key: ByteBuffer,
                        valueToBeReplaced: Long,
                        valueReplaceWith: Long,
                        txn: Option[Txn[ByteBuffer]] = None): Boolean = ???

  protected[Storage] def updateBbValueFloat(key: ByteBuffer,
                         valueToBeReplaced: Float,
                         valueReplaceWith: Float,
                         txn: Option[Txn[ByteBuffer]] = None): Boolean = ???

  protected[Storage] def updateBbValueDouble(key: ByteBuffer,
                          valueToBeReplaced: Double,
                          valueReplaceWith: Double,
                          txn: Option[Txn[ByteBuffer]] = None): Boolean = ???

  protected[Storage] def updateBbValueString(key: ByteBuffer,
                          valueToBeReplaced: String,
                          valueReplaceWith: String,
                          txn: Option[Txn[ByteBuffer]] = None): Boolean = ???

  def rowsIntInt(
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[(Int, Int)]] = {

    var array = new ArrayBuffer[(Int, Int)]

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.seek(MDB_FIRST)
      if (!outcome) {
        return None
      }

      while (outcome) {
        val key = cursor.key().getInt
        val value = cursor.`val`.getInt
        array += ((key, value))
        outcome = cursor.seek(MDB_NEXT)
      }
      Some(array.toArray)
    } catch {
      case e: RChainException =>
        println("rowsIntInt(Option[Txn[ByteBuffer]]): " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }
  }

  protected[Storage] def displayRowsIntInt(): Unit = {
    val rowsOption = rowsIntInt()
    assert(rowsOption.isDefined)
    val rows = rowsOption.get
    var i = 1
    for (row <- rows) {
      println(s"row $i. key: ${row._1}, value: ${row._2}")
      i += 1
    }
  }

  def rowsStrStr(txnIn: Option[Txn[ByteBuffer]] = None)
    : Option[Array[(String, String)]] = {

    var array = new ArrayBuffer[(String, String)]

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.seek(MDB_FIRST)
      if (!outcome) {
        return None
      }

      while (outcome) {
        val key = Bb.bbToStr(cursor.key())
        val value = Bb.bbToStr(cursor.`val`)
        array += ((key, value))
        outcome = cursor.seek(MDB_NEXT)
      }
      Some(array.toArray)
    } catch {
      case e: RChainException =>
        println("rowsStrStr(Option[Txn[ByteBuffer]]): " + e)
        return None
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        txn.close()
      }
    }
  }

  protected[Storage] def displayRowsStrStr(): Unit = {
    val rowsOption = rowsStrStr()
    assert(rowsOption.isDefined)
    val rows = rowsOption.get
    var i = 1
    for (row <- rows) {
      println(s"row $i. key: ${row._1}, value: ${row._2}")
      i += 1
    }
  }

  def memoryInUse(txnIn: Option[Txn[ByteBuffer]] = None): Long = {
    val txn: Txn[ByteBuffer] =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val stat = db.stat(txn)
    if (txnIn == None) { txn.close() }
    val size = stat.pageSize *
      (stat.branchPages + stat.leafPages + stat.overflowPages)
    size
  }

  def close(): Unit = {
    // mdb_dbi_close(): "Normally unnecessary."
    // http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5
    // db.close()
    env.close()
  }

  def deleteFiles(): Unit = {

    var success = false
    if (dbDataFile != null && dbDataFile.exists()) {
      success = dbDataFile.delete(); assert(success)
    }
    if (dbLockFile != null && dbLockFile.exists()) {
      success = dbLockFile.delete(); assert(success)
    }
    if (dbDataFileTarget != null && dbDataFileTarget.exists()) {
      success = dbDataFileTarget.delete(); assert(success)
    }
    if (dbLockFileTarget != null && dbLockFileTarget.exists()) {
      success = dbLockFileTarget.delete(); assert(success)
    }
  }

  // max key size is hard-coded as 511
  // mdb.c
  /**     @brief The max size of a key we can write, or 0 for computed max.
    *
    *      This macro should normally be left alone or set to 0.
    *      Note that a database with big keys or dupsort data cannot be
    *      reliably modified by a liblmdb which uses a smaller max.
    *      The default is 511 for backwards compat, or 0 when #MDB_DEVEL.
    *
    *      Other values are allowed, for backwards compat.  However:
    *      A value bigger than the computed max can break if you do not
    *      know what you are doing, and liblmdb <= 0.9.10 can break when
    *      modifying a DB with keys/dupsort data bigger than its max.
    *
    *      Data items in an #MDB_DUPSORT database are also limited to
    *      this size, since they're actually keys of a sub-DB.  Keys and
    *      #MDB_DUPSORT data items must fit on a node in a regular page.
    */
  // #ifndef MDB_MAXKEYSIZE
  // #define MDB_MAXKEYSIZE   ((MDB_DEVEL) ? 0 : 511)
  // #endif
  /**     The maximum size of a key we can write to the environment. */
  /*
  #if MDB_MAXKEYSIZE
  #define ENV_MAXKEY(env) (MDB_MAXKEYSIZE)
  #else
  #define ENV_MAXKEY(env) ((env)->me_maxkey)
  #endif
   */
  def getMaxKeySize(): Int = { env.getMaxKeySize() }

  def dirName: String = { dirNameIn }
  def name: String = { nameIn }
  def isKeyToValues: Boolean = { isKeyToValuesIn }
  def isWritable: Boolean = { isWritableIn }
  def baseDir: String = { baseDirIn }
  def maxMaxKeySize: Long = { env.getMaxKeySize }
}

object Lmdb {

  val mdb_val_size = 16 // sizeof(mdb_val)

  val maxDbSize = 17592186040320L
  val minDbSize = 448000

  // If MDB_DUPSORT then the size of a value cannot exceed 511
  // mdb.c
  /*
  #if SIZE_MAX > MAXDATASIZE
  if (data->mv_size > ((mc->mc_db->md_flags & MDB_DUPSORT) ? ENV_MAXKEY(env) : MAXDATASIZE))
    return MDB_BAD_VALSIZE;
  #else
  if ((mc->mc_db->md_flags & MDB_DUPSORT) && data->mv_size > ENV_MAXKEY(env))
    return MDB_BAD_VALSIZE;
  #endif
   */

  def isStringOrPrimitive[T](t: T): Boolean = {
    if (t.isInstanceOf[String]) return true
    isPrimitive(t)
  }

  def isPrimitive[T](t: T): Boolean = {
    if (t.isInstanceOf[Byte]) return true
    else if (t.isInstanceOf[Boolean]) return true
    else if (t.isInstanceOf[Char]) return true
    else if (t.isInstanceOf[Short]) return true
    else if (t.isInstanceOf[Int]) return true
    else if (t.isInstanceOf[Long]) return true
    else if (t.isInstanceOf[Float]) return true
    else if (t.isInstanceOf[Double]) return true
    false
  }
}

// RChainExceptions should not be seen by client.
// Let the user receive Lmdbjava exceptions
class RChainException(msg: String) extends Exception(msg) {}
