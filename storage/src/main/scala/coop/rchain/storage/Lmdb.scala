/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

/*
Lmdb is a key-value databaset.

Lmdb accepts keys and values of type:
 * primitive Java data types (and thus no unsigned types)
 * Java String
 * Key (defined in this package)
 * Java ByteBuffer

The Lmdb class can be configured:
 * read-only or writable
 * a key is associated with a single key or multiple keys

 */

package coop.rchain.storage

import coop.rchain.storage.Bb.Bbable
import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Paths
import org.lmdbjava.DbiFlags.{MDB_CREATE, MDB_DUPSORT}
import org.lmdbjava.Env.create
import org.lmdbjava.EnvFlags._
import org.lmdbjava.GetOp._
import org.lmdbjava.PutFlags.MDB_NODUPDATA
import org.lmdbjava.SeekOp._
import org.lmdbjava._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

/*
Construct an Lmdb object.

Parameters:
 * dirNameIn: The name of the directory in which the data.mdb
  and lock.mdb files reside.  If it does not exist, it is created.
  It will be created within baseDirIn.
 * nameIn: The name of the database.  This name is used only within lmdb.
 * isWritableIn:  Whether the database is opened for writing or not.
 * isKeyToValuesIn:  Whether a key is associated with a single value
  or multiple values.
 * baseDirIn: The base directory path in which database resides.
  If it is not provided, the directory path from which the
  executable runs is used.
 * maxSizeIn: The maximum size to which the database can grow.
 */

class Lmdb(dirNameIn: Option[String],
           nameIn: Option[String],
           isKeyToValuesIn: Boolean = false,
           isWritableIn: Boolean = false,
           baseDirIn: Option[String] = None,
           maxSizeIn: Long = Lmdb.minDbSize) {

  protected[storage] val debug = true

  assert(dirNameIn.isDefined && !dirNameIn.get.isEmpty)
  assert(nameIn.isDefined && !nameIn.get.isEmpty)
  assert(!baseDirIn.isDefined || !baseDirIn.get.isEmpty)
  assert(Lmdb.minDbSize <= maxSizeIn && maxSizeIn <= Lmdb.maxDbSize)

  val dirName =
    if (dirNameIn.isDefined && !dirNameIn.get.isEmpty) { dirNameIn.get } else {
      throw new RChainException("DB Directory is invalid")
    }
  val dbName =
    if (nameIn.isDefined && !nameIn.get.isEmpty) { nameIn.get } else {
      throw new RChainException("Name is invalid")
    }
  val baseDir =
    if (baseDirIn.isDefined) {
      if (new File(baseDirIn.get).isDirectory) { baseDirIn.get } else {
        throw new RChainException("Base Directory is invalid")
      }
    } else {
      System.getProperty("user.dir")
    }
  val mapSizeIn: Long =
    if (Lmdb.maxDbSize < maxSizeIn) {
      Lmdb.maxDbSize
    } else if (maxSizeIn < Lmdb.minDbSize) {
      Lmdb.minDbSize
    } else {
      maxSizeIn
    }

  protected[storage] val dbDir = new File(baseDir.toString + "/" + dirName)
  private var success = false
  if (!dbDir.exists) {
    success = dbDir.mkdir()
    assert(success)
  }

  protected[storage] val dbDataFile = new File(dbDir.toPath + "/" + "data.mdb")
  protected[storage] val dbLockFile = new File(dbDir.toPath + "/" + "lock.mdb")

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
    if (isKeyToValuesIn) { env.openDbi(nameIn.get, MDB_CREATE, MDB_DUPSORT) } else {
      env.openDbi(nameIn.get, MDB_CREATE)
    }

  // add a key-value row to the database
  def put[K: Bbable, V: Bbable](
      key: K,
      value: V,
      txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else {
      if (key.isInstanceOf[String]
          && getMaxKeySize < key.asInstanceOf[String].length) {
        throw new RChainException("put(): key string is too long")
      }
      val bbKey: Option[ByteBuffer] = Bb.create(key)
      val bbValue = Bb.create(value)
      if (!bbKey.isDefined || !bbValue.isDefined) {
        false
      } else {
        putByteBuffer(bbKey.get, bbValue.get, txn)
      }
    }
  }

  def putByteBuffer(key: ByteBuffer,
                    value: ByteBuffer,
                    txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else {
      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)
      cursor.put(key, value)
      cursor.close()
      if (txnIn == None) {
        txn.commit()
      }
      true
    }
  }

  // The getXx[K](key:K) returns an array of Xs.
  // This set of methods resist being expressed as a
  // single generic method due to the lack of genericity
  // of ByteBuffer.

  def getInts[K: Bbable](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Int]] = {

    if (key.isInstanceOf[Key]) {
      getInts(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getInts(): key is not primitive or string")
    } else {
      var returnVal: Option[Array[Int]] = None

      var bbKey = Lmdb.makeBbKey(key)

      val txn =
        if (txnIn == None) {
          env.txnRead()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      try {
        var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
        if (!outcome) {
          returnVal = None
        } else {
          if (isKeyToValues) {
            val blobsBuf = new ArrayBuffer[Int]()

            outcome = cursor.seek(MDB_FIRST_DUP)
            while (outcome) {
              blobsBuf += cursor.`val`.getInt
              outcome = cursor.seek(MDB_NEXT_DUP)
            }

            returnVal = if (blobsBuf.isEmpty) {
              None
            } else {
              Some(blobsBuf.toArray)
            }
          } else {
            outcome = cursor.seek(MDB_GET_CURRENT)
            if (!outcome) {
              throw new RChainException("getInts(): MDB_GET_CURRENT")
            }

            val valueArray = new Array[Int](1)
            valueArray(0) = cursor.`val`.getInt
            returnVal = Some(valueArray)
          }
        }
      } catch {
        case e: RChainException =>
          Log("getInts(): " + e)
          returnVal = None
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          txn.close()
        }
      }
      returnVal
    }
  }

  // strings paired with a key are sorted
  def getStrings[K: Bbable](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[String]] = {

    if (key.isInstanceOf[Key]) {
      getStrings(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getStrings(): key is not primitive or string")
    } else {
      var returnVal: Option[Array[String]] = None

      var bbKey = Lmdb.makeBbKey(key)

      val txn =
        if (txnIn == None) {
          env.txnRead()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      try {
        var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
        if (!outcome) {
          returnVal = None
        } else {
          if (isKeyToValues) {
            val blobsBuf = new ArrayBuffer[String]()

            outcome = cursor.seek(MDB_FIRST_DUP)
            if (!outcome) {
              throw new RChainException("getStrings(): MDB_FIRST_DUP")
            }
            while (outcome) {
              blobsBuf += Bb.bbToStr(cursor.`val`)
              outcome = cursor.seek(MDB_NEXT_DUP)
            }

            returnVal = if (blobsBuf.isEmpty) {
              None
            } else {
              Some(blobsBuf.toArray)
            }
          } else {
            outcome = cursor.seek(MDB_GET_CURRENT)
            if (!outcome) {
              throw new RChainException("getStrings(): MDB_GET_CURRENT")
            }

            val valueArray = new Array[String](1)
            valueArray(0) = Bb.bbToStr(cursor.`val`)
            returnVal = Some(valueArray)
          }
        }
      } catch {
        case e: RChainException =>
          Log("getStrings(): " + e)
          returnVal = None
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          txn.close()
        }
      }
      returnVal
    }
  }

  def getLongs[K: Bbable](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Long]] = {

    if (key.isInstanceOf[Key]) {
      getLongs(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getLongs(): key is not primitive or string")
    } else {
      var returnVal: Option[Array[Long]] = None

      var bbKey = Lmdb.makeBbKey(key)

      val txn =
        if (txnIn == None) {
          env.txnRead()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      try {
        var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
        if (!outcome) {
          returnVal = None
        } else if (isKeyToValues) {
          val blobsBuf = new ArrayBuffer[Long]()

          outcome = cursor.seek(MDB_FIRST_DUP)
          if (!outcome) {
            throw new RChainException("getLongs(): MDB_FIRST_DUP")
          }
          while (outcome) {
            blobsBuf += cursor.`val`.getLong()
            outcome = cursor.seek(MDB_NEXT_DUP)
          }

          returnVal = if (blobsBuf.isEmpty) {
            None
          } else {
            Some(blobsBuf.toArray)
          }
        } else {
          outcome = cursor.seek(MDB_GET_CURRENT)
          if (!outcome) {
            throw new RChainException("getLongs(): MDB_GET_CURRENT")
          }

          val valueArray = new Array[Long](1)
          valueArray(0) = cursor.`val`.getLong()
          returnVal = Some(valueArray)
        }
      } catch {
        case e: RChainException =>
          Log("getLongs(): " + e)
          returnVal = None
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          txn.close()
        }
      }
      returnVal
    }
  }

  def getFloats[K: Bbable](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Float]] = {

    if (key.isInstanceOf[Key]) {
      getFloats(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getFloats(): key is not primitive or string")
    } else {
      var returnVal: Option[Array[Float]] = None

      var bbKey = Lmdb.makeBbKey(key)

      val txn =
        if (txnIn == None) {
          env.txnRead()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      try {
        var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
        if (!outcome) {
          returnVal = None
        } else {
          if (isKeyToValues) {
            val blobsBuf = new ArrayBuffer[Float]()

            outcome = cursor.seek(MDB_FIRST_DUP)
            if (!outcome) {
              throw new RChainException("getFloats(): MDB_FIRST_DUP")
            }
            while (outcome) {
              blobsBuf += cursor.`val`.getFloat()
              outcome = cursor.seek(MDB_NEXT_DUP)
            }

            returnVal = if (blobsBuf.isEmpty) {
              None
            } else {
              Some(blobsBuf.toArray)
            }
          } else {
            outcome = cursor.seek(MDB_GET_CURRENT)
            if (!outcome) {
              throw new RChainException("getFloats(): MDB_GET_CURRENT")
            }

            val valueArray = new Array[Float](1)
            valueArray(0) = cursor.`val`.getFloat()
            returnVal = Some(valueArray)
          }
        }
      } catch {
        case e: RChainException =>
          Log("getFloats(): " + e)
          returnVal = None
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          txn.close()
        }
      }
      returnVal
    }
  }

  def getDoubles[K: Bbable](
      key: K,
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[Double]] = {

    if (key.isInstanceOf[Key]) {
      getDoubles(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException("getDoubles(): key is not primitive or string")
    } else {
      var returnVal: Option[Array[Double]] = None

      var bbKey = Lmdb.makeBbKey(key)

      val txn =
        if (txnIn == None) {
          env.txnRead()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      try {
        var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
        if (!outcome) {
          returnVal = None
        } else {
          if (isKeyToValues) {
            val blobsBuf = new ArrayBuffer[Double]()

            outcome = cursor.seek(MDB_FIRST_DUP)
            if (!outcome) {
              throw new RChainException("getDoubles(): MDB_FIRST_DUP")
            }
            while (outcome) {
              blobsBuf += cursor.`val`.getDouble()
              outcome = cursor.seek(MDB_NEXT_DUP)
            }

            returnVal = if (blobsBuf.isEmpty) {
              None
            } else {
              Some(blobsBuf.toArray)
            }
          } else {
            outcome = cursor.seek(MDB_GET_CURRENT)
            if (!outcome) {
              throw new RChainException("getDoubles(): MDB_GET_CURRENT")
            }

            val valueArray = new Array[Double](1)
            valueArray(0) = cursor.`val`.getDouble()
            returnVal = Some(valueArray)
          }
        }
      } catch {
        case e: RChainException =>
          Log("getDoubles(): " + e)
          returnVal = None
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          txn.close()
        }
      }
      returnVal
    }
  }

  // delete a key and the value associated with it
  def deleteKey[K: Bbable](key: K,
                           txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) { return false }

    if (key.isInstanceOf[Key]) {
      deleteKey(key.asInstanceOf[Key].term, txnIn)
    } else if (!Lmdb.isStringOrPrimitive(key)) {
      throw new RChainException(
        "deleteKey(): key or value is not a string or primitive")
    } else {
      var deleted = false

      val bbKey = Bb.create(key)
      if (!bbKey.isDefined) {
        deleted = false
      } else {
        val txn =
          if (txnIn == None) {
            env.txnWrite()
          } else {
            txnIn.get
          }
        val cursor = db.openCursor(txn)

        try {
          var outcome = cursor.get(bbKey.get, MDB_SET_KEY)
          if (!outcome) {
            deleted = false
          } else {
            if (isKeyToValues) {
              cursor.delete(MDB_NODUPDATA)
            } else {
              cursor.delete()
            }

            deleted = true
          }
        } catch {
          case e: RChainException =>
            Log("deleteKey(): " + e)
            deleted = false
          case e: Throwable =>
            throw e
        } finally {
          cursor.close()
          if (txnIn == None) {
            if (deleted) {
              txn.commit()
            } else {
              txn.close()
            }
          }
        }
      }
      deleted
    }
  }

  // delete a value associated with a key
  def delete[K: Bbable, V: Bbable](
      key: K,
      value: V,
      txn: Option[Txn[ByteBuffer]] = None): Boolean = {

    if (!isWritable) {
      false
    } else if (key.isInstanceOf[Key]) {
      delete(key.asInstanceOf[Key].term, value, txn)
    } else if (value.isInstanceOf[Key]) {
      delete(key, value.asInstanceOf[Key].term, txn)
    } else {
      if (!Lmdb.isStringOrPrimitive(key)) {
        throw new RChainException("delete(): key is not a string or primitive")
      }
      if (!Lmdb.isStringOrPrimitive(value)) {
        throw new RChainException(
          "delete(): value is not a string or primitive")
      }

      val bbKey = Bb.create(key)
      if (!bbKey.isDefined) {
        throw new RChainException("delete(): key cannot be translated to Bb")
      }

      value match {
        case _: Int => {
          deleteBbValueInt(bbKey.get, Bb.create(value), txn)
        }
        case _: Long => {
          deleteBbValueLong(bbKey.get, Bb.create(value), txn)
        }
        case _: Float => {
          deleteBbValueFloat(bbKey.get, Bb.create(value), txn)
        }
        case _: Double => {
          deleteBbValueDouble(bbKey.get, Bb.create(value), txn)
        }
        case _: String => {
          deleteBbValueString(bbKey.get, Bb.create(value), txn)
        }
        case _ => {
          throw new RChainException(
            "delete(): value is not a string or primitive")
        }
      }
    }
  }

  def deleteBbValueInt(key: ByteBuffer,
                       optionValue: Option[ByteBuffer],
                       txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (!isKeyToValues) {
      false // use deleteKey()
    } else if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueInt(): value is invalid")
    } else {
      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      var deleted = false

      try {
        val bbValue = optionValue.get
        val value = bbValue.getInt

        var outcome = cursor.get(key, MDB_SET_KEY)
        if (!outcome) {
          deleted = false
        } else {
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
        }
      } catch {
        case e: RChainException =>
          Log("deleteBbValueInt(): " + e)
          deleted = false
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          if (deleted) {
            txn.commit()
          } else {
            txn.close()
          }
        }
      }
      deleted
    }
  }

  def deleteBbValueLong(key: ByteBuffer,
                        optionValue: Option[ByteBuffer],
                        txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (!isKeyToValues) {
      false // use deleteKey()
    } else {
      if (!optionValue.isDefined) {
        throw new RChainException("deleteBbValueLong(): value is invalid")
      }

      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      var deleted = false

      try {
        val bbValue = optionValue.get
        val value = bbValue.getLong

        var outcome = cursor.get(key, MDB_SET_KEY)
        if (!outcome) {
          deleted = false
        } else {
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
        }
      } catch {
        case e: RChainException =>
          Log("deleteBbValueLong(): " + e)
          deleted = false
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          if (deleted) {
            txn.commit()
          } else {
            txn.close()
          }
        }
      }
      deleted
    }
  }

  def deleteBbValueFloat(key: ByteBuffer,
                         optionValue: Option[ByteBuffer],
                         txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (!isKeyToValues) {
      false // use deleteKey()
    } else if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueFloat(): value is invalid")
    } else {
      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      var deleted = false

      try {
        val bbValue = optionValue.get
        val value = bbValue.getFloat

        var outcome = cursor.get(key, MDB_SET_KEY)
        if (!outcome) {
          deleted = false
        } else {
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
        }
      } catch {
        case e: RChainException =>
          Log("deleteBbValueFloat(): " + e)
          deleted = false
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          if (deleted) {
            txn.commit()
          } else {
            txn.close()
          }
        }
      }
      deleted
    }
  }

  def deleteBbValueDouble(key: ByteBuffer,
                          optionValue: Option[ByteBuffer],
                          txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (!isKeyToValues) {
      false // use deleteKey()
    } else if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueDouble(): value is invalid")
    } else {
      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      var deleted = false

      try {
        val bbValue = optionValue.get
        val value = bbValue.getDouble

        var outcome = cursor.get(key, MDB_SET_KEY)
        if (!outcome) {
          deleted = false
        } else {
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
        }
      } catch {
        case e: RChainException =>
          Log("deleteBbValueDouble(): " + e)
          deleted = false
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          if (deleted) {
            txn.commit()
          } else {
            txn.close()
          }
        }
      }
      deleted
    }
  }

  def deleteBbValueString(key: ByteBuffer,
                          optionValue: Option[ByteBuffer],
                          txnIn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (!isKeyToValues) {
      false // use deleteKey()
    } else if (!optionValue.isDefined) {
      throw new RChainException("deleteBbValueString(): value is invalid")
    } else {
      val txn =
        if (txnIn == None) {
          env.txnWrite()
        } else {
          txnIn.get
        }
      val cursor = db.openCursor(txn)

      var deleted = false

      try {
        var outcome = cursor.get(key, MDB_SET_KEY)
        if (!outcome) {
          deleted = false
        } else {
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
        }
      } catch {
        case e: RChainException =>
          Log("deleteBbValueString(): " + e)
          deleted = false
        case e: Throwable =>
          throw e
      } finally {
        cursor.close()
        if (txnIn == None) {
          if (deleted) {
            txn.commit()
          } else {
            txn.close()
          }
        }
      }
      deleted
    }
  }

  // update a value associated with a key
  def update[K: Bbable, V: Bbable](
      key: K,
      valueToBeReplaced: V,
      valueReplaceWith: V,
      txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    if (!isWritable) {
      false
    } else if (key.isInstanceOf[Key]) {
      update(key.asInstanceOf[Key].term,
             valueToBeReplaced,
             valueReplaceWith,
             txn)
    } else if (valueToBeReplaced.isInstanceOf[Key] && valueReplaceWith
                 .isInstanceOf[Key]) {
      update(key,
             valueToBeReplaced.asInstanceOf[Key].term,
             valueReplaceWith.asInstanceOf[Key].term,
             txn)
    } else {
      if (!Lmdb.isStringOrPrimitive(key)) {
        throw new RChainException("update(): key is not a string or primitive")
      }
      if (!Lmdb.isStringOrPrimitive(valueToBeReplaced)) {
        throw new RChainException(
          "update(): valueToBeReplaced is not a string or primitive")
      }
      if (!Lmdb.isStringOrPrimitive(valueReplaceWith)) {
        throw new RChainException(
          "update(): valueReplaceWith is not a string or primitive")
      }
      val bbKey = Bb.create(key)
      if (!bbKey.isDefined) {
        throw new RChainException("update(): key cannot be translated to Bb")
      }

      valueToBeReplaced match {
        case _: Int => {
          updateBbValueInt(bbKey.get,
                           valueToBeReplaced.asInstanceOf[Int],
                           valueReplaceWith.asInstanceOf[Int],
                           txn)
        }
        case _: Long => {
          updateBbValueLong(bbKey.get,
                            valueToBeReplaced.asInstanceOf[Long],
                            valueReplaceWith.asInstanceOf[Long],
                            txn)
        }
        case _: Float => {
          updateBbValueFloat(bbKey.get,
                             valueToBeReplaced.asInstanceOf[Float],
                             valueReplaceWith.asInstanceOf[Float],
                             txn)
        }
        case _: Double => {
          updateBbValueDouble(bbKey.get,
                              valueToBeReplaced.asInstanceOf[Double],
                              valueReplaceWith.asInstanceOf[Double],
                              txn)
        }
        case _: String => {
          updateBbValueString(bbKey.get,
                              valueToBeReplaced.asInstanceOf[String],
                              valueReplaceWith.asInstanceOf[String],
                              txn)
        }
        case _ => {
          throw new RChainException(
            "update(): value is not a string or primitive")
        }
      }
    }
  }

  def updateBbValueInt(key: ByteBuffer,
                       valueToBeReplaced: Int,
                       valueReplaceWith: Int,
                       txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    var returnVal = false
    try {
      val bbToBeReplaced = Bb.create(valueToBeReplaced)
      val bbReplaceWith = Bb.create(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueInt(): Bb failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueInt(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getInt, txn)
      }
      if (!outcome) {
        returnVal = false
      } else {
        returnVal = putByteBuffer(key, bbReplaceWith.get, txn)
      }
    } catch {
      case e: RChainException =>
        Log("updateBbValueInt(): " + e)
        returnVal = false
      case e: Throwable =>
        throw e
    }
    returnVal
  }

  def updateBbValueLong(key: ByteBuffer,
                        valueToBeReplaced: Long,
                        valueReplaceWith: Long,
                        txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    var returnVal = false
    try {
      val bbToBeReplaced = Bb.create(valueToBeReplaced)
      val bbReplaceWith = Bb.create(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueLong(): Bb failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueLong(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getLong, txn)
      }
      if (!outcome) {
        returnVal = false
      } else {
        returnVal = putByteBuffer(key, bbReplaceWith.get, txn)
      }
    } catch {
      case e: RChainException =>
        Log("updateBbValueLong(): " + e)
        returnVal = false
      case e: Throwable =>
        throw e
    }
    returnVal
  }

  def updateBbValueFloat(key: ByteBuffer,
                         valueToBeReplaced: Float,
                         valueReplaceWith: Float,
                         txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    var returnVal = false
    try {
      val bbToBeReplaced = Bb.create(valueToBeReplaced)
      val bbReplaceWith = Bb.create(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueFloat(): Bb failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueFloat(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getLong, txn)
      }
      if (!outcome) {
        returnVal = false
      } else {
        returnVal = putByteBuffer(key, bbReplaceWith.get, txn)
      }
    } catch {
      case e: RChainException =>
        Log("updateBbValueFloat(): " + e)
        returnVal = false
      case e: Throwable =>
        throw e
    }
    returnVal
  }

  def updateBbValueDouble(key: ByteBuffer,
                          valueToBeReplaced: Double,
                          valueReplaceWith: Double,
                          txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    var returnVal = false
    try {
      val bbToBeReplaced = Bb.create(valueToBeReplaced)
      val bbReplaceWith = Bb.create(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueDouble(): Bb failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueDouble(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getLong, txn)
      }
      if (!outcome) {
        returnVal = false
      } else {
        returnVal = putByteBuffer(key, bbReplaceWith.get, txn)
      }
    } catch {
      case e: RChainException =>
        Log("updateBbValueDouble(): " + e)
        returnVal = false
      case e: Throwable =>
        throw e
    }
    returnVal
  }

  def updateBbValueString(key: ByteBuffer,
                          valueToBeReplaced: String,
                          valueReplaceWith: String,
                          txn: Option[Txn[ByteBuffer]] = None): Boolean = {
    var returnVal = false
    try {
      val bbToBeReplaced = Bb.create(valueToBeReplaced)
      val bbReplaceWith = Bb.create(valueReplaceWith)
      if (!bbToBeReplaced.isDefined || !bbReplaceWith.isDefined) {
        throw new RChainException("updateBbValueString(): Bb failure")
      }
      var outcome = false
      if (isKeyToValues) {
        outcome = deleteBbValueString(key, bbToBeReplaced, txn)
      } else {
        outcome = deleteKey(key.getLong, txn)
      }
      if (!outcome) {
        returnVal = false
      } else {
        returnVal = putByteBuffer(key, bbReplaceWith.get, txn)
      }
    } catch {
      case e: RChainException =>
        Log("updateBbValueString(): " + e)
        returnVal = false
      case e: Throwable =>
        throw e
    }
    returnVal
  }

  // Return rows that have keys and values that are Ints.
  // This method is just used for methods.
  protected[storage] def rowsIntInt(
      txnIn: Option[Txn[ByteBuffer]] = None): Option[Array[(Int, Int)]] = {
    var array = new ArrayBuffer[(Int, Int)]
    var returnValIsNone = false

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.seek(MDB_FIRST)
      if (!outcome) {
        returnValIsNone = true
      } else {
        while (outcome) {
          val key = cursor.key().getInt
          val value = cursor.`val`.getInt
          array += ((key, value))
          outcome = cursor.seek(MDB_NEXT)
        }
      }
    } catch {
      case e: RChainException =>
        Log("rowsIntInt(): " + e)
        returnValIsNone = true
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) { txn.close() }
    }

    if (returnValIsNone) {
      None
    } else {
      Some(array.toArray)
    }
  }

  // Display the rows that have keys and values that are Ints.
  // This method is just used for methods.
  protected[storage] def displayRowsIntInt(): Unit = {
    val rowsOption = rowsIntInt()
    assert(rowsOption.isDefined)
    val rows = rowsOption.get
    var i = 1
    for (row <- rows) {
      println(s"row $i. key: ${row._1}, value: ${row._2}")
      i += 1
    }
  }

  // Return rows that have keys and values that are Strings.
  // This method is just used for methods.
  protected[storage] def rowsStrStr(txnIn: Option[Txn[ByteBuffer]] = None)
    : Option[Array[(String, String)]] = {
    var array = new ArrayBuffer[(String, String)]
    var returnValIsNone = false

    val txn =
      if (txnIn == None) { env.txnRead() } else { txnIn.get }
    val cursor = db.openCursor(txn)

    try {
      var outcome = cursor.seek(MDB_FIRST)
      if (!outcome) {
        returnValIsNone = true
      } else {
        while (outcome) {
          val key = Bb.bbToStr(cursor.key())
          val value = Bb.bbToStr(cursor.`val`)
          array += ((key, value))
          outcome = cursor.seek(MDB_NEXT)
        }
      }
    } catch {
      case e: RChainException =>
        Log("rowsStrStr(): " + e)
        returnValIsNone = true
      case e: Throwable =>
        throw e
    } finally {
      cursor.close()
      if (txnIn == None) {
        txn.close()
      }
    }

    if (returnValIsNone) {
      None
    } else {
      Some(array.toArray)
    }
  }

  // Display rows that have keys and values that are Strings.
  // This method is just used for methods.
  protected[storage] def displayRowsStrStr(): Unit = {
    val rowsOption = rowsStrStr()
    assert(rowsOption.isDefined)
    val rows = rowsOption.get
    var i = 1
    for (row <- rows) {
      println(s"row $i. key: ${row._1}, value: ${row._2}")
      i += 1
    }
  }

  // Return the memory in use by the lmdb database, excluding
  // other memory that supports the lmdb database
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
    if (debug) {
      // This code is to used to counteract sbt, which leaves
      // around copies of the data.mdb, which can be quite large.
      val paths = java.nio.file.Files
        .walk(Paths.get(baseDir))
        .iterator()
        .asScala
        .filter(file => file.toString.endsWith(".mdb"))
      for (path <- paths) {
        if (path.toString.contains("/target/scala-2.")
            && path.toString.contains("/classes/")) {
          path.toFile.delete()
        }
      }
    }
  }

  // return the size limitation imposed on keys by lmdb
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

  def isKeyToValues: Boolean = { isKeyToValuesIn }
  def isWritable: Boolean = { isWritableIn }

  protected[storage] def Log(s: String, newline: Boolean = true): Unit = {
    if (newline) { println(s) } else { print(s) }
  }
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
    if (t.isInstanceOf[String]) {
      true
    } else {
      isPrimitive(t)
    }
  }

  def isPrimitive[T](t: T): Boolean = {
    var is = false
    if (t.isInstanceOf[Byte]) { is = true } else if (t.isInstanceOf[Boolean]) {
      is = true
    } else if (t.isInstanceOf[Char]) { is = true } else if (t.isInstanceOf[
                                                              Short]) {
      is = true
    } else if (t.isInstanceOf[Int]) { is = true } else if (t.isInstanceOf[
                                                               Long]) {
      is = true
    } else if (t.isInstanceOf[Float]) { is = true } else if (t.isInstanceOf[
                                                               Double]) {
      is = true
    }
    is
  }

  def makeBbKey[K: Bbable](key: K): Option[ByteBuffer] = {
    var bbKey: Option[ByteBuffer] = None
    if (Lmdb.isPrimitive(key)) {
      bbKey = Bb.create(key)
    } else {
      bbKey = Bb.create(key.asInstanceOf[String])
    }
    bbKey
  }
}

// RChainExceptions should not be seen by client.
// Let the user receive Lmdbjava exceptions
class RChainException(msg: String) extends Exception(msg) {}
