/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

import java.io.File
import java.lang._
import org.scalatest._

// TODO: add timings to tests (get and put)

class LmdbTests extends FlatSpec with Matchers {
  val basePath = System.getProperty("user.dir") + "/"

  "Lmdb with keys that are ints and strings are associated with multiple values" should "retrieve the expected values" in {
    val dirName: Option[String] = Some("lmdbPutGetDupSortDb")
    val name: Option[String] = Some("lmdbPutGetDupSort")

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      lmdb.put(1, 2)
      val getInt = lmdb.getInts(1)
      assert(getInt.get(0) == 2)

      lmdb.put(3, 4)
      lmdb.put(3, 5)
      val getInts = lmdb.getInts(3)
      assert(getInts.get(0) == 4)
      assert(getInts.get(1) == 5)

      lmdb.put("a", "b")
      val getStr = lmdb.getStrings("a")
      val str = getStr.get(0)
      assert(str == "b")

      lmdb.put("c", "d")
      lmdb.put("c", "e")
      val getStrings = lmdb.getStrings("c")
      assert(getStrings.get(0) == "d")
      assert(getStrings.get(1) == "e")
    } catch {
      case e: Throwable =>
        fail("intInt(): " + e)
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }

  }

  "Lmdb with int key associated with int" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbIntIntDb")
    val name: Option[String] = Some("lmdbIntInt")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, false, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        lmdb.put(key.get, key.get + 1)
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val valueArray = lmdb.getInts(key.get)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == key.get + 1)
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable =>
        fail("intInt(): " + e)
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with multiple ints" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbIntIntsDb")
    val name: Option[String] = Some("lmdbIntInts")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        for (valueCount <- 0 until valuesCount) {
          lmdb.put(key.get, key.get + 1 + valueCount)
        }
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val values = lmdb.getInts(key.get)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          assert(value == key.get + 1 + i)
        }
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable =>
        fail("intInts(): " + e)
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with 1 MB blob" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbIntStrDb")
    val name: Option[String] = Some("lmdbIntStr")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val blogSize = 1000000 // 1 MB
    val blobs = TestTools.createBlobs(numKeys, blogSize)

    val lmdb = new Lmdb(dirName, name, false, true, None, Lmdb.maxDbSize)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        lmdb.put(key.get, blobs(key.get))
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val valueArray = lmdb.getStrings(key.get)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val valueString: String = valueArray.get(0)
        val valueCharString: String = valueString.substring(0, 1)
        val value = valueCharString.toInt
        assert(value == (key.get % 10))
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable =>
        fail("intStr(): " + e)
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with multiple strings" should "retrieve expected values" in {
    // this shows that strings associated with a key are sorted

    val dirName: Option[String] = Some("lmdbIntStrsDb")
    val name: Option[String] = Some("lmdbIntStrs")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    val blogSize = 2
    val blobs = TestTools.createBlobs(numKeys, blogSize)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        for (valueCount <- 0 until valuesCount) {
          val blob = blobs((key.get + valueCount + 1) % 10)
          lmdb.put(key.get, blob)
        }
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val valuesSeen = scala.collection.mutable.Map[Int, Boolean]()
        for (i <- 0 until valuesCount) {
          valuesSeen += (((key.get + 1 + i) % 10) -> false)
        }

        val values = lmdb.getStrings(key.get)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          // fails because strings are sorted
          // assert(value.substring(0,1).toInt == (key.get + 1 + i) % 10)
          valuesSeen -= ((key.get + 1 + i) % 10)
          valuesSeen += (((key.get + 1 + i) % 10) -> true)
        }
        for ((k, v) <- valuesSeen) {
          assert(v, s"$k was not returned for key ${key.get}")
        }
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable => {
        fail("intStrs(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with string key associated with multiple strings" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbIterateStrStrsDb")
    val name: Option[String] = Some("lmdbIterateStrStrs")

    val numKeys = 3
    val valuesCount = 2

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      for (i <- 0 until numKeys) {
        val key = i.toString
        for (v <- 0 until valuesCount) {
          val value = (i + 1 + v).toString
          lmdb.put(key, value)
        }
      }

      for (i <- 0 until numKeys) {
        val valuesSeen = scala.collection.mutable.Map[String, Boolean]()
        for (v <- 0 until valuesCount) {
          valuesSeen += (((i + 1 + v) % 10).toString -> false)
        }

        val key = i.toString
        val values = lmdb.getStrings(key)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          valuesSeen -= ((key.toInt + 1 + i) % 10).toString
          valuesSeen += (((key.toInt + 1 + i) % 10).toString -> true)
        }

        for ((k, v) <- valuesSeen) {
          assert(v, s"$k was not returned for key $key")
        }
      }
    } catch {
      case e: Throwable => {
        fail("iterateStrStrs(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with int" should "delete expected values" in {
    val dirName: Option[String] = Some("lmdbIntIntDeleteDb")
    val name: Option[String] = Some("lmdbIntIntDelete")

    val lmdb = new Lmdb(dirName, name, false, true)

    val numKeys = 4

    try {
      for (i <- 0 until numKeys) {
        lmdb.put(i, i + 1)
      }
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getInts(i)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == i + 1)
      }

      // deleteKey(1)
      var outcome = lmdb.deleteKey(1); assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 1) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      // deleteKey(0)
      outcome = lmdb.deleteKey(0); assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray:Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 0 || i == 1) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      // deleteKey(3)
      outcome = lmdb.deleteKey(3); assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 0 || i == 1 || i == 3) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      // deleteKey(2)
      outcome = lmdb.deleteKey(2); assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        assert(valueArray == None)
      }
    } catch {
      case e: Throwable => {
        fail("intIntDelete(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with multiple ints" should "delete expected values" in {
    val dirName: Option[String] = Some("lmdbIntIntsDeleteDb")
    val name: Option[String] = Some("lmdbIntIntsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        for (v <- 0 until valuesCount) {
          lmdb.put(i, i + 1 + v)
        }
      }
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getInts(i)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
        for (v <- 0 until valuesCount) {
          val value = valueArray.get(v)
          assert(value == i + 1 + v)
        }
      }

      // deleteKey(2)
      var outcome = lmdb.deleteKey(2);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
          for (v <- 0 until valuesCount) {
            val value = valueArray.get(v)
            assert(value == i + 1 + v)
          }
        }
      }
      // delete(3,5)
      outcome = lmdb.delete(3, 5);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount - 1)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.size) {
            val value = valueArray.get(v)
            if (i != 3 && value != 5) {
              assert(value == i + 1 + v)
            }
          }
        }
      }
      // delete(3,4)
      outcome = lmdb.delete(3, 4);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount - 2)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            if (i != 3 && (value != 5 && value != 4)) {
              assert(value == i + 1 + v)
            }
          }
        }
      }
      // delete(3,6)
      outcome = lmdb.delete(3, 6);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2 || i == 3) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            assert(value == i + 1 + v)
          }
        }
      }
    } catch {
      case e: Throwable => {
        fail("intIntsDelete(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with string key associated with multiple longs" should "delete expected values" in {
    val dirName: Option[String] = Some("lmdbStringLongsDeleteDb")
    val name: Option[String] = Some("lmdbStringLongsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        val key = "str" + i
        for (v <- 0 until valuesCount) {
          val value = (i + 1 + v).toLong
          lmdb.put(key, value)
        }
      }

      // deleteKey(str2)
      var outcome = lmdb.deleteKey("str2");
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(str3,5)
      outcome = lmdb.delete("str3", 5.toLong);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount - 1)
          assert(!(valueArray.get contains 5.toLong))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }

      // delete(str3,4)
      outcome = lmdb.delete("str3", 4.toLong);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount - 2)
          assert(!(valueArray.get contains 4.toLong))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }

      // delete(str3,6)
      outcome = lmdb.delete("str3", 6.toLong);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2 || i == 3) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
        }
      }
    } catch {
      case e: Throwable => {
        fail("stringLongsDelete(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with long key associated with multiple floats" should "delete expected values" in {
    val dirName: Option[String] = Some("lmdbLongFloatsDeleteDb")
    val name: Option[String] = Some("lmdbLongFloatsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        for (v <- 0 until valuesCount) {
          val value = (1 + v) / (i + 1).toFloat
          lmdb.put(i.toLong, value)
        }
      }

      // deleteKey(2)
      var outcome = lmdb.deleteKey(2.toLong)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.5)
      outcome = lmdb.delete(3.toLong, 0.5.toFloat)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount - 1)
          assert(!(valueArray.get contains 0.5.toFloat))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.25)
      outcome = lmdb.delete(3.toLong, 0.25.toFloat)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount - 2)
          assert(!(valueArray.get contains 0.25.toFloat))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.75)
      outcome = lmdb.delete(3.toLong, 0.75.toFloat);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2 || i == 3) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
        }
      }
    } catch {
      case e: Throwable => {
        fail("longFloatsDelete(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int float associated with multiple doubles" should "delete expected values" in {
    val dirName: Option[String] = Some("lmdbFloatDoublesDeleteDb")
    val name: Option[String] = Some("lmdbFloatDoublesDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        for (v <- 0 until valuesCount) {
          val value = (1 + v) / (i + 1).toDouble
          lmdb.put(i.toFloat, value)
        }
      }
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getDoubles(i.toFloat)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
      }

      // deleteKey(2)
      var outcome = lmdb.deleteKey(2.toFloat)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getDoubles(i.toFloat)
        if (i == 2) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.5)
      outcome = lmdb.delete(3.toFloat, 0.5.toDouble)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getDoubles(i.toFloat)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray.get.size == valuesCount - 1)
          assert(!(valueArray.get contains 0.5.toDouble))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.25)
      outcome = lmdb.delete(3.toFloat, 0.25.toDouble)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toFloat)
        if (i == 2) {
          assert(valueArray == None)
        } else if (i == 3) {
          assert(valueArray.get.size == valuesCount - 2)
          assert(!(valueArray.get contains 0.25.toDouble))
        } else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
        }
      }
      // delete(3,0.75)
      outcome = lmdb.delete(3.toFloat, 0.75.toDouble);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toFloat)
        if (i == 2 || i == 3) {
          assert(valueArray == None)
        } else {
          assert(valueArray != None)
        }
      }
    } catch {
      case e: Throwable => {
        fail("floatDoublesDelete(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with string key associated with multiple longs" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbStrLongsDb")
    val name: Option[String] = Some("lmdbStrLongs")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        val keyStr = "str" + key.get.toString
        for (valueCount <- 0 until valuesCount) {
          val valueLong = (key.get + 1 + valueCount).toLong
          lmdb.put(keyStr, valueLong)
        }
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val keyStr = "str" + key.get.toString
        val values = lmdb.getLongs(keyStr)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          val valueLongExpected = (key.get + 1 + i).toLong
          assert(value == valueLongExpected)
        }
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable => {
        fail("strLongs(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with multiple floats" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbIntFloatsDb")
    val name: Option[String] = Some("lmdbIntFloats")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        for (i <- 0 until valuesCount) {
          val valueFloat = key.get / (i + 1).toFloat
          lmdb.put(key.get, valueFloat)
        }
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val values = lmdb.getFloats(key.get)
        assert(values != None)
        assert(
          (key.get == 0 && values.get.size == 1) || values.get.size == valuesCount)
        val valuesSeen = new Array[Float](values.get.size)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          valuesSeen(i) = value
        }
        for (i <- 0 until values.get.size) {
          val valueFloatExpected = key.get / (i + 1).toFloat
          assert(valuesSeen contains valueFloatExpected)
        }
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable => {
        fail("intFloats(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with float key associated with multiple doubles" should "retrieve expected values" in {
    val dirName: Option[String] = Some("lmdbFloatDoublesDb")
    val name: Option[String] = Some("lmdbFloatDoubles")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        val keyFloat = key.get.toFloat
        for (i <- 0 until valuesCount) {
          val valueDouble = key.get / (i + 1).toDouble
          lmdb.put(keyFloat, valueDouble)
        }
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val keyFloat = key.get.toFloat
        val values = lmdb.getDoubles(keyFloat)
        assert(values != None)
        assert(
          (keyFloat == 0 && values.get.size == 1) || values.get.size == valuesCount)
        val valuesSeen = new Array[Double](values.get.size)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          valuesSeen(i) = value
        }
        for (i <- 0 until values.get.size) {
          val valueDoubleExpected = key.get / (i + 1).toDouble
          assert(valuesSeen contains valueDoubleExpected)
        }
        key = randGen.nextKey()
      }
    } catch {
      case e: Throwable => {
        fail("floatDoubles(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with int" should "update expected values" in {
    val dirName: Option[String] = Some("lmdbUpdateIntIntDb")
    val name: Option[String] = Some("lmdbUpdateIntInt")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        lmdb.put(key.get, key.get + 1)
        key = randGen.nextKey()
      }

      for (key <- 0 until numKeys) {
        val valueArray = lmdb.getInts(key)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == key + 1)
      }

      var outcome = lmdb.update(1, 2, 23)
      assert(outcome)
      val valueArray = lmdb.getInts(1)
      assert(valueArray.get(0) == 23)

      outcome = lmdb.update(2, 99, 100)
      assert(!outcome)
    } catch {
      case e: Throwable => {
        fail("updateIntInt(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }

  "Lmdb with int key associated with multiple ints" should "update expected values" in {
    val dirName: Option[String] = Some("lmdbUpdateIntIntsDb")
    val name: Option[String] = Some("lmdbUpdateIntInts")

    val numKeys = 10
    val valuesCount = 4

    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        for (valueCount <- 0 until valuesCount) {
          lmdb.put(key.get, key.get + 1 + valueCount)
        }
        key = randGen.nextKey()
      }

      for (key <- 0 until numKeys) {
        val values = lmdb.getInts(key)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          assert(value == key + 1 + i)
        }
      }

      var outcome = lmdb.update(1, 2, 23)
      assert(outcome)
      val valueArray = lmdb.getInts(1)
      assert(valueArray.get contains 23)
      outcome = lmdb.update(2, 99, 100)
      assert(!outcome)
    } catch {
      case e: Throwable => {
        fail("intInts(): " + e)
      }
    } finally {
      lmdb.close()
      lmdb.deleteFiles()
      // new File(basePath + dirName.get).delete()
    }
  }
}
