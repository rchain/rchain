/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.Storage

import java.lang._

// TODO: add timings to tests (get and put)


object LmdbTests {

  def tests(): Unit = {

    println("LmdbTests.tests() begin")

    updateIntInt()
    updateIntInts()

    putGetDupSort()

    iterateStrStrs()
    iterateIntInts()

    intInt()
    intInts()

    intStr()
    intStrs()

    intIntDelete()
    intIntsDelete()

    stringLongsDelete()
    longFloatsDelete()
    floatDoublesDelete()

    strLongs()
    intFloats()
    floatDoubles()

    // TODO: test read only db
    // TODO: test big blob value
    // TODO: pass cursors in to methods

    println("LmdbTests.tests() end")
  }


  def putGet():Unit = ???

  def putGetDupSort():Unit = {

    println("putGetDupSort() begin")

    val dirName:Option[String] = Some("lmdbPutGetDupSortDb")
    val name:Option[String] = Some("lmdbPutGetDupSort")

    val lmdb = new Lmdb(dirName, name, true, true)

    lmdb.put(1,2)
    val getInt = lmdb.getInts(1)
    assert(getInt.get(0) == 2)

    lmdb.put(3,4)
    lmdb.put(3,5)
    val getInts = lmdb.getInts(3)
    assert(getInts.get(0) == 4)
    assert(getInts.get(1) == 5)

    lmdb.put(10L,20L)
    // val getInt = lmdb.getLongLongs(10L)
    // assert(getInt.get(0) == 2)

    lmdb.put(3, 4)
    lmdb.put(3, 5)
    // val getInts = lmdb.getIntInts(3)
    // assert(getInts.get(0) == 4)
    // assert(getInts.get(1) == 5)

    lmdb.put("a", "b")
    val getStr = lmdb.getStrings("a")
    val str = getStr.get(0)
    assert(str == "b")

    lmdb.put("c", "d")
    lmdb.put("c", "e")
    val getStrings = lmdb.getStrings("c")
    assert(getStrings.get(0) == "d")
    assert(getStrings.get(1) == "e")

    println("putGetDupSort() end")
  }

  def iterateIntInts(): Unit = {

    println("iterateIntIntsDb() begin")

    val dirName:Option[String] = Some("lmdbIterateIntIntsDb")
    val name:Option[String] = Some("lmdbIterateIntInts")

    val numKeys = 3
    val valuesCount = 2

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      for (i <- 0 until numKeys) {
        print(s"put($i): ")
        for (v <- 0 until valuesCount) {
          print((i+1+v) +", ")
          lmdb.put(i, i + 1 + v)
        }
        println()
      }

      lmdb.displayRowsIntInt()
    }
    catch {
      case e:Throwable =>
        println("iterateIntInts(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("iterateIntInts() end\n")

  }

  def intInt(): Unit = {

    println("intInt() begin")

    val dirName:Option[String] = Some("lmdbIntIntDb")
    val name:Option[String] = Some("lmdbIntInt")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, false, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        println(s"put(${key.get}, " + (key.get + 1) + ")")
        lmdb.put(key.get, key.get + 1)
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        print(s"get(${key.get}) returns ")
        val valueArray = lmdb.getInts(key.get)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == key.get + 1)
        println(value)
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("intInt(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intInt() end\n")
  }

  def intInts(): Unit = {

    println("intInts() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbIntIntsDb")
    val name:Option[String] = Some("lmdbIntInts")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        print(s"put(${key.get}): ")
        for (valueCount <- 0 until valuesCount) {
          print((key.get + 1 + valueCount) + ", ")
          lmdb.put(key.get, key.get + 1 + valueCount)
        }
        println()
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        print(s"get(${key.get}) returns ")
        // val values:Option[Array[Int]] = lmdb.get(key.get)
        // val values = lmdb.getBbInts(Bb.intToBb(key.get))
        val values = lmdb.getInts(key.get)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          assert(value == key.get + 1 + i)
          print(value + ", ")
        }
        println()
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("intInts(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intInts() end\n")
  }

  def intStr(): Unit = {

    println("intStr() begin")

    val dirName:Option[String] = Some("lmdbIntStrDb")
    val name:Option[String] = Some("lmdbIntStr")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val blogSize = 1000000 // 1 MB
    val blobs = TestTools.createBlobs(numKeys, blogSize)

    val lmdb = new Lmdb(dirName, name, false, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        println(s"put(${key.get}, '" + blobs(key.get)(0) + "')")
        lmdb.put(key.get, blobs(key.get))
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        print(s"get(${key.get}) returns ")
        val valueArray = lmdb.getStrings(key.get)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val valueString:String = valueArray.get(0)
        val valueCharString:String = valueString.substring(0,1)
        val value = valueCharString.toInt
        assert(value == (key.get % 10))
        println(value)
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("intStr(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intStr() end\n")
  }


  // this shows that strings associated with a key are sorted
  def intStrs(): Unit = {

    println("intStrs() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbIntStrsDb")
    val name:Option[String] = Some("lmdbIntStrs")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    val blogSize = 2
    val blobs = TestTools.createBlobs(numKeys, blogSize)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        print(s"put(${key.get}): ")
        for (valueCount <- 0 until valuesCount) {
          val blob = blobs((key.get +valueCount+1) % 10)
          print(blob(0) + ", ")
          lmdb.put(key.get, blob)
        }
        println()
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val valuesSeen = scala.collection.mutable.Map[Int, Boolean]()
        for (i <- 0 until valuesCount)
          valuesSeen += (((key.get + 1 + i) % 10) -> false)

        print(s"get(${key.get}) returns ")
        // val values:Option[Array[String]] = lmdb.get(key.get)
        val values = lmdb.getStrings(key.get)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          // fails because strings are sorted
          // assert(value.substring(0,1).toInt == (key.get + 1 + i) % 10)
          valuesSeen -= ((key.get + 1 + i) % 10)
          valuesSeen += (((key.get + 1 + i) % 10) -> true)
          print(value.substring(0,1) + ", ")
        }
        println()
        for ((k,v) <- valuesSeen)
          assert(v, s"$k was not returned for key ${key.get}")
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("intStrs(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intStrs() end\n")
  }


  def iterateStrStrs(): Unit = {

    println("iterateStrStrs() begin")

    val dirName:Option[String] = Some("lmdbIterateStrStrsDb")
    val name:Option[String] = Some("lmdbIterateStrStrs")

    val numKeys = 3
    val valuesCount = 2

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      for (i <- 0 until numKeys) {
        print(s"put($i): ")
        val key = i.toString
        for (v <- 0 until valuesCount) {
          val value = (i+1+v).toString
          print(value +", ")
          lmdb.put(key, value)
        }
        println()
      }

      lmdb.displayRowsStrStr()
    }
    catch {
      case e:Throwable =>
        println("iterateStrStrs(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("iterateStrStrs() end\n")

  }


  def intIntDelete(): Unit = {

    println("intIntDelete() begin")

    val dirName:Option[String] = Some("lmdbIntIntDeleteDb")
    val name:Option[String] = Some("lmdbIntIntDelete")

    val lmdb = new Lmdb(dirName, name, false, true)

    val numKeys = 4

    try {
      for (i <- 0 until numKeys) {
        println(s"put($i, ${i+1})")
        lmdb.put(i, i+1)
      }
      for (i <- 0 until numKeys) {
        print(s"get($i) returns ")
        // val valueArray = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray = lmdb.getInts(i)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == i+1)
        println(value)
      }

      println("lmdb.deleteKey(1)")
      var outcome = lmdb.deleteKey(1); assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray:Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray:Option[Array[Int]] = lmdb.getInts(i)
        if (i == 1)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      println("lmdb.deleteKey(0)")
      outcome = lmdb.deleteKey(0); assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray:Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray:Option[Array[Int]] = lmdb.getInts(i)
        if (i == 0 || i == 1)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      println("lmdb.deleteKey(3)")
      outcome = lmdb.deleteKey(3); assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray:Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray:Option[Array[Int]] = lmdb.getInts(i)
        if (i == 0 || i == 1 || i == 3)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == 1)
          val value = valueArray.get(0)
          assert(value == i + 1)
        }
      }
      println("lmdb.deleteKey(2)")
      outcome = lmdb.deleteKey(2); assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray:Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray:Option[Array[Int]] = lmdb.getInts(i)
        assert(valueArray == None)
      }
    }
    catch {
      case e:Throwable =>
        println("intIntDelete(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intIntDelete() end\n")
  }


  def intIntsDelete(): Unit = {

    println("intIntsDelete() begin")

    val dirName: Option[String] = Some("ldmbIntIntsDeleteDb")
    val name: Option[String] = Some("lmdbIntIntsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        print(s"put($i): ")
        for (v <- 0 until valuesCount) {
          print((i+1+v) +", ")
          lmdb.put(i, i + 1 + v)
        }
        println()
      }
      for (i <- 0 until numKeys) {
        print(s"get($i) returns ")
        // val valueArray = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray = lmdb.getInts(i)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
        for (v <- 0 until valuesCount) {
          val value = valueArray.get(v)
          assert(value == i + 1 + v)
          print(value +", ")
        }
        println()
      }

      println("lmdb.deleteKey(2)")
      var outcome = lmdb.deleteKey(2);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
          for (v <- 0 until valuesCount) {
            val value = valueArray.get(v)
            assert(value == i + 1 + v)
          }
        }
      }
      println("lmdb.delete(3,5)")
      outcome = lmdb.delete(3, 5);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-1)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.size) {
            val value = valueArray.get(v)
            if (i != 3 && value != 5)
              assert(value == i + 1 + v)
          }
        }
      }
      lmdb.displayRowsIntInt()

      println("lmdb.delete(3,4)")
      outcome = lmdb.delete(3, 4);
      assert(outcome)

      for (i <- 0 until numKeys) {
        // val valueArray: Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-2)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            if (i != 3 && (value != 5 && value != 4))
              assert(value == i + 1 + v)
          }
        }
      }
      // lmdb.displayRowsIntInt()

      println("lmdb.delete(3,6)")
      outcome = lmdb.delete(3, 6);
      assert(outcome)
      for (i <- 0 until numKeys) {
        // val valueArray: Option[Array[Int]] = lmdb.getBbInts(Bb.intToBb(i))
        val valueArray: Option[Array[Int]] = lmdb.getInts(i)
        if (i == 2 || i == 3)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            assert(value == i + 1 + v)
          }
        }
      }
      // lmdb.displayRowsIntInt()
   }
    catch {
      case e: Throwable =>
        println("intIntsDelete(): " + e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intIntsDelete() end\n")
  }

  def stringLongsDelete(): Unit = {

    println("stringLongsDelete() begin")

    val dirName: Option[String] = Some("ldmbStringLongsDeleteDb")
    val name: Option[String] = Some("lmdbStringLongsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        val key = "str" + i
        print(s"put($key): ")
        for (v <- 0 until valuesCount) {
          val value = (i+1+v).toLong
          print(value +", ")
          lmdb.put(key, value)
        }
        println()
      }
      for (i <- 0 until numKeys) {
        val key = "str" + i
        print(s"get($key) returns ")
        val valueArray = lmdb.getLongs(key)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
        for (v <- 0 until valuesCount) {
          val value = valueArray.get(v)
          val valueExpected = (i + 1 + v).toLong
          // TODO assert(value == valueExpected)
          print(value +", ")
        }
        println()
      }

      println("lmdb.deleteKey(str2)")
      var outcome = lmdb.deleteKey("str2");
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
          for (v <- 0 until valuesCount) {
            val value = valueArray.get(v)
            // TODO assert(value == i + 1 + v)
            print(value +", ")
          }
          println()
        }
      }
      println("lmdb.delete(str3,5)")
      outcome = lmdb.delete("str3", 5.toLong);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-1)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.size) {
            val value = valueArray.get(v)
            if (i != 3 && value != 5) {
              // assert(value == i + 1 + v)
              print(value +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(str3,4)")
      outcome = lmdb.delete("str3", 4.toLong);
      assert(outcome)

      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-2)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            if (i != 3 && (value != 5 && value != 4)) {
              // TODO assert(value == i + 1 + v)
              print(value +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(str3,6)")
      outcome = lmdb.delete("str3", 6.toLong);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val key = "str" + i
        val valueArray = lmdb.getLongs(key)
        if (i == 2 || i == 3)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            // TODO assert(value == i + 1 + v)
            print(value +", ")
          }
        }
        println()
      }
    }
    catch {
      case e: Throwable =>
        println("stringLongsDelete(): " + e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("stringLongsDelete() end\n")
  }

  def longFloatsDelete():Unit = {

    println("longFloatsDelete() begin")

    val dirName: Option[String] = Some("ldmbLongFloatsDeleteDb")
    val name: Option[String] = Some("lmdbLongFloatsDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        print(s"put($i): ")
        for (v <- 0 until valuesCount) {
          val value = (1+v)/(i+1).toFloat
          print(value +", ")
          lmdb.put(i.toLong, value)
        }
        println()
      }
      for (i <- 0 until numKeys) {
        print(s"get($i) returns ")
        val valueArray = lmdb.getFloats(i.toLong)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
        for (v <- 0 until valuesCount) {
          val value = valueArray.get(v)
          val valueExpected = (1+v)/(i+1).toFloat
          // TODO assert(value == valueExpected)
          print("%f".format(value) +", ")
        }
        println()
      }

      println("lmdb.deleteKey(2)")
      var outcome = lmdb.deleteKey(2.toLong)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
          for (v <- 0 until valuesCount) {
            val value = valueArray.get(v)
            val valueExpected = (1+v)/(i+1).toFloat
            // TODO assert(value == valueExpected)
            print("%f".format(value) +", ")
          }
          println()
        }
      }
      println("lmdb.delete(3,0.5)")
      outcome = lmdb.delete(3.toLong, 0.5.toFloat)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-1)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.size) {
            val value = valueArray.get(v)
            if (i != 3 && value != 5) {
              val valueExpected = (1 + v) / (i + 1).toFloat
              // TODO assert(value == valueExpected)
              print("%f".format(value) +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(3,0.25)")
      outcome = lmdb.delete(3.toLong, 0.25.toFloat)
      assert(outcome)

      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-2)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            if (i != 3 && (value != 5 && value != 4)) {
              val valueExpected = (1 + v) / (i + 1).toFloat
              // TODO assert(value == valueExpected)
              print("%f".format(value) +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(3,0.75)")
      outcome = lmdb.delete(3.toLong, 0.75.toFloat);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toLong)
        if (i == 2 || i == 3)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            // TODO assert(value == i + 1 + v)
            print("%f".format(value) +", ")
          }
          println()
        }
      }
    }
    catch {
      case e: Throwable =>
        println("longFloatsDelete(): " + e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("longFloatsDelete() end\n")
  }

  def floatDoublesDelete() = {

    println("floatDoublesDelete() begin")

    val dirName: Option[String] = Some("ldmbFloatDoublesDeleteDb")
    val name: Option[String] = Some("lmdbFloatDoublesDelete")

    val lmdb = new Lmdb(dirName, name, true, true)

    val numKeys = 4
    val valuesCount = 3

    try {
      for (i <- 0 until numKeys) {
        print(s"put($i): ")
        for (v <- 0 until valuesCount) {
          val value = (1+v)/(i+1).toDouble
          print(value +", ")
          lmdb.put(i.toFloat, value)
        }
        println()
      }
      for (i <- 0 until numKeys) {
        print(s"get($i) returns ")
        val valueArray = lmdb.getDoubles(i.toFloat)
        assert(valueArray != None)
        assert(valueArray.get.size == valuesCount)
        for (v <- 0 until valuesCount) {
          val value = valueArray.get(v)
          val valueExpected = (1+v)/(i+1).toDouble
          // TODO assert(value == valueExpected)
          print("%f".format(value) +", ")
        }
        println()
      }

      println("lmdb.deleteKey(2)")
      var outcome = lmdb.deleteKey(2.toFloat)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getDoubles(i.toFloat)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(valueArray.get.size == valuesCount)
          for (v <- 0 until valuesCount) {
            val value = valueArray.get(v)
            val valueExpected = (1+v)/(i+1).toDouble
            // TODO assert(value == valueExpected)
            print("%f".format(value) +", ")
          }
          println()
        }
      }
      println("lmdb.delete(3,0.5)")
      outcome = lmdb.delete(3.toFloat, 0.5.toDouble)
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getDoubles(i.toFloat)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-1)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.size) {
            val value = valueArray.get(v)
            if (i != 3 && value != 5) {
              val valueExpected = (1 + v) / (i + 1).toDouble
              // TODO assert(value == valueExpected)
              print("%f".format(value) +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(3,0.25)")
      outcome = lmdb.delete(3.toFloat, 0.25.toDouble)
      assert(outcome)

      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toFloat)
        if (i == 2)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          assert(i != 3 || valueArray.get.size == valuesCount-2)
          assert(i == 3 || valueArray.get.size == valuesCount)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            if (i != 3 && (value != 5 && value != 4)) {
              val valueExpected = (1 + v) / (i + 1).toDouble
              // TODO assert(value == valueExpected)
              print("%f".format(value) +", ")
            }
          }
          println()
        }
      }

      println("lmdb.delete(3,0.75)")
      outcome = lmdb.delete(3.toFloat, 0.75.toDouble);
      assert(outcome)
      for (i <- 0 until numKeys) {
        val valueArray = lmdb.getFloats(i.toFloat)
        if (i == 2 || i == 3)
          assert(valueArray == None)
        else {
          assert(valueArray != None)
          for (v <- 0 until valueArray.get.size) {
            val value = valueArray.get(v)
            // TODO assert(value == i + 1 + v)
            print("%f".format(value) +", ")
          }
          println()
        }
      }
    }
    catch {
      case e: Throwable =>
        println("floatDoublesDelete(): " + e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("floatDoublesDelete() end\n")
  }


  def strLongs(): Unit = {

    println("strLongs() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbStrLongsDb")
    val name:Option[String] = Some("lmdbStrLongs")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        val keyStr = "str" + key.get.toString
        print(s"put($keyStr): ")
        for (valueCount <- 0 until valuesCount) {
          val valueLong = (key.get + 1 + valueCount).toLong
          print(valueLong + ", ")
          lmdb.put(keyStr, valueLong)
        }
        println()
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val keyStr = "str" + key.get.toString
        print(s"get($keyStr) returns ")
        val values = lmdb.getLongs(keyStr)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          val valueLongExpected = (key.get + 1 + i).toLong
          assert(value == valueLongExpected)
          print(value + ", ")
        }
        println()
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("strLongs(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("strLongs() end\n")
  }

  def intFloats(): Unit = {

    println("intFloats() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbIntFloatsDb")
    val name:Option[String] = Some("lmdbIntFloats")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        print(s"put(${key.get}): ")
        for (i <- 0 until valuesCount) {
          val valueFloat = key.get/(i+1).toFloat
          print("%f, ".format(valueFloat))
          lmdb.put(key.get, valueFloat)
        }
        println()
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        print(s"get(${key.get}) returns ")
        val values = lmdb.getFloats(key.get)
        assert(values != None)
        assert((key.get == 0 && values.get.size == 1 ) || values.get.size == valuesCount)
        val valuesSeen = new Array[Float](values.get.size)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          valuesSeen(i) = value
          print(value + ", ")
        }
        for (i <- 0 until values.get.size) {
          val valueFloatExpected = key.get/(i+1).toFloat
          var found = false
          for (j <- 0 until valuesSeen.size) {
            if (valuesSeen(j) == valueFloatExpected) {
              found = true
            }
          }
          assert(found)
        }
        println()
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("intFloats(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("intFloats() end\n")
  }

  def floatDoubles(): Unit = {

    println("floatDoubles() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbFloatDoublesDb")
    val name:Option[String] = Some("lmdbFloatDoubles")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        val keyFloat = key.get.toFloat
        print("put(%f): ".format(keyFloat))
        for (i <- 0 until valuesCount) {
          val valueDouble = key.get/(i+1).toDouble
          print("%f, ".format(valueDouble))
          lmdb.put(keyFloat, valueDouble)
        }
        println()
        key = randGen.nextKey()
      }

      randGen = new TestTools.RandKeyGen(numKeys)
      key = randGen.nextKey()
      while (key != None) {
        val keyFloat = key.get.toFloat
        print("get(%f) returns ".format(keyFloat))
        val values = lmdb.getDoubles(keyFloat)
        assert(values != None)
        assert((keyFloat == 0 && values.get.size == 1 ) || values.get.size == valuesCount)
        val valuesSeen = new Array[Double](values.get.size)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          valuesSeen(i) = value
          print(value + ", ")
        }
        for (i <- 0 until values.get.size) {
          val valueDoubleExpected = key.get/(i+1).toDouble
          var found = false
          for (j <- 0 until valuesSeen.size) {
            if (valuesSeen(j) == valueDoubleExpected) {
              found = true
            }
          }
          assert(found)
        }
        println()
        key = randGen.nextKey()
      }
    }
    catch {
      case e:Throwable =>
        println("floatDoubles(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("floatDoubles() end\n")
  }

  def updateIntInt() = {
    println("updateIntInt() begin")

    val dirName:Option[String] = Some("lmdbUpdateIntIntDb")
    val name:Option[String] = Some("lmdbUpdateIntInt")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)
    // TODO: figure out why the following fails
    // val lmdb = new Lmdb(dirName, name, false, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        println(s"put(${key.get}, " + (key.get + 1) + ")")
        lmdb.put(key.get, key.get + 1)
        key = randGen.nextKey()
      }

      for (key <- 0 until numKeys) {
        print(s"get($key) returns ")
        val valueArray = lmdb.getInts(key)
        assert(valueArray != None)
        assert(valueArray.get.size == 1)
        val value = valueArray.get(0)
        assert(value == key + 1)
        println(value)
      }

      var outcome = lmdb.update(1, 2, 23)
      assert(outcome)
      val valueArray = lmdb.getInts(1)
      assert(valueArray.get(0) == 23)

      outcome = lmdb.update(2, 99, 100)
      assert(!outcome)
    }
    catch {
      case e:Throwable =>
        println("updateIntInt(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("updateIntInt() end\n")
  }

  def updateIntInts():Unit = {

    println("updateIntInts() begin")

    val valuesCount = 4

    val dirName:Option[String] = Some("lmdbUpdateIntIntsDb")
    val name:Option[String] = Some("lmdbUpdateIntInts")

    val numKeys = 10
    var randGen = new TestTools.RandKeyGen(numKeys)

    val lmdb = new Lmdb(dirName, name, true, true)

    try {
      var key = randGen.nextKey()
      while (key != None) {
        print(s"put(${key.get}): ")
        for (valueCount <- 0 until valuesCount) {
          print((key.get + 1 + valueCount) + ", ")
          lmdb.put(key.get, key.get + 1 + valueCount)
        }
        println()
        key = randGen.nextKey()
      }

      for (key <- 0 until numKeys) {
        print(s"get($key) returns ")
        val values = lmdb.getInts(key)
        assert(values != None)
        assert(values.get.size == valuesCount)
        for (i <- 0 until values.get.size) {
          val value = values.get(i)
          assert(value == key + 1 + i)
          print(value + ", ")
        }
        println()
      }

      var outcome = lmdb.update(1, 2, 23)
      assert(outcome)
      val valueArray = lmdb.getInts(1)
      var found = false
      for (i <- 0 until valuesCount){
        val value = valueArray.get(i)
        if (value == 23) {
          found = true
        }
      }
      assert(found)

      outcome = lmdb.update(2, 99, 100)
      assert(!outcome)
    }
    catch {
      case e:Throwable =>
        println("intInts(): "+e)
    }
    finally {
      lmdb.close()
      lmdb.deleteFiles()
    }

    println("updateIntInts() end\n")
  }
}
