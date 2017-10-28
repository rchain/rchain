/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

// TODO: add timings to tests (get and put)

object StorageTests {

  def tests(): Unit = {

    println("StorageTests.tests() begin")

    loadFiles()

    putGetDupSort()

    println("StorageTests.tests() end")
  }

  def putGetDupSort(): Unit = {

    println("putGetDupSort() begin")

    val dirName: Option[String] = Some("storageGetDupSortDb")
    val name: Option[String] = Some("storageGetDupSort")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = dirName
    storConf.name = name
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    storage.deleteFiles()

    try {
      storage.put(1, 2)
      val getInt = storage.getInts(1)
      assert(getInt.get(0) == 2)

      storage.put(3, 4)
      storage.put(3, 5)
      val getInts = storage.getInts(3)
      assert(getInts.get(0) == 4)
      assert(getInts.get(1) == 5)

      storage.put(10L, 20L)
      // val getInt = lmdb.getLongLongs(10L)
      // assert(getInt.get(0) == 2)

      storage.put("a", "b")
      val getStr = storage.getStrings("a")
      val str = getStr.get(0)
      assert(str == "b")

      storage.put("c", "d")
      storage.put("c", "e")
      val getStrs = storage.getStrings("c")
      assert(getStrs.get(0) == "d")
      assert(getStrs.get(1) == "e")
    } catch {
      case e: Throwable => {
        println("putGetDupSort(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("putGetDupSort() end")
  }

  def loadFiles(): Unit = {

    println("loadFile() begin")

    val storeFlat =
      "<root>/src/test/scala/coop/rchain/storage/stores/storeFlat.txt"
    val storeNested =
      "<root>/src/test/scala/coop/rchain/storage/stores/storeNested.txt"
    val storeRecursive =
      "<root>/src/test/scala/coop/rchain/storage/stores/storeRecursive.txt"

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageSimpleTestDb")
    storConf.name = Some("storageSimpleTest")
    assert(storConf.isValid())

    var storage = new Storage(storConf)

    try {

      storage.loadFile(storeFlat, true)
      storage.loadFile(storeNested, true)
      storage.loadFile(storeRecursive, true)

      val itr = storage.uniKeys
      while (itr.hasNext) {
        val k = itr.next
        val strKey = k.term
        val valuesArray = storage.getStrings(strKey)
        if (valuesArray.isDefined) {
          for (strValue <- valuesArray.get) {
            println(s"${k.term} -> $strValue")
          }
        } else {
          println(s"${k.term}: no value found")
        }
      }
    } catch {
      case e: Throwable => {
        println("simpleTest(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("loadFile() end")
  }
}
