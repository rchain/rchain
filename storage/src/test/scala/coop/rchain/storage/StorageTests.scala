/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

import java.io.File
import org.scalatest._

// TODO: add timings to tests (get and put)

class StorageTests extends FlatSpec with Matchers {

  "Storage with keys that are ints and strings are associated with multiple values" should "retrieve the expected values" in {
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

    try {
      storage.put(1, 2)
      val getInt = storage.getInts(1)
      assert(getInt.get.contains(2))

      storage.put(3, 4)
      storage.put(3, 5)
      val getInts = storage.getInts(3)
      assert(getInts.get.contains(4))
      assert(getInts.get.contains(5))

      storage.put("a", "b")
      val getStr = storage.getStrings("a")
      val str = getStr.get(0)
      assert(getStr.get.contains("b"))

      storage.put("c", "d")
      storage.put("c", "e")
      val getStrs = storage.getStrings("c")
      assert(getStrs.get.contains("d"))
      assert(getStrs.get.contains("e"))
    } catch {
      case e: Throwable => {
        fail(e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }
  }

  "Storage" should "load files and retrieve expected values" in {
    val basePath = System.getProperty("user.dir") +
      "/src/test/scala/coop/rchain/storage/stores/"
    val storeFlat = basePath + "storeFlat.txt"
    val storeNested = basePath + "storeNested.txt"
    val storeRecursive = basePath + "storeRecursive.txt"

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageSimpleTestDb")
    storConf.name = Some("storageSimpleTest")
    assert(storConf.isValid())

    var storage = new Storage(storConf)

    try {

      storage.loadFile(storeFlat)
      storage.loadFile(storeNested)
      storage.loadFile(storeRecursive)

      val itr = storage.uniKeys
      while (itr.hasNext) {
        val k = itr.next
        val strKey = k.term
        val valuesArray = storage.getStrings(strKey)
        if (valuesArray.isEmpty) {
          fail(s"${k.term}: no value found")
        }
      }
    } catch {
      case e: Throwable => {
        fail(e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }
  }
}
