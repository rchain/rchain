/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable._
import scala.io.Source

// KeyValueStore maps Keys as Strings to ValueLists

class KeyValueStore {
  val keyValueStore = SortedMap[Key, ValueList]()

  def add(key: Key, value: String): Unit = {
    if (keyValueStore.contains(key))
      keyValueStore(key).add(value)
    else {
      keyValueStore(key) = new ValueList
      keyValueStore(key).add(value)
    }
  }

  def remove(key: Key): Unit = {
    if (keyValueStore.contains(key))
      keyValueStore.remove(key)
    else
      throw new Exception("KeyValueStore.Remove(" + key.term + ") not found")
  }

  def get(key: Key): ValueList = {
    if (keyValueStore.contains(key))
      return keyValueStore(key)

    throw new Exception("KeyValueStore.Get(" + key.term + ") not found")
  }

  // This method is motivated by compile problems I am having
  // that involve catching the exception thrown in get()
  def getOrNull(key: Key): ValueList = {
    if (keyValueStore.contains(key))
      return keyValueStore(key)

    return null
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
        val (keyStr, value) = line.splitAt(line.lastIndexOf(' '))
        val key = new Key(keyStr.trim)
        add(key, value.trim)
      }
    }
  }

  def display: Unit = {
    var i = 1
    for ((k, v) <- keyValueStore) {
      print(i + ". " + k.term + " -> ")
      v.display
      i += 1
    }
  }
}
