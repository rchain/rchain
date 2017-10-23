/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package scala.coop.rchain.KeyValueStore

import scala.collection.mutable.SortedMap
import scala.io.Source

// KeyValueStore maps Keys as Strings to ValueLists

class KeyValueStore {
  protected[KeyValueStore] var keyValueStore =
    collection.mutable.SortedMap[Key, ValueList]()

  def add(key: Key, value: Value): Unit = {
    if (keyValueStore.contains(key)) {
      keyValueStore(key).add(value)
    } else {
      keyValueStore(key) = new ValueList
      keyValueStore(key).add(value)
    }
  }

  def add(key: Key, valueList: ValueList): Unit = {
    for (value <- valueList.iterator) {
      add(key, value)
    }
  }

  def add(key: Key, valueList: Array[Value]): Unit = {
    for (value <- valueList) {
      this.add(key, value)
    }
  }

  def get(key: Key): ValueList = {
    if (keyValueStore.contains(key)) {
      return keyValueStore(key)
    }
    null
  }

  def remove(key: Key, value: Value = null): Boolean = {
    if (keyValueStore.contains(key)) {
      if (value == null) {
        keyValueStore = keyValueStore - key
        return true
      } else {
        return keyValueStore(key).remove(value)
      }
    }
    false
  }

  def iterator: Iterator[Key] = { keyValueStore.keys.toIterator }

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
        val key = new Key(keyStr)
        add(key, new Value(value))
      }
    }
  }

  def display: Unit = {
    var i = 1
    for ((k, v) <- keyValueStore) {
      print(i + ". " + k.term + " -> ")
      v.display; println
      i += 1
    }
  }
}
