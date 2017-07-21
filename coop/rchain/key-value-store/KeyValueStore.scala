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

class KeyValueStore
{
  // TO DO: Need to define an Ordering on Key
  // val keyValueStore = SortedMap[Key, ValueList]()
  val _keyValueStore = Map[String, ValueList]()

  def add(key: String, value: String): Unit =
  {
    if (_keyValueStore.contains(key))
      _keyValueStore(key).add(value)
    else
    {
      _keyValueStore(key) = new ValueList
      _keyValueStore(key).add(value)
    }
  }

  def remove(key: String): Unit =
  {
    if (_keyValueStore.contains(key))
      _keyValueStore(key).remove(key)
    else
      throw new Exception(s"KeyValueStore.Remove($key) not found")
  }

  def get(key: String): ValueList =
  {
    if (_keyValueStore.contains(key))
      return _keyValueStore(key)

    throw new Exception(s"KeyValueStore.Get($key) not found")
  }

  def display: Unit =
  {
    for((k, v) <- _keyValueStore)
    {
      print (s"$k -> ")
      v.display
    }
  }

  def loadFile(filePath: String, display: Boolean = false)
  {
    if (display)
      println(s"Load file: $filePath")

    val source = Source.fromFile(filePath)
    val lineIterator = source.getLines

    for (lineOriginal <- lineIterator)
    {
      val line = lineOriginal.trim

      if (!line.isEmpty && line.slice(0,2) != "//")
      {
        // TO DO: error checking for line
        val (keyStr,value) = line.splitAt(line.lastIndexOf(' '))
        val key = new Key(keyStr.trim)
        add(key._key, value.trim)
      }

      if (display) println(line)
    }
    if (display) println
  }
}


