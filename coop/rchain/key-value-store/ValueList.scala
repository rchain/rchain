/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable._


// ValueList is a list of strings meant to model a list of blobs or data

class ValueList
{
  // If the same value must be added more than once
  // then you need to use an Array instead of LinkedHashSet
  val _linkedHashSet = LinkedHashSet[String]()
  def add(x: String): Unit = { _linkedHashSet += x }
  def remove(x: String): Unit = { _linkedHashSet -= x }
  override def toString: String =
  {
    var str = "["
    var arr = _linkedHashSet.toArray
    for (i <- 0 until arr.size-1)
      str += arr(i) + ","
    str += arr(arr.size-1) + "]"
    str
  }
  def display: Unit = { println(toString) }
}

