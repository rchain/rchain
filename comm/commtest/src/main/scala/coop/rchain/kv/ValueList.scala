/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.kv

import scala.collection.mutable._

// ValueList is a list of Value objects meant to model a list of
// blobs or data. The values are stored in the order of appending.

class Value(str: String) {
  if (str == null) throw new Exception("Value constructor argument is null")
  val value = str.trim
  if (value.isEmpty) throw new Exception("Value constructor argument is empty")
  override def toString: String = value
  def display: Unit = print(value)
}

class ValueList {
  val list = ArrayBuffer[Value]()
  def add(value: Value): Unit = list += value
  def remove(value: Value): Boolean = {
    for (i <- 0 until list.size)
      if (value.value == list(i).value) {
        list -= list(i)
        return true
      }
    false
  }
  def size: Int = list.size
  def iterator: Iterator[Value] = list.toIterator
  override def toString: String = {
    var str = "["
    var arr = list.toArray
    for (i <- 0 until arr.size - 1)
      str += arr(i) + ","
    str += arr(arr.size - 1) + "]"
    str
  }
  def display: Unit = print(toString)
  def Equals(inValueList: ValueList): Boolean = {
    if (list.size != inValueList.size)
      return false
    for (i <- 0 until list.size)
      if (list(i).value != inValueList.list(i).value)
        return false
    true
  }
}

object ValueListTest {
  def Test: String = {
    val list = new ValueList()
    var value = new Value("value1"); list.add(value)
    value = new Value("value2"); list.add(value)
    value = new Value("value3"); list.add(value)
    value = new Value("value1"); list.add(value)
    value = new Value("value5"); list.add(value)
    // list.display; println

    // println("remove(\"value1\")")
    var outcome = list.remove(new Value("value1"))
    var oracle = new ValueList()
    oracle.add(new Value("value2")); oracle.add(new Value("value3"));
    oracle.add(new Value("value1")); oracle.add(new Value("value5"));
    if (!outcome || !list.Equals(oracle)) {
      return "Tests failed"
    }

    // println("remove(\"value3\")")
    outcome = list.remove(new Value("value3"))
    oracle = new ValueList()
    oracle.add(new Value("value2")); oracle.add(new Value("value1"));
    oracle.add(new Value("value5"));
    if (!outcome || !list.Equals(oracle)) {
      return "Tests failed"
    }

    // println("remove(\"value1\")")
    outcome = list.remove(new Value("value1"))
    oracle = new ValueList()
    oracle.add(new Value("value2")); oracle.add(new Value("value5"));
    if (!outcome || !list.Equals(oracle)) {
      return "Tests failed"
    }

    // println("remove(\"value9\")")
    outcome = list.remove(new Value("value9"))
    oracle = new ValueList()
    oracle.add(new Value("value2")); oracle.add(new Value("value5"));
    if (outcome || !list.Equals(oracle)) {
      return "Tests failed"
    }

    "Tests succeeded"
  }
}
