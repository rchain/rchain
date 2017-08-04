/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import java.io.IOException
import scala.collection.mutable._

object Tests {
  def TestsNested(storeFilePath: String): String = {
    var store = new KeyValueStore
    try {
      store.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }
    println("Store contents:"); store.display
    println

    println("Starting tests"); println

    // TO DO:  Next improvement is to read the oracle from a file

    // A(B(x))
    var query = new Key("A(B(x))")
    var queryOutcome = QueryTools.queryResultsToArrayString(
      query,
      query.unifyQuery(store),
      store)
    var oracle = Array("{x:y} -> [two]", "{x:1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // A(1)
    query = new Key("A", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{1:x} -> [thirteen]", "{1} -> [twelve]")
    EvaluateTest(queryOutcome, oracle)

    // A(1,2)
    query = new Key("A", "1", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("")
    EvaluateTest(queryOutcome, oracle)

    "Completed tests"
  }

  def TestsFlat(storeFilePath: String): String = {
    var store = new KeyValueStore
    try {
      store.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }
    println("Store contents:"); store.display
    println

    println("Starting tests"); println

    // TO DO:  Next improvement is to read the oracle from a file

    // A(1)
    var query = new Key("A", "1")
    var queryOutcome = QueryTools.queryResultsToArrayString(
      query,
      query.unifyQuery(store),
      store)
    var oracle = Array("{1:x} -> [what]", "{1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // Z(1)
    query = new Key("Z", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("")
    EvaluateTest(queryOutcome, oracle)

    // A(2)
    query = new Key("A", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{2} -> [two]", "{2:x} -> [what]")
    EvaluateTest(queryOutcome, oracle)

    // A(3)
    query = new Key("A", "3")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{3:x} -> [what]")
    EvaluateTest(queryOutcome, oracle)

    // A(x)
    query = new Key("A", "x")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{x:2} -> [two]", "{x} -> [what]", "{x:1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // A(y)
    query = new Key("A", "y")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{y:2} -> [two]", "{y:x} -> [what]", "{y:1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,1)

    query = new Key("B", "1", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{1,1:x} -> [where]", "{1:x,1:y} -> [this]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,2)
    query = new Key("B", "1", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{1,2:x} -> [where]",
                   "{1:x,2} -> [how]",
                   "{1:x,2:y} -> [this]",
                   "{1,2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,x)
    query = new Key("B", "1", "x")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{1,x} -> [where]",
                   "{1:x,x:2} -> [how]",
                   "{1:x,x:y} -> [this]",
                   "{1,x:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,y)
    query = new Key("B", "1", "y")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{1,y:x} -> [where]",
                   "{1:x,y:2} -> [how]",
                   "{1:x,y} -> [this]",
                   "{1,y:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(a,b)
    query = new Key("B", "a", "b")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{a:1,b:x} -> [where]",
                   "{a:10,b:11} -> [yeah]",
                   "{a:x,b:2} -> [how]",
                   "{a:x,b:y} -> [this]",
                   "{a:1,b:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(x,2)
    query = new Key("B", "x", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{x:1,2:x} -> [where]",
                   "{x,2} -> [how]",
                   "{x,2:y} -> [this]",
                   "{x:1,2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(x,1)
    query = new Key("B", "x", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = Array("{x:1,1:x} -> [where]", "{x,1:y} -> [this]")
    EvaluateTest(queryOutcome, oracle)

    "Completed tests"
  }

  def EvaluateTest(queryOut: (String, Array[String]),
                   oracle: Array[String],
                   display: Boolean = true): Boolean = {
    val queryName = queryOut._1
    val queryResults = queryOut._2

    if (queryResults.length == 0) {
      if (display) {
        println(s"$queryName failed, not in the store")
        println
      }
      return false
    }

    if (display) print(s"$queryName ")
    if (ArraysEqual(queryResults, oracle)) {
      if (display) {
        println("succeeded:")
        for (i <- 0 until queryResults.length)
          println(queryResults(i))
      }
    } else {
      if (display) {
        println("failed result:")
        for (i <- 0 until queryResults.length)
          println(queryResults(i))
        println("should be:")
        for (i <- 0 until oracle.length)
          println(oracle(i))
      }
      return false
    }
    if (display) println
    true
  }

  def ArraysEqual(a1: Array[String], a2: Array[String]): Boolean = {
    if (a1.size != a2.size) return false
    for (i <- 0 until a1.size) {
      var contains = false
      for (j <- 0 until a2.size)
        if (a1(i) == a2(j))
          contains = true
      if (!contains)
        return false
    }
    true
  }
}
