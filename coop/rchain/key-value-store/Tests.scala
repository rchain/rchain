/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import java.io.IOException
import scala.collection.mutable._


object Tests
{
  def Tests(storeFilePath: String): String =
  {
    var store = new KeyValueStore
    try
    {
      store.loadFile(storeFilePath, true)
    }
    catch
    {
      case _: IOException => println("Error opening file: " + storeFilePath)
      return "Error opening store file: " + storeFilePath
    }
    println("Store contents:"); store.display
    println

    println("Starting tests"); println

    // TO DO:  Next improvement is to read the oracle from a file

    // A(1)
    var query = new Query("A", "1")
    var queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    var oracle = Array("{1:x} -> [what]","{1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // A(2)
    query = new Query("A", "2")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{2} -> [two]","{2:x} -> [what]")
    EvaluateTest(queryOutcome, oracle)

    // A(3)
    query = new Query("A", "3")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{3:x} -> [what]")
    EvaluateTest(queryOutcome, oracle)

    // A(x)
    query = new Query("A", "x")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{x:2} -> [two]","{x} -> [what]","{x:1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // A(y)
    query = new Query("A", "y")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{y:2} -> [two]","{y:x} -> [what]","{y:1} -> [one]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,1)
    query = new Query("B", "1", "1")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{1,1:x} -> [where]","{1:x,1:y} -> [this]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,2)
    query = new Query("B", "1", "2")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{1,2:x} -> [where]","{1:x,2} -> [how]",
      "{1:x,2:y} -> [this]", "{1,2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,x)
    query = new Query("B", "1", "x")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{1,x} -> [where]","{1:x,x:2} -> [how]",
      "{1:x,x:y} -> [this]", "{1,x:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(1,y)
    query = new Query("B", "1", "y")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{1,y:x} -> [where]","{1:x,y:2} -> [how]",
      "{1:x,y} -> [this]", "{1,y:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(a,b)
    query = new Query("B", "a", "b")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{a:1,b:x} -> [where]","{a:10,b:11} -> [yeah]",
      "{a:x,b:2} -> [how]","{a:x,b:y} -> [this]", "{a:1,b:2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(x,2)
    query = new Query("B", "x", "2")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{x:1,2:x} -> [where]","{x,2} -> [how]",
      "{x,2:y} -> [this]", "{x:1,2} -> [who]")
    EvaluateTest(queryOutcome, oracle)

    // B(x,1)
    query = new Query("B", "x", "1")
    queryOutcome = QueryUtils.queryResultsToArrayString(
      query, query.execute(store), store)
    oracle = Array("{x:1,1:x} -> [where]","{x,1:y} -> [this]")
    EvaluateTest(queryOutcome, oracle)

    "Completed tests"
  }

  def EvaluateTest(queryOut: (String, Array[String]), oracle: Array[String],
    display: Boolean = true): Boolean =
  {
    val queryName = queryOut._1
    val queryResults = queryOut._2

    if (display) print(s"$queryName ")
    if (ArraysEqual(queryResults, oracle))
    {
      if (display)
      {
        println("succeeded:")
        for (i <- 0 until queryResults.length)
          println(queryResults(i))
      }
    }
    else
    {
      if (display)
      {
        println("failed result:")
        for (i <- 0 until queryResults.length)
          println(queryResults(i))
        println("should be:")
        for (i <- 0 until oracle.length)
          println(oracle(i))
      }
      throw new Exception(
        s"Tests.EvaluateTest(): test failed: ${queryOut._1}")
      false
    }
    if (display) println
    true
  }

  def ArraysEqual(a1: Array[String], a2: Array[String]): Boolean =
  {
    if (a1.size != a2.size) return false
    for (i <- 0 until a1.size)
    {
      var contains = false
      for (j <- 0 until a2.size)
        if (a1(i) == a2(j))
          contains = true
      if (!contains) return false
    }
    true
  }
}

