/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package Storage

import java.io.IOException
import java.lang._

object UniTests {

  def tests(): Unit = {

    println("UniTests.tests() begin")

    val storeFlat =
      "/home/griff/Documents/storage/2017-10-19_2_lmdbjava/stores/storeFlat.txt"
    val storeNested =
      "/home/griff/Documents/storage/2017-10-19_2_lmdbjava/stores/storeNested.txt"
    val storeRecursive =
      "/home/griff/Documents/storage/2017-10-19_2_lmdbjava/stores/storeRecursive.txt"

    TestsFlat(storeFlat)
    TestsNested(storeNested)
    TestsRecursive(storeRecursive)

    println("UniTests.tests() end")

  }

  def TestsRecursive(storeFilePath: String): String = {

    println("TestsRecursive() begin")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsRecursiveDb")
    storConf.name = Some("storageTestsRecursive")
    assert(storConf.isValid())

    var storage = new Storage(storConf)

    try {
      storage.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }

    println("Store contents:");
    storage.displayStrings
    println()

    try {

      println("Starting tests"); println

      // a(Y)
      var query = new Key("a(Y)")
      var queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      var oracle = new UniOracle(
        Array("[queryVars:{Y:b(c(Y))},keyVars:{}] -> [X]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(X, Y)
      query = new Key("a(X, Y)")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:b(c(A),d(e(B))),Y:f(C)},keyVars:{}] -> [Y]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(b(c(X), Y), f(2))
      // more examples like this
      query = new Key("a(b(c(X), Y), f(2))")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:A,Y:d(e(B))},keyVars:{A:X,C:2}] -> [Y]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(b(c(X), 1), f(2))
      query = new Key("a(b(c(X), 1), f(2))")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      // If you allow a constant to match a predicate:
      // Array("{X:A,1:d(e(B)),2:C} -> [Y]")
      oracle = new UniOracle(Array[String]())
      EvaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("intStrs(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsRecursive() end")

    "Completed tests"
  }

  def TestsNested(storeFilePath: String): String = {

    println("TestsNested() begin")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsNestedDb")
    storConf.name = Some("storageTestsNested")
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    try {
      storage.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }

    println("Store contents:");
    storage.displayStrings
    println()

    println("Starting tests"); println

    try {

      // a(b(X))
      var query = new Key("a(b(X))")
      var queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      var oracle = new UniOracle(
        Array("[queryVars:{X:Y},keyVars:{Y:X}] -> [two]",
              "[queryVars:{X:1},keyVars:{}] -> [one]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(1)
      query = new Key("a", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [thirteen]",
              "[queryVars:{},keyVars:{}] -> [twelve]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(1,2)
      query = new Key("a", "1", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array(""))
      EvaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("intStrs(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsNested() end")

    "Completed tests"
  }

  def TestsFlat(storeFilePath: String): String = {

    println("TestsFlat() begin")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsFlatDb")
    storConf.name = Some("storageTestsFlat")
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    try {
      storage.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }

    println("Store contents:");
    storage.displayStrings
    println()

    println("Starting tests"); println

    try {

      // a(1)
      var query = new Key("a", "1")
      var queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)

      var oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [what]",
              "[queryVars:{},keyVars:{}] -> [one]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // Z(1)
      query = new Key("Z", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array(""))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(2)
      query = new Key("a", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{}] -> [two]",
              "[queryVars:{},keyVars:{X:2}] -> [what]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(3)
      query = new Key("a", "3")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array("[queryVars:{},keyVars:{X:3}] -> [what]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(X)
      query = new Key("a", "X")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:2},keyVars:{}] -> [two]",
              "[queryVars:{},keyVars:{}] -> [what]",
              "[queryVars:{X:1},keyVars:{}] -> [one]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // a(Y)
      query = new Key("a", "Y")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{Y:2},keyVars:{}] -> [two]",
              "[queryVars:{Y:X},keyVars:{X:Y}] -> [what]",
              "[queryVars:{Y:1},keyVars:{}] -> [one]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(1,1)
      query = new Key("b", "1", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{X:1,Y:1}] -> [this]"))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(1,2)
      query = new Key("b", "1", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{X:2}] -> [where]",
          "[queryVars:{},keyVars:{X:1}] -> [how]",
          "[queryVars:{},keyVars:{X:1,Y:2}] -> [this]",
          "[queryVars:{},keyVars:{}] -> [who]"
        ))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(1,X)
      // good test
      query = new Key("b", "1", "X")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{}] -> [where]",
          "[queryVars:{X:2},keyVars:{X:1}] -> [how]",
          "[queryVars:{X:Y},keyVars:{X:1,Y:X}] -> [this]",
          "[queryVars:{X:2},keyVars:{}] -> [who]"
        ))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(1,Y)
      query = new Key("b", "1", "Y")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{Y:X},keyVars:{X:Y}] -> [where]",
          "[queryVars:{Y:2},keyVars:{X:1}] -> [how]",
          "[queryVars:{},keyVars:{X:1}] -> [this]",
          "[queryVars:{Y:2},keyVars:{}] -> [who]"
        ))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(A,B)
      query = new Key("b", "A", "B")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{A:1,B:X},keyVars:{X:B}] -> [where]",
          "[queryVars:{A:10,B:11},keyVars:{}] -> [yeah]",
          "[queryVars:{A:X,B:2},keyVars:{X:A}] -> [how]",
          "[queryVars:{A:X,B:Y},keyVars:{X:A,Y:B}] -> [this]",
          "[queryVars:{A:1,B:2},keyVars:{}] -> [who]"
        ))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(X,2)
      // very good test
      query = new Key("b", "X", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{Y:2}] -> [this]",
          "[queryVars:{X:1},keyVars:{}] -> [who]",
          "[queryVars:{X:1},keyVars:{X:2}] -> [where]",
          "[queryVars:{},keyVars:{}] -> [how]"
        ))
      EvaluateTest(query.term, queryOutcome, oracle)

      // b(X,1)
      query = new Key("b", "X", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:1},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{Y:1}] -> [this]"))
      EvaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("intStrs(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsFlat() end")

    "Completed tests"
  }

  def EvaluateTest(queryName: String,
                   unification: QueryTools.Unification,
                   oracle: UniOracle,
                   display: Boolean = true): Boolean = {
    val queryResults = unification.repr

    if (queryResults.length == 0) {
      if (display) {
        println(s"$queryName failed, not in the store"); println
      }
      return false
    }

    if (display) { print(s"$queryName ") }

    if (TestTools.ArraysEqual(queryResults, oracle.standard)) {
      if (display) {
        println("succeeded:")
        for (i <- 0 until queryResults.length) {
          println(queryResults(i))
        }
      }
    } else {
      if (display) {
        println("failed result:")
        for (i <- 0 until queryResults.length) {
          println(queryResults(i))
        }
        println("should be:")
        for (i <- 0 until oracle.standard.length) {
          println(oracle.standard(i))
        }
      }
      if (display) { println }
      return false
    }
    if (display) { println }
    true
  }

  // UniOracle is an oracle for a unification
  class UniOracle(standardRepresentation: Array[String]) {
    val standard = standardRepresentation
  }
}
