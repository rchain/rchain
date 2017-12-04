/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

import java.io.IOException
import java.lang._

import org.scalatest._

import scala.io.StdIn

object UniTests {

  def tests(): Unit = {

    println("UniTests.tests() begin")

    val basePath = System.getProperty("user.dir") +
      "/src/test/scala/coop/rchain/storage/stores/"
    val storeFlat = basePath + "storeFlat.txt"
    val storeNested = basePath + "storeNested.txt"
    val storeRecursive = basePath + "storeRecursive.txt"

    TestsFlatKeys(storeFlat); println()
    TestsNestedKeys(storeNested); println()
    TestsRecursiveKeys(storeRecursive)

    TestsFlat(storeFlat)
    TestsNested(storeNested)
    TestsRecursive(storeRecursive)

    println("UniTests.tests() end")

  }

  def TestsRecursiveKeys(storeFilePath: String): String = {

    println("TestsRecursiveKeys() begin")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsRecursiveKeysDb")
    storConf.name = Some("storageTestsRecursiveKeys")
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
    storage.displayUniKeys
    println()

    try {

      println("Starting tests"); println

      // a(Y)
      var query = new Key("a(Y)")
      var keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(X, Y)
      query = new Key("a(X, Y)")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(b(c(X), Y), f(2))
      // more examples like this
      query = new Key("a(b(c(X), Y), f(2))")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(b(c(X), 1), f(2))
      query = new Key("a(b(c(X), 1), f(2))")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // bt(X,bt(1,Y,3),bt(4,5,Z))
      query = new Key("bt(X,bt(1,Y,3),bt(4,5,Z))")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      } else {
        println("query: " + query.term + " does not resolves")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()
    } catch {
      case e: Throwable => {
        println("TestsRecursiveKeys(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsRecursiveKeys() end")

    "Completed tests"
  }

  def TestsNestedKeys(storeFilePath: String): String = {

    println("TestsNestedKeys() begin")

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsNestedKeysDb")
    storConf.name = Some("storageTestsNestedKeys")
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
    storage.displayUniKeys
    println()

    println("Starting tests"); println

    try {

      // a(b(X))
      var query = new Key("a(b(X))")
      var keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(1)
      query = new Key("a", "1")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(1,2)
      query = new Key("a", "1", "2")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      // println()
    } catch {
      case e: Throwable => {
        println("TestsNestedKeys(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsNestedKeys() end")

    "Completed tests"
  }

  def TestsFlatKeys(storeFilePath: String): String = {

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
    storage.displayUniKeys
    println()

    println("Starting tests"); println

    try {

      // a(1)
      var query = new Key("a", "1")
      var keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // Z(1)
      query = new Key("Z", "1")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(2)
      query = new Key("a", "2")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(3)
      query = new Key("a", "3")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(X)
      query = new Key("a", "X")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // a(Y)
      query = new Key("a", "Y")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(1,1)
      query = new Key("b", "1", "1")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(1,2)
      query = new Key("b", "1", "2")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(1,X)
      // good test
      query = new Key("b", "1", "X")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(1,Y)
      query = new Key("b", "1", "Y")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(A,B)
      query = new Key("b", "A", "B")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(X,2)
      // very good test
      query = new Key("b", "X", "2")
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()

      // b(X,1)
      keys = storage.unifyQuery(query)
      if (0 < keys.length) {
        println("query: " + query.term + " resolves:")
      }
      for (key <- keys) {
        println(key.term)
      }
      println()
    } catch {
      case e: Throwable => {
        println("TestsFlat(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsFlat() end")

    "Completed tests"
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
    storage.displayUniKeys
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
      evaluateTest(query.term, queryOutcome, oracle)

      // a(X, Y)
      query = new Key("a(X, Y)")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:b(c(A),d(e(B))),Y:f(C)},keyVars:{}] -> [Y]"))
      evaluateTest(query.term, queryOutcome, oracle)

      // a(b(c(X), Y), f(2))
      // more examples like this
      query = new Key("a(b(c(X), Y), f(2))")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:A,Y:d(e(B))},keyVars:{A:X,C:2}] -> [Y]"))
      evaluateTest(query.term, queryOutcome, oracle)

      // a(b(c(X), 1), f(2))
      query = new Key("a(b(c(X), 1), f(2))")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      // If you allow a constant to match a predicate:
      // Array("{X:A,1:d(e(B)),2:C} -> [Y]")
      oracle = new UniOracle(Array[String]())
      evaluateTest(query.term, queryOutcome, oracle)

      // bt(X,bt(1,Y,3),bt(4,5,Z))
      query = new Key("bt(X,bt(1,Y,3),bt(4,5,Z))")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:0,Y:2,Z:6},keyVars:{}] -> [Z]"))
      evaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("TestsRecursive(): " + e)
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
    storage.displayUniKeys
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
      evaluateTest(query.term, queryOutcome, oracle)

      // a(1)
      query = new Key("a", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [thirteen]",
              "[queryVars:{},keyVars:{}] -> [twelve]"))
      evaluateTest(query.term, queryOutcome, oracle)

      // a(1,2)
      query = new Key("a", "1", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array(""))
      evaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("TestsNested(): " + e)
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
    storage.displayUniKeys
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
      evaluateTest(query.term, queryOutcome, oracle)

      // Z(1)
      query = new Key("Z", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array(""))
      evaluateTest(query.term, queryOutcome, oracle)

      // a(2)
      query = new Key("a", "2")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{}] -> [two]",
              "[queryVars:{},keyVars:{X:2}] -> [what]"))
      evaluateTest(query.term, queryOutcome, oracle)

      // a(3)
      query = new Key("a", "3")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(Array("[queryVars:{},keyVars:{X:3}] -> [what]"))
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

      // b(1,1)
      query = new Key("b", "1", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{X:1,Y:1}] -> [this]"))
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

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
      evaluateTest(query.term, queryOutcome, oracle)

      // b(X,1)
      query = new Key("b", "X", "1")
      queryOutcome = QueryTools.queryResultsToArrayString(
        query,
        query.unifyQuery(storage),
        storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:1},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{Y:1}] -> [this]"))
      evaluateTest(query.term, queryOutcome, oracle)
    } catch {
      case e: Throwable => {
        println("TestsFlat(): " + e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }

    println("TestsFlat() end")

    "Completed tests"
  }

  // evaluateTest
  def evaluateTest(queryName: String,
                   unification: QueryTools.Unification,
                   oracle: UniOracle,
                   display: Boolean = true): Boolean = {
    val queryResults = unification.repr

    if (queryResults.length == 0) {
      if (display) {
        println(s"$queryName failed, not in the store");
        println
      }
      false
    } else {
      if (display) {
        print(s"$queryName ")
      }

      var returnVal = false
      var returnValSet = false

      if (TestTools.arraysEqual(queryResults, oracle.standard)) {
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
          println
        }
        returnVal = false
        returnValSet = true
      }
      if (display) {
        println
      }
      if (returnValSet) {
        returnVal
      } else {
        false
      }
    }
  }

  // UniOracle is an oracle for a unification
  class UniOracle(standardRepresentation: Array[String]) {
    val standard = standardRepresentation
  }

  def interactive(storagePath: Option[String] = None): Unit = {
    println; println

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageInteractiveDb")
    storConf.name = Some("storageInteractive")
    assert(storConf.isValid())

    var storage = new Storage(storConf)

    if (storagePath.isDefined) {
      storage.loadFile(storagePath.get, true)
    }

    var (command, remainder) = readLine()
    command = command.trim
    remainder = remainder.trim
    remainder = remainder.replaceAll("\\s+", "")

    while (command != "exit") {
      try {
        command match {
          case "exit" => {}
          case "query" => {
            val query = new Key(remainder)
            val unified = query.unifyQuery(storage)
            val bindings =
              QueryTools.queryResultsToArrayString(query, unified, storage)
            for (binding <- bindings)
              println(binding)
            for (key <- storage.unifyQuery(query)) {
              println(key.term)
            }
            if (bindings.isEmpty)
              println("no match in store")
          }
          case "key-value" => {
            var (keyStr, valuesStr) = remainder.splitAt(remainder.indexOf("->"))
            val key = new Key(keyStr)
            valuesStr = valuesStr.substring(2, valuesStr.length)
            val values = valuesStr.split(",")
            for (value <- values) {
              storage.put(key.term, value)
            }
            println("storage contents:")
            storage.displayUniKeys
          }
          case "display" => {
            println("storage contents:")
            storage.displayUniKeys
          }
          case _ => {}
        }
      } catch {
        case _: IOException    => println("error")
        case _: AssertionError => println("error")
      }
      println

      val outcome = readLine()
      command = outcome._1
      remainder = outcome._2
    }
  }

  def readLine(): (String, String) = {
    var returnVal = ("", "")
    var returnValSet = false
    var line = ""
    do {
      line = StdIn.readLine("a, d, q, e> ")
      line = line.trim

      var command = ""
      var remainder = ""
      if (0 <= line.indexOf(' ')) {
        val outcome = line.splitAt(line.indexOf(' '))
        command = outcome._1
        remainder = outcome._2
      } else
        command = line

      command match {
        case "e" => {
          returnVal = ("exit", "")
          returnValSet = true
        }
        case "q" => {
          returnVal = ("query", remainder)
          returnValSet = true
        }
        case "a" => {
          returnVal = ("key-value", remainder)
          returnValSet = true
        }
        case "d" => {
          returnVal = ("display", remainder)
          returnValSet = true
        }
        case _ => {
          if (!line.isEmpty)
            println(s"unknown command: $command");
          line = ""
        }
      }
    } while (line.isEmpty && !returnValSet)
    assert(returnValSet)
    returnVal
  }

  def readQuery(): String = {
    var query = ""
    do {
      query = StdIn.readLine("Query: ")
    } while (query.isEmpty)
    query
  }
}
