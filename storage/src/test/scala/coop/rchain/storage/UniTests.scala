/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

import java.io.File
import java.io.IOException
import java.lang._
import org.scalatest._
import scala.io.StdIn

class UniTests extends FlatSpec with Matchers {
  val basePath = System.getProperty("user.dir") +
    "/src/test/scala/coop/rchain/storage/stores/"
  val storeFlatPath = basePath + "storeFlat.txt"
  val storeNestedPath = basePath + "storeNested.txt"
  val storeRecursivePath = basePath + "storeRecursive.txt"

  "Storage Unifier" should "unify nested keys" in {
    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(basePath)
    storConf.dirName = Some("storageTestsRecursiveDb")
    storConf.name = Some("storageTestsRecursive")
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    try {
      storage.loadFile(storeRecursivePath)
    } catch {
      case _: IOException => {
        fail("Error opening file: " + storeRecursivePath)
      }
    }

    try {
      // a(Y)
      var query = new Key("a(Y)")
      var queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      var oracle =
        new UniOracle(Array("[queryVars:{Y:b(c(Y))},keyVars:{}] -> [X]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(X, Y)
      query = new Key("a(X, Y)")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:b(c(A),d(e(B))),Y:f(C)},keyVars:{}] -> [Y]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(b(c(X), Y), f(2))
      query = new Key("a(b(c(X), Y), f(2))")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:A,Y:d(e(B))},keyVars:{A:X,C:2}] -> [Y]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(b(c(X), 1), f(2))
      query = new Key("a(b(c(X), 1), f(2))")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      // If constants were allowed to match a predicate:
      // Array("{X:A,1:d(e(B)),2:C} -> [Y]")
      oracle = new UniOracle(Array[String]())
      assert(!evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // bt(X,bt(1,Y,3),bt(4,5,Z))
      query = new Key("bt(X,bt(1,Y,3),bt(4,5,Z))")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle =
        new UniOracle(Array("[queryVars:{X:0,Y:2,Z:6},keyVars:{}] -> [Z]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)
    } catch {
      case e: Throwable => {
        fail(e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }
  }

  "Storage Unifier" should "unify nested keys 2" in {

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsNestedDb")
    storConf.name = Some("storageTestsNested")
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    try {
      storage.loadFile(storeNestedPath)
    } catch {
      case _: IOException => {
        fail("Error opening file: " + storeNestedPath)
      }
    }

    try {

      // a(b(X))
      var query = new Key("a(b(X))")
      var queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      var oracle = new UniOracle(
        Array("[queryVars:{X:Y},keyVars:{Y:X}] -> [two]",
              "[queryVars:{X:1},keyVars:{}] -> [one]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(1)
      query = new Key("a", "1")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [thirteen]",
              "[queryVars:{},keyVars:{}] -> [twelve]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(1,2)
      query = new Key("a", "1", "2")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(Array(""))
      assert(!evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)
    } catch {
      case e: Throwable => {
        fail(e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }
  }

  "Storage Unifier" should "unify flat keys" in {

    val storConf = new StorageConfig()
    storConf.isKeyToValues = true
    storConf.isWritable = true
    storConf.baseDir = Some(System.getProperty("user.dir"))
    storConf.dirName = Some("storageTestsFlatDb")
    storConf.name = Some("storageTestsFlat")
    assert(storConf.isValid())

    var storage = new Storage(storConf)
    try {
      storage.loadFile(storeFlatPath)
    } catch {
      case _: IOException =>
        fail("Error opening file: " + storeFlatPath)
    }

    try {
      // a(1)
      var query = new Key("a", "1")
      var queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)

      var oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [what]",
              "[queryVars:{},keyVars:{}] -> [one]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // Z(1)
      query = new Key("Z", "1")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(Array(""))
      assert(!evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(2)
      query = new Key("a", "2")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{}] -> [two]",
              "[queryVars:{},keyVars:{X:2}] -> [what]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(3)
      query = new Key("a", "3")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(Array("[queryVars:{},keyVars:{X:3}] -> [what]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(X)
      query = new Key("a", "X")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:2},keyVars:{}] -> [two]",
              "[queryVars:{},keyVars:{}] -> [what]",
              "[queryVars:{X:1},keyVars:{}] -> [one]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // a(Y)
      query = new Key("a", "Y")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{Y:2},keyVars:{}] -> [two]",
              "[queryVars:{Y:X},keyVars:{X:Y}] -> [what]",
              "[queryVars:{Y:1},keyVars:{}] -> [one]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(1,1)
      query = new Key("b", "1", "1")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{X:1,Y:1}] -> [this]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(1,2)
      query = new Key("b", "1", "2")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{X:2}] -> [where]",
          "[queryVars:{},keyVars:{X:1}] -> [how]",
          "[queryVars:{},keyVars:{X:1,Y:2}] -> [this]",
          "[queryVars:{},keyVars:{}] -> [who]"
        ))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(1,X)
      // good test
      query = new Key("b", "1", "X")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{}] -> [where]",
          "[queryVars:{X:2},keyVars:{X:1}] -> [how]",
          "[queryVars:{X:Y},keyVars:{X:1,Y:X}] -> [this]",
          "[queryVars:{X:2},keyVars:{}] -> [who]"
        ))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(1,Y)
      query = new Key("b", "1", "Y")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{Y:X},keyVars:{X:Y}] -> [where]",
          "[queryVars:{Y:2},keyVars:{X:1}] -> [how]",
          "[queryVars:{},keyVars:{X:1}] -> [this]",
          "[queryVars:{Y:2},keyVars:{}] -> [who]"
        ))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(A,B)
      query = new Key("b", "A", "B")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
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
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(X,2)
      // very good test
      query = new Key("b", "X", "2")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array(
          "[queryVars:{},keyVars:{Y:2}] -> [this]",
          "[queryVars:{X:1},keyVars:{}] -> [who]",
          "[queryVars:{X:1},keyVars:{X:2}] -> [where]",
          "[queryVars:{},keyVars:{}] -> [how]"
        ))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)

      // b(X,1)
      query = new Key("b", "X", "1")
      queryOutcome =
        QueryTools.queryResultsToArrayString(query,
                                             query.unifyQuery(storage),
                                             storage)
      oracle = new UniOracle(
        Array("[queryVars:{X:1},keyVars:{X:1}] -> [where]",
              "[queryVars:{},keyVars:{Y:1}] -> [this]"))
      assert(evaluateTest(query.term, queryOutcome, oracle),
             "query: " + query.term)
    } catch {
      case e: Throwable => {
        fail(e)
      }
    } finally {
      storage.close()
      storage.deleteFiles()
    }
  }

  def evaluateTest(queryName: String,
                   unification: QueryTools.Unification,
                   oracle: UniOracle,
                   display: Boolean = false): Boolean = {
    val queryResults = unification.repr

    if (queryResults.length == 0) {
      if (display) {
        println(s"$queryName failed, not in the store"); println
      }
      false
    }
    else {
      var returnVal = true

      if (display) {
        print(s"$queryName ")
      }

      if (TestTools.arraysEqual(queryResults, oracle.standard)) {
        if (display) {
          println("succeeded:")
          for (i <- queryResults.indices) {
            println(queryResults(i))
          }
        }
      } else {
        if (display) {
          println("failed result:")
          for (i <- queryResults.indices) {
            println(queryResults(i))
          }
          println("should be:")
          for (i <- oracle.standard.indices) {
            println(oracle.standard(i))
          }
        }
        if (display) {
          println
        }
        returnVal = false
      }
      if (display) {
        println
      }
      returnVal
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
            for (binding <- bindings) {
              println(binding)
            }
            for (key <- storage.unifyQuery(query)) {
              println(key.term)
            }
            if (bindings.isEmpty) {
              println("no match in store")
            }
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
      } else {
        command = line
      }

      command match {
        case "e" => ("exit", "")
        case "q" => ("query", remainder)
        case "a" => ("key-value", remainder)
        case "d" => ("display", remainder)
        case _ => {
          if (!line.isEmpty) {
            println(s"unknown command: $command")
          }
          line = ""
        }
      }
    } while (line.isEmpty)
    throw new Exception("readLine(): shouldn't get here")
  }

  def readQuery(): String = {
    var query = ""
    do {
      query = StdIn.readLine("Query: ")
    } while (query.isEmpty)
    query
  }
}
