/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package scala.coop.rchain.KeyValueStore

import java.io.IOException
import java.lang._
import scala.io.StdIn

object Tests {
  def TestsRecursive(storeFilePath: String): String = {
    var store = new KeyValueStore
    try {
      store.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }

    println("Store contents:");
    store.display; println

    println("Starting tests"); println

    // a(Y)
    var query = new Key("a(Y)")
    var queryOutcome = QueryTools.queryResultsToArrayString(
      query,
      query.unifyQuery(store),
      store)
    var oracle = new UniOracle(
      Array("[queryVars:{Y:b(c(Y))},keyVars:{}] -> [X]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(X, Y)
    query = new Key("a(X, Y)")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{X:b(c(A),d(e(B))),Y:f(C)},keyVars:{}] -> [Y]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(b(c(X), Y), f(2))
    // more examples like this
    query = new Key("a(b(c(X), Y), f(2))")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{X:A,Y:d(e(B))},keyVars:{A:X,C:2}] -> [Y]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(b(c(X), 1), f(2))
    query = new Key("a(b(c(X), 1), f(2))")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    // If you allow a constant to match a predicate:
    // Array("{X:A,1:d(e(B)),2:C} -> [Y]")
    oracle = new UniOracle(Array[String]())
    EvaluateTest(query.term, queryOutcome, oracle)

    "Completed tests"
  }

  def TestsNested(storeFilePath: String): String = {
    var store = new KeyValueStore
    try {
      store.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening file: " + storeFilePath)
        return "Error opening store file: " + storeFilePath
    }
    println("Store contents:");
    store.display; println

    println("Starting tests"); println

    // a(b(X))
    var query = new Key("a(b(X))")
    var queryOutcome = QueryTools.queryResultsToArrayString(
      query,
      query.unifyQuery(store),
      store)
    var oracle = new UniOracle(
      Array("[queryVars:{X:Y},keyVars:{Y:X}] -> [two]",
            "[queryVars:{X:1},keyVars:{}] -> [one]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(1)
    query = new Key("a", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{},keyVars:{X:1}] -> [thirteen]",
            "[queryVars:{},keyVars:{}] -> [twelve]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(1,2)
    query = new Key("a", "1", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(Array(""))
    EvaluateTest(query.term, queryOutcome, oracle)

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
    println("Store contents:");
    store.display; println

    println("Starting tests"); println

    // a(1)
    var query = new Key("a", "1")
    var queryOutcome = QueryTools.queryResultsToArrayString(
      query,
      query.unifyQuery(store),
      store)
    var oracle = new UniOracle(
      Array("[queryVars:{},keyVars:{X:1}] -> [what]",
            "[queryVars:{},keyVars:{}] -> [one]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // Z(1)
    query = new Key("Z", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(Array(""))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(2)
    query = new Key("a", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{},keyVars:{}] -> [two]",
            "[queryVars:{},keyVars:{X:2}] -> [what]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(3)
    query = new Key("a", "3")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(Array("[queryVars:{},keyVars:{X:3}] -> [what]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(X)
    query = new Key("a", "X")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{X:2},keyVars:{}] -> [two]",
            "[queryVars:{},keyVars:{}] -> [what]",
            "[queryVars:{X:1},keyVars:{}] -> [one]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // a(Y)
    query = new Key("a", "Y")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{Y:2},keyVars:{}] -> [two]",
            "[queryVars:{Y:X},keyVars:{X:Y}] -> [what]",
            "[queryVars:{Y:1},keyVars:{}] -> [one]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // b(1,1)
    query = new Key("b", "1", "1")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{},keyVars:{X:1}] -> [where]",
            "[queryVars:{},keyVars:{X:1,Y:1}] -> [this]"))
    EvaluateTest(query.term, queryOutcome, oracle)

    // b(1,2)
    query = new Key("b", "1", "2")
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
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
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
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
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
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
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
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
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
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
    queryOutcome = QueryTools.queryResultsToArrayString(query,
                                                        query.unifyQuery(store),
                                                        store)
    oracle = new UniOracle(
      Array("[queryVars:{X:1},keyVars:{X:1}] -> [where]",
            "[queryVars:{},keyVars:{Y:1}] -> [this]"))
    EvaluateTest(query.term, queryOutcome, oracle)

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
      if (display) println
      return false
    }
    if (display) println
    true
  }

  // UniOracle is an oracle for a unification
  class UniOracle(standardRepresentation: Array[String]) {
    val standard = standardRepresentation
  }
}

object TestTools {
  def interactive(): Unit = {
    println; println

    val kvs = new KeyValueStore

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
            val unification = QueryTools.queryResultsToArrayString(
              query,
              query.unifyQuery(kvs),
              kvs)
            for (binding <- unification.repr) {
              println(binding)
            }
            if (unification.repr.isEmpty)
              println("no match in store")
          }
          case "key-value" => {
            var (keyStr, valuesStr) = remainder.splitAt(remainder.indexOf("->"))
            val key = new Key(keyStr)
            valuesStr = valuesStr.substring(2, valuesStr.length)
            val values = valuesStr.split(",")
            for (value <- values) {
              kvs.add(key, new Value(value))
            }
            println("key value store contents:")
            kvs.display
          }
          case "display" => {
            println("key value store contents:")
            kvs.display
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
        case "e" => return ("exit", "")
        case "q" => return ("query", remainder)
        case "a" => return ("key-value", remainder)
        case "d" => return ("display", remainder)
        case _ => {
          if (!line.isEmpty)
            println(s"unknown command: $command");
          line = ""
        }
      }
    } while (line.isEmpty)
    throw new Exception("readLine(): shouldn't get here")
  }

  def ArraysEqual(a1: Array[String], a2: Array[String]): Boolean = {
    if (a1.length != a2.length) return false
    for (i <- 0 until a1.length) {
      var contains = false
      for (j <- 0 until a2.length) {
        if (a1(i) == a2(j)) {
          contains = true
        }
      }
      if (!contains) {
        return false
      }
    }
    true
  }

  def readQuery(): String = {
    var query = ""
    do {
      query = StdIn.readLine("Query: ")
    } while (query.isEmpty)
    query
  }
}
