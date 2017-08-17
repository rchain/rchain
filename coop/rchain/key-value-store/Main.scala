/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

import KeyValueStore._
import scala.io._
import java.io.IOException

object Main {
  def main(args: Array[String]): Unit = {
    var storeFilePath = ""
    if (args.length == 0) {
      println("usage:")
      println("run <store file path> or")
      println("run test <store file path>")
      return
    } else if (args.length == 1) {
      storeFilePath = args(0)
    } else if (args.length == 2) {
      val arg = args(0)
      storeFilePath = args(1)
      if (arg.slice(0, 4).toLowerCase == "test") {
        if (storeFilePath.contains("Flat")) {
          val outcome = Tests.TestsFlat(storeFilePath)
          println(outcome)
        } else if (storeFilePath.contains("Nested")) {
          val outcome = Tests.TestsNested(storeFilePath)
          println(outcome)
        } else if (storeFilePath.contains("Recursive")) {
          val outcome = Tests.TestsRecursive(storeFilePath)
          println(outcome)
        }
        return
      }
      storeFilePath = arg
    } else
      throw new Exception("main(): too many parameters")

    var store = new KeyValueStore
    try {
      store.loadFile(storeFilePath, true)
    } catch {
      case _: IOException =>
        println("Error opening store file: " + storeFilePath)
        return
    }
    println("Store contents:"); store.display
    println

    println("""Enter query, "q" to quit:"""); println

    var queryStr = readQuery()
    while (queryStr != "q") {
      val query = new Key(queryStr)
      println

      val matches = query.unifyQuery(store)

      println(query.term + " returns:")
      val (_, queryResults) =
        QueryTools.queryResultsToArrayString(query, matches, store)
      for (i <- 0 until queryResults.length)
        println(queryResults(i))
      if (matches.size == 0)
        println("no matches")

      println

      queryStr = readQuery()
    }
  }

  def readQuery(): String = {
    var query = ""
    do {
      query = StdIn.readLine("Query: ")
    } while (query.isEmpty)
    query
  }
}
