/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

import java.io.IOException
import KeyValueStore._

// Usage:
// no arguments : interactive test harness with empty store
// <store file path> : read file into store and launch interactive test harness
// test <store file path> : read file into store and then run corresponding test

object Main {
  def main(args: Array[String]): Unit = {
    // For historical reasons, this code contains two representations of the
    // unification of a query and a key.  When I first wrote the code I represented
    // the unification in a non-standard way.  I wrote the tests in Tests.scala
    // using the non-standard notation.  When I was introduced to the standard
    // notation, I incorporated it into this code.  I wanted to test the new code
    // against the existing tests written with the non-standard notation.
    //
    // Set uniRep to "Standard" to run the code with standard unification
    // representation.  Use any non-empty string other than "Standard" to
    // use the non-standard notation.
    val uniRep = "Standard"

    var storeFilePath = ""
    if (args.length == 0) {
      TestTools.interactive()
      return
    } else if (args.length == 1) {
      storeFilePath = args(0)
    } else if (args.length == 2) {
      val arg = args(0)
      storeFilePath = args(1)
      if (arg.slice(0, 4).toLowerCase == "test") {
        if (storeFilePath.contains("Flat")) {
          val outcome = Tests.TestsFlat(storeFilePath, uniRep)
          println(outcome)
        } else if (storeFilePath.contains("Nested")) {
          val outcome = Tests.TestsNested(storeFilePath, uniRep)
          println(outcome)
        } else if (storeFilePath.contains("Recursive")) {
          val outcome = Tests.TestsRecursive(storeFilePath, uniRep)
          println(outcome)
        } else {
          println("File not found: " + storeFilePath)
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

    var queryStr = TestTools.readQuery()
    while (queryStr != "q") {
      val query = new Key(queryStr)
      println

      val matches = query.unifyQuery(store)

      println(query.term + " returns:")
      val twoUnifications =
        QueryTools.queryResultsToArrayString(query, matches, store)
      if (matches.size == 0)
        println("no matches")
      println

      queryStr = TestTools.readQuery()
    }
  }
}
