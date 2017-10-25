/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.Storage

import java.lang._
import java.util.Calendar


object TestTools {

  // numKeysIn parameter is Int because we need to use an array to hold all
  // returned values and array indices must be Int.
  class RandKeyGen(numKeysIn:Int) {
    val numKeys = numKeysIn
    var countUsedKeys = 0
    val usedKeys = new Array[Boolean](numKeys)
    val randGen = new scala.util.Random(Calendar.getInstance().getTimeInMillis())

    def nextKey(): Option[Int] = {
      if (numKeys <= countUsedKeys)
        return None
      var r = randGen.nextInt(numKeys)
      while (usedKeys(r))
        r = randGen.nextInt(numKeys)
      countUsedKeys += 1
      usedKeys(r) = true
      Some(r)
    }
  }

  // blobsCount parameter must be Int because it is used to define an Array.
  // blobLength parameter must be Int because it is used to define a StringBuffer
  def createBlobs(blobsCount:Int, blogSize:Int): Array[String] =
  {
    val blobs = new Array[String](blobsCount)

    for (iStr <- 0 until blobsCount)
    {
      var strBuf = new StringBuffer(blogSize)
      for (i <- 0 until blogSize)
        strBuf.append((iStr % 10).toString)
      blobs(iStr) = strBuf.toString
    }
    /* works
    for (iStr <- 0 until 10)
    {
      val bbStr = strToBb(blobs(iStr))
      val str = bbToStr(bbStr)
      println(s"$iStr: $str")
    }
    */
    blobs
  }

    /*
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
                val twoUnifications = QueryTools.queryResultsToArrayString(
                  query,
                  query.unifyQuery(kvs),
                  kvs)
                for (binding <- twoUnifications.standard)
                  println(binding)
                if (twoUnifications.standard.isEmpty)
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
          } else
            command = line

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
          for (j <- 0 until a2.length)
            if (a1(i) == a2(j))
              contains = true
          if (!contains)
            return false
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
    */

  def ArraysEqual(a1: Array[String], a2: Array[String]): Boolean = {
    if (a1.length != a2.length) return false
    for (i <- 0 until a1.length) {
      var contains = false
      for (j <- 0 until a2.length)
        if (a1(i) == a2(j))
          contains = true
      if (!contains)
        return false
    }
    true
  }
}
