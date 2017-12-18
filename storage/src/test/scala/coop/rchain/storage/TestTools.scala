/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage

import java.lang._
import java.util.Calendar

object TestTools {

  // numKeysIn parameter is Int because we need to use an array to hold all
  // returned values and array indices must be Int.
  class RandKeyGen(numKeysIn: Int) {
    val numKeys = numKeysIn
    var countUsedKeys = 0
    val usedKeys = new Array[Boolean](numKeys)
    val randGen =
      new scala.util.Random(Calendar.getInstance().getTimeInMillis())

    def nextKey(): Option[Int] = {
      if (numKeys <= countUsedKeys) {
        None
      } else {
        var r = randGen.nextInt(numKeys)
        while (usedKeys(r)) {
          r = randGen.nextInt(numKeys)
        }
        countUsedKeys += 1
        usedKeys(r) = true
        Some(r)
      }
    }
  }

  def createBlobs(blobsCount: Int, blogSize: Int): Array[String] = {
    val blobs = new Array[String](blobsCount)

    for (iStr <- 0 until blobsCount) {
      var strBuf = new StringBuffer(blogSize)
      for (i <- 0 until blogSize) {
        strBuf.append((iStr % 10).toString)
      }
      blobs(iStr) = strBuf.toString
    }
    blobs
  }

  def arraysEqual(a1: Array[String], a2: Array[String]): Boolean = {
    if (a1.length != a2.length) {
      false
    } else {
      var equal = true
      val a1sorted = a1.sorted
      val a2sorted = a2.sorted
      var i = 0
      while (i < a1sorted.length && equal) {
        if (a1sorted(i) != a2sorted(i)) {
          equal = false
        }
        i += 1
      }
      equal
    }
  }
}
