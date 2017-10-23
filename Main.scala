/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

import Storage.{BbTests, LmdbTests, StorageTests, UniTests}

object Main {

  def main(args: Array[String]): Unit = {

    try {
      UniTests.tests()
      StorageTests.tests()
      LmdbTests.tests()
      BbTests.tests()
    } catch {
      case e: Throwable => {
        println("main(): " + e)
      }
    }
  }
}
