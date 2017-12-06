/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

import coop.rchain.storage.{LmdbTests, StorageTests, UniTests}

object Main {

  def main(args: Array[String]): Unit = {

    try {
      /*
      val arg = if (args.length == 1) { Some(args(0)) } else { None }
      UniTests.interactive(arg)
     */
    } catch {
      case e: Throwable => {
        println("main(): " + e)
      }
    }
  }
}
