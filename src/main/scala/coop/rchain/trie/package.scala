/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain

package object trie {
  /**
   * A Kestrel combinator
   */
   def returning[A](x: A)(f: A => Unit): A = { f(x); x }

   implicit def char2String(s: Char): String = s.toString

   implicit def string2Option(s: String): Option[String] = Option(s)
}
