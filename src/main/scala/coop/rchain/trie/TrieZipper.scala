/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

sealed trait RTrie[+A]
case object Nil extends RTrie[Nothing]

case class TrieZipper[A](next: RTrie[A], prev: RTrie[A] = Nil) {
  def left: TrieZipper[A] = ???
  def right: TrieZipper[A] = ???
  def get: Option[A] = ???
  def set(x: A): TrieZipper[A] = ???
  def toTrie: RTrie[A] = ???
}
