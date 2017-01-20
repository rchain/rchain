/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

//sealed trait RTrie[+T] {
//  def get(s: String): RTrie[T]
//  def put(s: String): RTrie[T]
//}
//
//object RTrie {
//  implicit val formats = DefaultFormats
//  def empty[T]: Leaf[T] = EmptyLeaf
//}
//
//case class SuffixMap(suffixes: Vector[String], ids: Vector[String]) {
//  def contains(s: String): Boolean
//  def get(s: String): Option[String]
//  def +(t: (String, String)): SuffixMap
//}
//case class Root(sm: SuffixMap) extends RTrie[T]
//case class Node(sm: SuffixMap) extends RTrie[T]
//case class Leaf(value: T) extends RTrie[T]
//case object EmptyLeaf extends RTrie[Nothing]