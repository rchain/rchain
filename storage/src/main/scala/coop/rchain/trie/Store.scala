/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

trait Store {
  def get(id: String): Option[Trie]
  def put(t: Trie): Unit
  def insert(t: Trie): Unit
  def delete(id: String): Unit
  def getKey(k: String, v: String): Option[Trie]
}

object Datastore {
  implicit lazy val db = new MongoDB //TODO load from config
}
