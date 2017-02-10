/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import scala.collection.immutable.MapLike
import scala.collection.immutable.Map

case class PrefixMatch(query: MatchResult, suffix: MatchResult, id: String) {
  val exact: Boolean = suffix.word == query.word
  val partial1: Boolean = !exact && !query.tail.isEmpty && !suffix.tail.isEmpty
  val partial2: Boolean = !exact && !query.tail.isEmpty && suffix.tail.isEmpty
  val partial3: Boolean = !exact && query.tail.isEmpty && !suffix.tail.isEmpty
  val depth: Int = query.head.length
  val split: Boolean = partial1
}

case class MatchResult(word: String, head: String) {
  val tail: String = word.splitAt(head.length)._2
}

case class SuffixMap(sx: Vector[String], kx: Vector[String]) extends Map[String, String] with MapLike[String, String, SuffixMap] {
  
  def get(suffix: String): Option[String] = sx.find(_.eq(suffix)).flatMap(valueAt(_))
  
  def iterator: Iterator[(String, String)] = sx.iterator.map(s => (s -> valueAt(s)))
    
  def + [B >: String](kv: (String, B)): SuffixMap = {
    if (contains(kv._1)) {
      var i = sx.indexOf(kv._1)
      SuffixMap(sx.updated(i, kv._1), kx.updated(i, kv._2.toString))
    } 
    else SuffixMap(sx :+ kv._1, kx :+ kv._2.toString)
  }
  
  def - (suffix: String): SuffixMap = {
    if (!contains(suffix)) this
    else SuffixMap(sx.diff(Vector(suffix)), kx.diff(Vector(valueAt(suffix).get)))
  }
  
  override def empty: SuffixMap = SuffixMap(Vector(), Vector())
  override def size: Int = sx.size

  def findPrefix(p: String): Option[PrefixMatch] = {
    def loop(s: String): Option[PrefixMatch] = {
      if (s.isEmpty) None
      else keyWithPrefix(s) match {
        case None    => loop(s.slice(0, s.size - 1))
        case Some(sfx) => Some(PrefixMatch(MatchResult(p, s), MatchResult(sfx, s), get(sfx).get))
      }
    }
    loop(p)
  }

  def keyWithPrefix(s: String): Option[String] = sx.filter(_.startsWith(s)).headOption
  
  private def valueAt(suffix: String): String = if (sx.contains(suffix)) kx(sx.indexOf(suffix)) else ""
}

object SuffixMap {
  def apply(kvs: (String, String)*): SuffixMap = kvs.foldLeft(empty)(_+_)
  
  def empty: SuffixMap = SuffixMap(Vector(), Vector())
}
