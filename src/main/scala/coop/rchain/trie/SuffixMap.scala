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

trait XMatchResult
object XMatchResult {
  def apply(query: String, suffix: String, m: String, id: String): XMatchResult = {
    val tails = (query.splitAt(m.length)._2, suffix.splitAt(m.length)._2)
    (tails._1.isEmpty, tails._2.isEmpty) match {
      case (true,true)   => Hit(id)
      case (true,false)  => PartialRight(id, m, tails._2)
      case (false,true)  => PartialLeft(id, m, tails._1)
      case (false,false) => Partial(id, (m, tails._1), tails._2)
    }
  }
}
case class Hit(id: String) extends XMatchResult
case class Miss(key: String) extends XMatchResult
case class Partial(id: String, h: (String, String), t: String) extends XMatchResult
case class PartialRight(id: String, h: String, t: String) extends XMatchResult
case class PartialLeft(id: String, h: String, t: String) extends XMatchResult

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
  
  def checkPrefix(query: String): XMatchResult = {
    def loop(s: String): XMatchResult = {
      if (s.isEmpty) Miss(query)
      else keyWithPrefix(s) match {
        case None      => loop(s.slice(0, s.size - 1)) 
        case Some(hit) => XMatchResult(query, hit, s, get(hit).get) 
      }
    }
    loop(query)
  }

  //TODO make this more efficient. Return the k and value to avoid get(sfx) above
  def keyWithPrefix(s: String): Option[String] = sx.filter(_.startsWith(s)).headOption
  
  private def valueAt(suffix: String): String = if (sx.contains(suffix)) kx(sx.indexOf(suffix)) else ""
}

object SuffixMap {
  def apply(kvs: (String, String)*): SuffixMap = kvs.foldLeft(empty)(_+_)
  
  def empty: SuffixMap = SuffixMap(Vector(), Vector())
}
