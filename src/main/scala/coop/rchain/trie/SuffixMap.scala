/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import scala.collection.immutable.MapLike
import scala.collection.immutable.Map

case class SuffixMap(sx: Vector[String], kx: Vector[String]) extends Map[String, String] with MapLike[String, String, SuffixMap] {
  
  def get(suffix: String): Option[String] = sx.find(_.equals(suffix)).flatMap(valueAt(_))
  
  def iterator: Iterator[(String, String)] = sx.iterator.map(s => (s -> valueAt(s)))
    
  def + [B >: String](kv: (String, B)): SuffixMap = {
    if (sx.contains(kv._1)) {
      var i = sx.indexOf(kv._1)
      SuffixMap(sx.updated(i, kv._1), kx.updated(i, kv._2.toString))
    } 
    else SuffixMap(sx :+ kv._1, kx :+ kv._2.toString)
  }
  
  def - (suffix: String): SuffixMap = {
    if (!sx.contains(suffix)) this
    else SuffixMap(sx.diff(Vector(suffix)), kx.diff(Vector(valueAt(suffix).get)))
  }
  
  override def empty: SuffixMap = SuffixMap(Vector(), Vector())
  override def size: Int = sx.size
  
  def checkPrefix(query: String): PrefixMatch = {
    def loop(s: String): PrefixMatch = {
      if (s.isEmpty) Miss(query)
      else keyWithPrefix(s) match {
        case None      => loop(s.slice(0, s.size - 1)) 
        case Some(hit) => PrefixMatch(hit, query, s, get(hit).get) 
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

trait PrefixMatch {
  val suffix: String = ""
  val nextSuffix: String = ""
}
trait PartialMatch extends PrefixMatch {
  val id: String
  val h: (String, String)
  val t: String
  override val suffix: String = if (h._2 == Trie.Terminator) h._1 else h._1 + h._2
  override val nextSuffix: String = if (t == Trie.Terminator) "" else t
}
case class Hit(id: String) extends PrefixMatch
case class Miss(key: String) extends PrefixMatch
case class Partial(id: String, h: (String, String), t: String) extends PartialMatch
case class PartialLeft(id: String, h: (String, String), t: String) extends PartialMatch
case class PartialRight(id: String, h: (String, String), t: String) extends PartialMatch

object PrefixMatch {
  def apply(suffix: String, query: String, mtch: String, id: String): PrefixMatch = {
    val tails = (suffix.splitAt(mtch.length)._2, query.splitAt(mtch.length)._2)
    (tails._1.isEmpty, tails._2.isEmpty) match {
      case (true,true)   => Hit(id)
      case (false,true)  => PartialLeft(id, (mtch, tails._1), Trie.Terminator)
      case (true,false)  => PartialRight(id, (mtch, Trie.Terminator), tails._2)
      case (false,false) => Partial(id, (mtch, tails._1), tails._2)
    }
  }
}
