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

  def get(suffix: String): Option[String] = 
    if (sx.contains(suffix)) kx(sx.indexOf(suffix))
    else None

  def iterator: Iterator[(String, String)] = sx.iterator.map(s => (s -> kx(sx.indexOf(s))))

  def +[B >: String](kv: (String, B)): SuffixMap = {
    if (sx.contains(kv._1)) {
      var i = sx.indexOf(kv._1)
      SuffixMap(sx.updated(i, kv._1), kx.updated(i, kv._2.toString))
    }
    else SuffixMap(sx :+ kv._1, kx :+ kv._2.toString)
  }

  def - (suffix: String): SuffixMap = {
    if (!sx.contains(suffix)) this
    else SuffixMap(sx.diff(Vector(suffix)), kx.diff(Vector(kx(sx.indexOf(suffix)))))
  }

  override def empty: SuffixMap = SuffixMap(Vector(), Vector())
  override def size: Int = sx.size

  def checkPrefix(query: String): PrefixMatch = {
    def loop(s: String): PrefixMatch = {
      if (s.isEmpty) Miss(query)
      else sx.filter(_.startsWith(s)).headOption match {
        case None      => loop(s.slice(0, s.size - 1))
        case Some(hit) => PrefixMatch(query, hit, s, get(hit).get)
      }
    }
    val overlap: Int = if (kx.isEmpty) 0 else kx.max.length - 1
    loop(query.slice(0, overlap))
  }
}

object SuffixMap {
  def apply(kvs: (String, String)*): SuffixMap = kvs.foldLeft(empty)(_ + _)
  def empty: SuffixMap = SuffixMap(Vector(), Vector())
}

trait PrefixMatch {
  val suffix: String = ""
}
trait PartialMatch extends PrefixMatch {
  val id: String
  val h: (String, String)
  val t: String
  override val suffix: String = if (h._2 == Trie.Terminator) h._1 else h._1 + h._2
  def depth = h._1.length
}
case class Hit(id: String) extends PrefixMatch
case class Miss(key: String) extends PrefixMatch
case class Partial(id: String, h: (String, String), t: String) extends PartialMatch
case class PartialLeft(id: String, h: (String, String), t: String) extends PartialMatch
case class PartialRight(id: String, h: (String, String), t: String) extends PartialMatch

object PrefixMatch {
  def apply(query: String, suffix: String, mtch: String, id: String): PrefixMatch = {
    val tails = (query.splitAt(mtch.length)._2, suffix.splitAt(mtch.length)._2)
    (tails._1.isEmpty, tails._2.isEmpty) match {
      case (true,true)   => Hit(id)
      case (false,true)  => PartialRight(id, (mtch, Trie.Terminator), tails._1)
      case (true,false)  => PartialLeft(id, (mtch, tails._2), Trie.Terminator)
      case (false,false) => Partial(id, (mtch, tails._2), tails._1)
    }
  }
}
