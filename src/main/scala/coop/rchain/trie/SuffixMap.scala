/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

/**
 * Compare suffixes against a query to determine insertion cases
 *
 * @param query the string to be inserted
 * @param suffix the node suffix matching the query
 *
 * and    -> ""  No match            None
 * and    -> and Exact match         Match((and, and, ""), (and, and, ""), id)
 * ant    -> and Partial match 1     Match((and, an, d), (ant, an, t), id)
 * anders -> and Partial match 2     Match((and, and, ""), (anders, and, ers), id)
 * ""     -> and End of key reached
 */
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


case class SuffixMap(sx: Vector[String], kx: Vector[String]) {
  val size: Int = sx.size
  val isEmpty: Boolean = size == 0

  def contains(s: String): Boolean = sx.contains(s) || kx.contains(s)

  def suffixAt(key: String): Option[String] =
    if (contains(key)) Some(sx(kx.indexOf(key)))
    else None

  def keyAt(suffix: String): Option[String] =
    if (contains(suffix)) Some(kx(sx.indexOf(suffix)))
    else None

  def findPrefix(p: String): Option[PrefixMatch] = {
    def loop(s: String): Option[PrefixMatch] = {
      if (s.isEmpty) None
      else startsWith(s) match {
        case None    => loop(s.slice(0, s.size - 1))
        case Some(sfx) => Some(PrefixMatch(MatchResult(p, s), MatchResult(sfx, s), keyAt(sfx).get))
      }
    }
    loop(p)
  }

  def :+(t: (String, String)): SuffixMap = {
    if (contains(t._1)) {
      var i = sx.indexOf(t._1)
      SuffixMap(sx.updated(i, t._1), kx.updated(i, t._2))
    } else SuffixMap(sx :+ t._1, kx :+ t._2)
  }

  def without(suffix: String): SuffixMap = {
    if (!contains(suffix)) this
    else SuffixMap(sx.diff(Vector(suffix)), kx.diff(Vector(keyAt(suffix).get)))
  }

  def startsWith(s: String): Option[String] = {
    var v = sx.filter(_.startsWith(s))
    if (v.isEmpty) None else v.head
  }
}

object SuffixMap {
  def empty: SuffixMap = SuffixMap(Vector(), Vector())
}
