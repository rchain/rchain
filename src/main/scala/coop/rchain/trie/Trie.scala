/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import scala.collection.immutable.Vector

import org.mongodb.scala.bson.Document
import io.jvm.uuid._
import coop.rchain.trie.Datastore._

sealed trait Trie {
  val id: String
  val children: SuffixMap
  val v: Option[String]
  def isLeaf: Boolean = children.isEmpty || terminatorKey != None

  val terminatorKey: Option[String] = children.get(Trie.Terminator)

  def get(k: String): Option[String] = v

  /**
   * Insert a key value pair
   *
   */
  def put(k: String, v: String): Trie = {
    if (k.isEmpty) IO.Update(Node(id, children, v)).run
    else children.findPrefix(k) match {
      case None                    => doAppend(k, v).run
      case Some(px) if px.exact    => Trie.ioGet(px.id).get.put(px.query.tail, v)
      case Some(px) if px.partial2 => expand2(px, v)
      case Some(px)                => expand(px, v)
    }
  }

  /**
   * Insert a partially matched key 1 (suffix) or both remainders
   *
   * Split on the shared prefix. Create a branch pointing to a new leaf whilst
   * preserving the subtree at the existing path.
   *
   *       (and)       put(ant,789) =>       (an)
   *         |                               /  \
   *        456                           (d)    (t)
   *                                      /        \
   *                                    456        789
   *
   * Where a prefix match has no remainder on the key we use a terminator
   * symbol to maintain a path to the existing key value.
   *
   *       (and)       put(a,789) =>         (a)
   *         |                               /  \
   *        456                           (TS)   (nd)
   *                                      /        \
   *                                    789        456
   *
   * @param px prefix match details
   * @param v the value to insert at the new branch
   */
  private def expand(px: PrefixMatch, v: String): Trie = {
    val tail = if (px.query.tail.isEmpty) Trie.Terminator else px.query.tail
    doExpand(px, v, tail, px.suffix.tail, px.suffix.head).run
  }

  private def doExpand(px: PrefixMatch, v: String, sfx1: String, sfx2: String, updated: String): IO[Trie] = for {
    leaf   <- IO.Insert(Node(v))
    node   <- IO.Insert(Node(SuffixMap.empty + (sfx1 -> leaf.id) + (sfx2 -> px.id)))
    parent <- IO.Update(Node(id, children - px.suffix.word + (updated -> node.id)))
  } yield parent

  /**
   * Insert a partially matched key with 1 (key) remainder
   *
   * Where a prefix match has no remainder on the existing suffix we use a terminator
   * symbol to maintain the path to values where they exist.
   *
   * If the matched suffix has no value we can continue to the next node
   *
   *       (dog)       put(dogs,789) =>      (dog)            (dog)
   *         |                               /  \                \
   *        456                           (TS)   (s)            (e s)
   *                                      /        \            /   \
   *                                    456        789       (...)  789
   *
   * @param px prefix match details
   * @param v the value to insert at the new branch
   */
  private def expand2(px: PrefixMatch, v: String): Trie = {
    db.get(px.id) match {
      case Some(n) if (n.children.isEmpty) =>
        doExpand(px, v, px.query.tail, Trie.Terminator, px.suffix.word).run
      case Some(n) => n.put(px.query.tail, v)
      case None => expand2(px, v) // retry
    }
  }

  /**
   * Append a non-matching key
   *
   * When a new key does not match any existing suffixes we can simply append
   * a new leaf onto this node
   *
   *       (and)      append(dig,789) =>     (and dig)
   *         |                                /     \
   *        456                             456     789
   *
   * @param s the suffix string
   * @param v the value to insert at the leaf
   */
  private def doAppend(s: String, v: String): IO[Trie] = for {
    leaf   <- IO.Insert(Node(v))
    parent <- IO.Update(Node(id, children + (s -> leaf.id)))
  } yield leaf
}

case class Node(id: String, children: SuffixMap, v: Option[String]) extends Trie

object Node {
  def makeId: String = UUID.randomString

  def apply(children: SuffixMap): Node = apply(makeId, children, None)

  def apply(children: SuffixMap, v: Option[String]): Node = apply(makeId, children, v)

  def apply(id: String, children: SuffixMap): Node = apply(id, children, None)

  def apply(v: Option[String]): Node = apply(makeId, SuffixMap.empty, v)
}

object Trie {
  val Terminator = ">"

  def empty: Trie = leaf(None)

  def leaf(value: Option[String]): Trie = Node(value)

  def root(namespace: String): Trie = {
    def create: Trie = IO.Insert(Node(namespace, SuffixMap.empty)).run
    IO.Get(namespace).run match {
      case None    => create
      case Some(r) => r
    }
  }

  def ioGet(id: String): Option[Trie] = IO.Get(id).run

  def put(ns: String, k: String, v: String): Trie = {
    root(ns).put(k, v)
  }
  
  def get(ns: String, k: String): Option[String] = {
    def explore(t: Trie, s: String, depth: Int): Option[String] = {
      if (depth == k.length) t.terminatorKey match {
        case None => t.get(k)
        case Some(id) => ioGet(id).get.get(k)
      }
      else t.children.checkPrefix(s) match {
        case Miss(_) => None
        case Hit(id) => ioGet(id).flatMap(x => explore(x, "", depth + s.length))
        case Partial(id,h,_) => ioGet(id).flatMap(x => explore(x, s substring h._1.length, depth + h._1.length))
      }
    }
    if (ns.isEmpty || k.isEmpty) None
    else explore(root(ns), k, 0)
  }
}
