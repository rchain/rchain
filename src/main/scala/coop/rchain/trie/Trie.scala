/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import scala.collection.immutable.Vector

import io.jvm.uuid._
import coop.rchain.trie.Datastore._

sealed trait Trie {
  val id: String
  val children: SuffixMap
  val v: Option[String]

  def tKey: Option[String] = children.get(Trie.Terminator)

  def get(k: String): Option[String] = {
    if (v == None) println("Missing Value")
    v
  }

  /**
   * Partial(id, (an, d) t) - remainders on both query and suffix
   * 
   * => Always Expand 
   *
   *  (and)   put(ant,789) =>   (an)         (px.h._1)         
   *    |                       /  \          /      \
   *   456                   (d)    (t)    (px.h._2)(px.t) 
   *                          |      |        |       |
   *                         456    789     px.id    789
   *
   * Partial(id, (an, d) t) - with existing TS
   *
   *     (and)   put(ant,123) =>  (an)               (px.h._1)         
   *     /   \                    /  \               /      \
   *   (TS) (over)              (d)  (t)        (px.h._2) (px.t) 
   *    |     |                /   \    \           |        |     
   *   456   789            (TS) (over) 123      (px.id)    123
   *                          |     |             |    |
   *                         456   789           456  789
   * 
   * PartialLeft(id, (an, d) TS) - remainder on existing suffix
   * 
   * => Always Expand 
   *
   *  (and)   put(an,789) =>   (an)           (px.h._1)
   *    |                      /  \            /     \
   *   456                  (d)   (TS)     (px.h._2) (px.t)     
   *                         |      |          |       |
   *                        456    789       px.id    789                        
   *
   * PartialLeft(id, (a, n) TS) - with existing TS
   *
   *     (an)    put(a, 345) =>  (a)             (px.h._1)
   *     /  \                   /   \             /     \
   *  (d)   (TS)              (n)   (TS)     (px.h._2)  (px.t)
   *   |      |               / \      \        / \       \
   *  456    789           (d)  (TS)   345    (px.id)     345
   *                        |     |           |     |
   *                       456   789         456   789
   *
   * PartialRight(id, (and, TS) over) - remainder on query
   * 
   *  (and)  put(andover,789) =>  (and)          (px.h._1)
   *    |                         /  \             /    \
   *   456                     (TS)  (over)   (px.h._2) (px.t)
   *                             |     |          |       |
   *                            456   789       px.id    789
   *
   * Miss - append a non-matching key
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
  def put(k: String, v: String): Trie = {
    if (k.isEmpty) IO.Update(Node(id, children, v)).run
    else children.checkPrefix(k) match {
      case Hit(id)              => Trie.ioGet(id).get.put("", v)
      case Miss(_)              => append(k, v).run
      case p @ (_:Partial)      => expand(p, v).run
      case p @ (_:PartialLeft)  => expand(p, v).run
      case p @ (_:PartialRight) => expandRight(p, v)
    }
  }
  
  private def expandRight(px: PartialMatch, v: String): Trie = {
    Trie.ioGet(px.id) match {
      case None => println("RETRY -------"); expandRight(px, v) //retry if the next node is not yet in the database
      case Some(node) =>
        node.children.checkPrefix(px.t) match {
          case Hit(_)            => node.put("", v)
          case (_: PartialMatch) => node.put(px.t, v)
          case Miss(_)           => {
            if (node.children.isEmpty) expand(px, v).run
            else node.append(px.t, v).run
          }
        }
    }
  }

  private def expand(px: PartialMatch, v: String): IO[Trie] = for {
    leaf   <- IO.Insert(Node(v))
    node   <- IO.Insert(Node(SuffixMap(px.t -> leaf.id, px.h._2 -> px.id)))
    parent <- IO.Update(Node(id, children - px.suffix + (px.h._1 -> node.id)))
  } yield leaf

  private def append(s: String, v: String): IO[Trie] = for {
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
  val Terminator = "$"

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
      if (depth == k.length) t.tKey match {
        case None     => t.get(k)
        case Some(id) => ioGet(id).flatMap(_.get(k))
      }
      else t.children.checkPrefix(s) match {
        case Miss(_)            => None
        case Hit(id)            => ioGet(id).flatMap(x => explore(x, "", depth + s.length))
        case (px: PartialMatch) => ioGet(px.id).flatMap(x => explore(x, s substring px.h._1.length, depth + px.h._1.length))
      }
    }
    if (ns.isEmpty || k.isEmpty) None
    else explore(root(ns), k, 0)
  }
}
