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

  /** Put a value at a given key
   *
   * @param key the key that should be updated
   * @param value the value to be associated with `key`
   * @return the leaf node (Trie) for the updated value
   */
  def put(key: String, value: String): Trie = {
    if (key.isEmpty) IO.Update(Node(id, children, value)).run
    else children.checkPrefix(key) match {
      case Hit(id)           => IO.Get(id).run.get.put("", value)
      case Miss(_)           => append(key, value).run
      case (px:Partial)      => expand(px, value).run
      case (px:PartialLeft)  => expand(px, value).run
      case (px:PartialRight) => expandRight(px, value)
    }
  }
  
  /** Retrieve a value at a given key
   *
   * @param key the key for lookup
   * @return Some(value) or None if not found
   */
  def get(key: String): Option[String] = {
    def explore(t: Trie, s: String, depth: Int): Option[String] = {
      if (depth == key.length) t.children.get(Trie.Terminator) match {
        case None     => t.v
        case Some(id) => IO.Get(id).run.flatMap(_.v)
      }
      else t.children.checkPrefix(s) match {
        case Miss(_)            => None
        case Hit(id)            => IO.Get(id).run.flatMap(explore(_, "", depth + s.length))
        case (px: PartialMatch) => IO.Get(px.id).run.flatMap(explore(_, s substring px.depth, depth + px.depth))
      }
    }
    explore(this, key, 0)
  }

  private def expandRight(px: PartialMatch, v: String): Trie = {
    IO.Get(px.id).run match {
      case None => expandRight(px, v) //retry if the next node is not yet in the database
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
      case None       => create
      case Some(root) => root
    }
  }
  
  def get(namespace: String, key: String): Option[String] = {
    if (namespace.isEmpty || key.isEmpty) None
    else root(namespace).get(key)
  }

  def put(namespace: String, key: String, value: String): Trie = root(namespace).put(key, value)
}
