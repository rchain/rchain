/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import scala.collection.immutable.Vector
import org.mongodb.scala.bson.Document

abstract class Trie {
  val size: Int
  val isFull: Boolean
  val suffixes: Vector[String]
  val keys: Vector[String]
  val value: Option[String]
  val id: String
  val db = MongoDB

  def get(s: String): Option[String] = {
    val suffix = s(0).toString
    println(s"suffix: $suffix")
    println(s"suffixes: $suffixes")
    println(s"keys: $keys")
    if (s.isEmpty) value
    else getId(s(0).toString) match {
      case (i, None) if i == -1 => println(s"Miss: $i"); None
      case (_, Some(x))         => println(s"Hit: $x"); Trie.getNode(x).get(s substring 1)
    }
  }

  def put(s: String, v: String): Trie = {
    if (s.length == 1) addLeaf(s, Some(v))
    else {
      val suffix = s(0).toString
      val node = getId(suffix) match {
        case (i, None) if i == -1 => println(s"Miss: $suffix"); expand(suffix) 
        case (_, Some(id))        => println(s"Hit: $id"); Trie.getNode(id) 
      }
      node.put(s substring 1, v)
    }
  }
  
  def getId(s: String): (Int, Option[String]) = (suffixes.indexOf(s), keys.lift(suffixes.indexOf(s)))
  
  def getSuffix(k: String): (Int, String) = (keys.indexOf(k), suffixes(keys.indexOf(k)))
  
  def setId(s: String, k: String): (Vector[String], Vector[String])

  private def expand(s: String): Trie = {
    val child = putNode(Trie.empty)
    val (sx, kx) = setId(s, child.id)
    replace(this, Trie(sx, kx, value))
    child
  }
  
  private def addLeaf(s: String, v: Option[String]): Trie = {
    val leaf = putNode(Trie.leaf(v))   
    val (sx, kx) = setId(s, leaf.id)   
    replace(this, Trie(sx, kx, value))  
    leaf
  }
  
  private def replace(stale: Trie, fresh: Trie): Unit = {
    val sp = stale.parent
    println(s"Stale Parent: $sp")
    db.delete(stale.id) 
    putNode(fresh)
    if (sp != None) {
      val parent = sp.get
      val (sx, kx) = setId(parent.getSuffix(stale.id)._2, fresh.id) 
      replace(parent, Trie(sx, kx, parent.value))
    }
  }

  private def parent: Option[Trie] = db.getParent(id)
  
  private def putNode(t: Trie): Trie = {
    db.put(t); t
  }
}

case class EmptyTrie(id: String, value: Option[String]) extends Trie {
  val size = 0
  val isFull = true
  val suffixes = Vector.empty
  val keys = Vector.empty

  def setId(s: String, k: String): (Vector[String], Vector[String]) = (suffixes :+ s, keys :+ k)
}

case class NonEmptyTrie(suffixes: Vector[String], keys: Vector[String], value: Option[String]) extends Trie {
  val size: Int = suffixes.size
  val isFull = size >= 16
  lazy val id: String = Digest.sha256(suffixes.hashCode.toString)

  def setId(s: String, k: String): (Vector[String], Vector[String]) = {
    val si = suffixes.indexOf(s)
    println(s"si: $si")
    val sx = si match {
      case _ if si == -1 => suffixes :+ s
      case _             => suffixes
    }
    println(s"sx: $sx")
    val debug = sx.indexOf(s)
    println(s"sx indexOf(s) $debug")
    val kx = si match {
      case _ if si == -1 => keys.updated(sx.indexOf(s), k)
      case _             => keys.updated(si, k)
    }
    println(s"keys: $keys")
    println(s"kx: $kx")
    (sx, kx)
  }
}

object Trie {
  case class Sha(val underlying: String) extends AnyVal
  case class RSON(val underlying: String) extends AnyVal
  
  def empty = EmptyTrie(Digest.sha256, None)
  
  def leaf(value: Option[String]) = EmptyTrie(Digest.sha256, value)

  def apply(sx: Vector[String], kx: Vector[String], v: Option[String]): Trie = NonEmptyTrie(sx, kx, v)

  def apply(key: String): Trie = MongoDB.getNode(key)

  def getNode(id: String): Trie = MongoDB.getNode(id)
}