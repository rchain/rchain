package coop.rchain.trie
import org.mongodb.scala.bson.collection.immutable.Document
  
object scratch {
  println("Welcome to the Scala worksheet")

  // Example Rose Tree
  sealed trait LeafTree[+T]
  object LeafTree {
    def empty[T]: LeafTree[T] = EmptyLeaf
  }

  case class Node[T](children: List[LeafTree[T]]) extends LeafTree[T]
  case class Leaf[T](value: T) extends LeafTree[T]
  case object EmptyLeaf extends LeafTree[Nothing]

  // Current Trie
  abstract class Trie
  object Trie {
    def empty = EmptyTrie(Digest.sha256, None)
  }
  
  case class EmptyTrie(id: String, value: Option[String]) extends Trie
  case class NonEmptyTrie(suffixes: Vector[String], keys: Vector[String], value: Option[String]) extends Trie
  
   // Current Trie
  abstract class RTrie[+T]
  object RTrie {
    def empty[T]: Leaf[T] = EmptyLeaf
  }
  
  case class SuffixMap(suffixes: Vector[String], ids: Vector[String])
  case class Node(sm: SuffixMap) extends RTrie[T]
  case class Leaf(value: T) extends RTrie[T]
  case object EmptyLeaf extends LeafTree[Nothing]
}