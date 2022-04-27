package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.serializers.ScodecSerialize.{codecTrie, RichAttempt}
import coop.rchain.shared.Base16
import scodec.bits.{BitVector, ByteVector}

sealed trait Trie

sealed trait NonEmptyTrie extends Trie

case object EmptyTrie extends Trie

final case class Skip(affix: ByteVector, ptr: ValuePointer) extends NonEmptyTrie {
  lazy val encoded: BitVector = codecTrie.encode(this).getUnsafe

  lazy val hash: Blake2b256Hash = Blake2b256Hash.create(encoded.toByteVector)

  override def toString: String =
    s"Skip(${hash}, ${affix.toHex}\n  ${ptr})"
}

final case class PointerBlock private (toVector: Vector[TriePointer]) extends NonEmptyTrie {
  def updated(tuples: List[(Int, TriePointer)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def countNonEmpty: Int = toVector.count(_ != EmptyPointer)

  lazy val encoded: BitVector = codecTrie.encode(this).getUnsafe

  lazy val hash: Blake2b256Hash = Blake2b256Hash.create(encoded.toByteVector)

  override def toString: String = {
    // TODO: this is difficult to visualize, maybe XML representation would be useful?
    val pbs =
      toVector.zipWithIndex
        .filter { case (v, _) => v != EmptyPointer }
        .map { case (v, n) => s"<$v, ${Base16.encode(Array(n.toByte))}>" }
        .mkString(",\n  ")
    s"PB(${hash}\n  $pbs)"
  }
}

object Trie {

  /**
    * Creates hash of Merkle Trie
    *
    * TODO: Fix encoding to use codec for the whole [[Trie]] and not for specific inherited variant.
    */
  def hash(trie: Trie): Blake2b256Hash =
    trie match {
      case pb: PointerBlock  => pb.hash
      case s: Skip           => s.hash
      case _: EmptyTrie.type => RadixTree.emptyRootHash
    }
}

object PointerBlock {
  val length = 256

  val empty: PointerBlock = new PointerBlock(Vector.fill(length)(EmptyPointer))

  def apply(first: (Int, TriePointer), second: (Int, TriePointer)): PointerBlock =
    PointerBlock.empty.updated(List(first, second))

  def unapply(arg: PointerBlock): Option[Vector[TriePointer]] = Option(arg.toVector)
}

/*
 * Trie pointer definitions
 */

sealed trait TriePointer

sealed trait NonEmptyTriePointer extends TriePointer {
  def hash: Blake2b256Hash
}

case object EmptyPointer extends TriePointer

sealed trait ValuePointer extends NonEmptyTriePointer

final case class LeafPointer(hash: Blake2b256Hash) extends ValuePointer

final case class SkipPointer(hash: Blake2b256Hash) extends NonEmptyTriePointer

final case class NodePointer(hash: Blake2b256Hash) extends ValuePointer

/*
 * Trie path used as a helper for traversal in History implementation
 */

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, KeyPath.empty)
}
