package coop.rchain.rspace.history

import coop.rchain.crypto.codec.Base16
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History._
import coop.rchain.rspace.serializers.ScodecSerialize.{RichAttempt, _}
import scodec.bits.{BitVector, ByteVector}

/**
  * History definition represents key-value API for RSpace tuple space
  *
  * [[History]] contains only references to data stored on keys ([[KeyPath]]).
  *
  * [[ColdStoreInstances.ColdKeyValueStore]] holds full data referenced by [[LeafPointer]] in [[History]].
  */
trait History[F[_]] {

  /**
    * Insert/update/delete operations on the underlying Merkle tree (key-value store)
    */
  def process(actions: List[HistoryAction]): F[History[F]]

  /**
    * Read operation on the Merkle tree
    */
  def find(key: KeyPath): F[(TriePointer, Vector[Trie])]

  /**
    * Get the root of the Merkle tree
    */
  def root: Blake2b256Hash

  /**
    * Returns History with specified with root pointer
    */
  def reset(root: Blake2b256Hash): History[F]
}

object History {

  val emptyRoot: Trie               = EmptyTrie
  private[this] def encodeEmptyRoot = codecTrie.encode(emptyRoot).getUnsafe.toByteVector
  val emptyRootHash: Blake2b256Hash = Blake2b256Hash.create(encodeEmptyRoot)

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toInt(b: Byte): Int =
    java.lang.Byte.toUnsignedInt(b)

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toByte(i: Int): Byte =
    i.toByte

  def commonPrefix(l: KeyPath, r: KeyPath): KeyPath =
    (l.view, r.view).zipped.takeWhile { case (ll, rr) => ll == rr }.map(_._1).toSeq

  type KeyPath = Seq[Byte]
}

/*
 * Type definitions for Merkle Trie implementation (History)
 */

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
    */
  def hash(trie: Trie): Blake2b256Hash =
    trie match {
      case pb: PointerBlock  => pb.hash
      case s: Skip           => s.hash
      case _: EmptyTrie.type => History.emptyRootHash
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
  def empty: TriePath = TriePath(Vector(), None, Nil)
}
