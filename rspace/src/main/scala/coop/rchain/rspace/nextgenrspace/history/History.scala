package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{discriminated, provide, uint2, vectorOfN}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.shared.AttemptOps._
import History._
import cats.Applicative
import Trie.{LeafPointer, PointerBlockPointer, TriePointer}

trait History[F[_]] {
  def process(actions: List[HistoryAction]): F[History[F]]
  def root: Blake2b256Hash
  def find(key: KeyPath): F[(Trie, Vector[Trie])]
  def close(): F[Unit]
  def reset(root: Blake2b256Hash): History[F]
}

object History {

  val emptyRoot: Trie = EmptyTrie
  val emptyRootHash: Blake2b256Hash =
    Trie.hash(emptyRoot)

  def skipOrFetch[F[_]: Applicative](
      path: KeyPath,
      pointer: Blake2b256Hash,
      fetch: Blake2b256Hash => F[Trie]
  ): F[Trie] =
    if (path.isEmpty) {
      fetch(pointer)
    } else {
      Applicative[F].pure(Skip(ByteVector(path), pointer))
    }

  def skipOrValue(path: KeyPath, t: Trie): Trie =
    if (path.isEmpty) {
      t
    } else {
      skip(t, path)
    }

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

  def skip(t: Trie, affix: KeyPath): Skip =
    Skip(ByteVector(affix), Trie.hash(t))

  def pointTo(idx: Byte, t: Trie): Trie =
    t match {
      case Skip(affix, p) => Skip(idx +: affix, p)
      case n: Node        => Skip(ByteVector(idx), Trie.hash(n))
      case l: Leaf        => Skip(ByteVector(idx), Trie.hash(l))
      case EmptyTrie      => EmptyTrie
    }

  type KeyPath = Seq[Byte]

}

sealed trait Trie
sealed trait NonEmptyTrie                                   extends Trie
case object EmptyTrie                                       extends Trie
final case class Skip(affix: ByteVector, hash: TriePointer) extends NonEmptyTrie
final case class Node(hash: PointerBlockPointer)            extends NonEmptyTrie
final case class Leaf(hash: LeafPointer)                    extends NonEmptyTrie

object Trie {

  type PointerBlockPointer = Blake2b256Hash
  type TriePointer         = Blake2b256Hash
  type LeafPointer         = Blake2b256Hash

  def hash(trie: Trie): Blake2b256Hash =
    codecTrie
      .encode(trie)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteVector))
      .get

  private val codecNode  = Blake2b256Hash.codecBlake2b256Hash
  private val codecLeaf  = Blake2b256Hash.codecBlake2b256Hash
  private val codecSkip  = codecByteVector :: Blake2b256Hash.codecBlake2b256Hash
  private val codecEmpty = provide(EmptyTrie)

  implicit def codecTrie: Codec[Trie] =
    discriminated[Trie]
      .by(uint2)
      .subcaseP(0) {
        case n: Node => n
      }(codecNode.as[Node])
      .subcaseP(1) {
        case s: Skip => s
      }(codecSkip.as[Skip])
      .subcaseP(2) {
        case emptyTrie: EmptyTrie.type => emptyTrie
      }(codecEmpty)
      .subcaseP(3) {
        case l: Leaf => l
      }(codecLeaf.as[Leaf])
}

final case class PointerBlock private (toVector: Vector[Trie]) {
  def updated(tuples: List[(Int, Trie)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def countNonEmpty: Int = toVector.count(_ != EmptyTrie)
}

object PointerBlock {
  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyTrie))

  def create(first: (Int, Trie)): PointerBlock =
    PointerBlock.create().updated(List(first))

  def create(first: (Int, Trie), second: (Int, Trie)): PointerBlock =
    PointerBlock.create().updated(List(first, second))

  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      Trie.codecTrie
    ).as[PointerBlock]

  def hash(pb: PointerBlock): Blake2b256Hash =
    codecPointerBlock
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
      .get

  def unapply(arg: PointerBlock): Option[Vector[Trie]] = Option(arg.toVector)
}

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, Nil)
}
