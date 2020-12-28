package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.{Attempt, Codec}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{discriminated, provide, uint, uint2, vectorOfN}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.shared.AttemptOps._
import History._
import coop.rchain.crypto.codec.Base16

trait History[F[_]] {
  def process(actions: List[HistoryAction]): F[History[F]]
  def root: Blake2b256Hash
  def find(key: KeyPath): F[(TriePointer, Vector[Trie])]
  def close(): F[Unit]
  def reset(root: Blake2b256Hash): History[F]
}

object History {

  val emptyRoot: Trie               = EmptyTrie
  private[this] def encodeEmptyRoot = Trie.codecTrie.encode(emptyRoot).get.toByteVector
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

  def skip(pb: PointerBlock, affix: KeyPath): Trie =
    Skip(ByteVector(affix), NodePointer(pb.hash))

  type KeyPath = Seq[Byte]
}

sealed trait Trie
sealed trait NonEmptyTrie extends Trie
case object EmptyTrie     extends Trie
final case class Skip(affix: ByteVector, ptr: ValuePointer) extends NonEmptyTrie {
  lazy val encoded: BitVector = Trie.codecSkip.encode(this).get

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

  lazy val encoded: BitVector = PointerBlock.codecPointerBlock.encode(this).get

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

sealed trait TriePointer
sealed trait NonEmptyTriePointer extends TriePointer {
  def hash: Blake2b256Hash
}
case object EmptyPointer  extends TriePointer
sealed trait ValuePointer extends NonEmptyTriePointer

final case class LeafPointer(hash: Blake2b256Hash) extends ValuePointer
final case class SkipPointer(hash: Blake2b256Hash) extends NonEmptyTriePointer
final case class NodePointer(hash: Blake2b256Hash) extends ValuePointer

object Trie {
  def hash(trie: Trie): Blake2b256Hash =
    trie match {
      case pb: PointerBlock  => pb.hash
      case s: Skip           => s.hash
      case _: EmptyTrie.type => History.emptyRootHash
    }

  val codecSkip: Codec[Skip] = (codecByteVector :: codecTrieValuePointer).as[Skip]

  val memoizingSkipCodec: Codec[Skip] =
    Codec.apply((s: Skip) => Attempt.successful(s.encoded), codecSkip.decode)

  val memoizingPointerBlockCodec: Codec[PointerBlock] =
    Codec.apply(
      (s: PointerBlock) => Attempt.successful(s.encoded),
      PointerBlock.codecPointerBlock.decode
    )

  val codecTrie: Codec[Trie] =
    discriminated[Trie]
      .by(uint2)
      .subcaseP(0) {
        case e: EmptyTrie.type => e
      }(provide(EmptyTrie))
      .subcaseP(1) {
        case s: Skip => s
      }(memoizingSkipCodec)
      .subcaseP(2) {
        case pb: PointerBlock => pb
      }(memoizingPointerBlockCodec)

  implicit def codecTriePointer: Codec[TriePointer] =
    discriminated[TriePointer]
      .by(uint2)
      .subcaseP(0) {
        case p: EmptyPointer.type => p
      }(provide(EmptyPointer))
      .subcaseP(1) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[LeafPointer])
      .subcaseP(2) {
        case p: SkipPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[SkipPointer])
      .subcaseP(3) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[NodePointer])

  implicit def codecTrieValuePointer: Codec[ValuePointer] =
    discriminated[ValuePointer]
      .by(uint(1))
      .subcaseP(0) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[LeafPointer])
      .subcaseP(1) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[NodePointer])

}

object PointerBlock {
  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyPointer))

  def create(first: (Int, TriePointer)): PointerBlock =
    PointerBlock.create().updated(List(first))

  def create(first: (Int, TriePointer), second: (Int, TriePointer)): PointerBlock =
    PointerBlock.create().updated(List(first, second))

  // consider using zlib
  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      Trie.codecTriePointer
    ).as[PointerBlock]

  def unapply(arg: PointerBlock): Option[Vector[TriePointer]] = Option(arg.toVector)
}

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, Nil)
}
