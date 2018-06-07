package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOps._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

sealed trait Child
case class NodeChild(hash: Blake2b256Hash) extends Child
case class LeafChild(hash: Blake2b256Hash) extends Child
//case class Extension(affix: ByteVector, hash: Blake2b256Hash) extends Child
case object EmptyChild extends Child

sealed trait Trie[+K, +V] extends Product with Serializable
//final case class Root(child: Child)               extends Trie[Nothing, Nothing]
final case class Leaf[K, V](key: K, value: V)     extends Trie[K, V]
final case class Node(pointerBlock: PointerBlock) extends Trie[Nothing, Nothing]

object Trie {

  def create[K, V](): Trie[K, V] = Node(PointerBlock.create())

  implicit def codecTrie[K, V](implicit codecK: Codec[K], codecV: Codec[V]): Codec[Trie[K, V]] =
    discriminated[Trie[K, V]]
      .by(uint8)
      .subcaseP(0) {
        case (leaf: Leaf[K, V]) => leaf
      }((codecK :: codecV).as[Leaf[K, V]])
      .subcaseP(1) {
        case (node: Node) => node
      }(PointerBlock.codecPointerBlock.as[Node])

  def hash[K, V](trie: Trie[K, V])(implicit codecK: Codec[K], codecV: Codec[V]): Blake2b256Hash =
    codecTrie[K, V]
      .encode(trie)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
      .get
}
