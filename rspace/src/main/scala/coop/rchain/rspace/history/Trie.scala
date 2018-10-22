package coop.rchain.rspace.history

import coop.rchain.rspace.internal._
import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import coop.rchain.rspace.internal.codecByteVector

import scala.ref.WeakReference

sealed trait Pointer
sealed trait NonEmptyPointer extends Pointer {
  def hash: Blake2b256Hash
}

case class NodePointer(hash: Blake2b256Hash) extends NonEmptyPointer
case class LeafPointer(hash: Blake2b256Hash) extends NonEmptyPointer
case object EmptyPointer                     extends Pointer

sealed trait Trie[+K, +V]                                          extends Product with Serializable
final case class Leaf[K, V](key: K, value: V)                      extends HashedEntity with Trie[K, V]
final case class Node(pointerBlock: PointerBlock)                  extends HashedEntity with Trie[Nothing, Nothing]
final case class Skip(affix: ByteVector, pointer: NonEmptyPointer) extends HashedEntity with Trie[Nothing, Nothing]

class HashedEntity {
  private[this] var memoizedData : WeakReference[(Blake2b256Hash, Array[Byte])] = WeakReference(null)

  //TODO: ys-pyrofex remove
//  def hash : Blake2b256Hash = memoizedData.get.get._1
//  def bytes : Array[Byte] = memoizedData.get.get._2

  def get : Option[(Blake2b256Hash, Array[Byte])] = memoizedData.get

  def initialize(f : => (Blake2b256Hash, Array[Byte])) : Blake2b256Hash = {
    val result = memoizedData.get
    result match {
      case Some((hash, _)) => hash
      case None =>
        val newValue = f
        memoizedData = WeakReference(newValue)
        newValue._1
    }
  }
}

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
      .subcaseP(2) {
        case (skip: Skip) => skip
      }((codecByteVector :: codecNonEmptyPointer).as[Skip])

  def hash[K, V](trie: Trie[K, V])(implicit codecK: Codec[K], codecV: Codec[V]): Blake2b256Hash =
    if(TrieCache.useCache) {
      trie.asInstanceOf[HashedEntity].initialize {
        codecTrie[K, V]
          .encode(trie)
          .map((vector: BitVector) => {
            val bytes = vector.toByteArray
            (Blake2b256Hash.create(bytes), bytes)
          }).get
      }
    } else {
      codecTrie[K, V]
        .encode(trie)
        .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
        .get
    }

  //TODO: ys-pyrofex remove
//  def encodeAndHash[K, V](trie: Trie[K, V])(implicit codecK: Codec[K], codecV: Codec[V]): (Array[Byte], Blake2b256Hash) =
//    codecTrie[K, V]
//      .encode(trie)
//      .map((vector: BitVector) => {
//        val bytes = vector.toByteArray
//        (bytes, Blake2b256Hash.create(bytes))
//      })
//      .get
}
