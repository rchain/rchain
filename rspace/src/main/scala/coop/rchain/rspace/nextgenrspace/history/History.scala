package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{discriminated, provide, uint2}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.shared.AttemptOps._
import History._

import scala.annotation.tailrec

sealed trait Trie
sealed trait NonEmptyTrie                                   extends Trie
case object EmptyTrie                                       extends Trie
final case class Skip(affix: ByteVector, hash: TriePointer) extends NonEmptyTrie
final case class Node(hash: PointerBlockPointer)            extends NonEmptyTrie
final case class Leaf(hash: LeafPointer)                    extends NonEmptyTrie

final case class TriePath(partialPath: Vector[Trie], conflicting: Option[Trie], path: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(partialPath = this.partialPath :+ t, path = this.path ++ affix)
}

object TriePath {
  def root(conflicting: Trie): TriePath =
    TriePath(Vector(), Some(conflicting), Nil)

  def start(pp: Vector[Trie], path: KeyPath): TriePath =
    TriePath(pp, None, path)

  def empty: TriePath = TriePath(Vector(), None, Nil)
}

final case class History[K](
    root: Blake2b256Hash,
    historyStore: HistoryStore,
    pointerBlockStore: PointerBlockStore
)(implicit codecK: Codec[K]) {

  private def storePointerBlock(pb: PointerBlock): Node = {
    val hash = PointerBlock.hash(pb)
    pointerBlockStore.put(hash, pb)
    Node(hash)
  }

  private def divideSkip(
      incomingTail: KeyPath,
      incomingIdx: Byte,
      incomingTrie: Trie,
      existingTail: KeyPath,
      existingIdx: Byte,
      existingPointer: Blake2b256Hash,
      prefixPath: KeyPath
  ): (List[Trie], Trie) = {
    val maybeSkipToIncoming = skipOrValue(incomingTail, incomingTrie)
    val maybeSkipToExisting = skipOrFetch(existingTail, existingPointer, historyStore.get)
    val pointerBlock =
      PointerBlock.create(
        (toInt(incomingIdx), maybeSkipToIncoming),
        (toInt(existingIdx), maybeSkipToExisting)
      )
    val nodeToPB        = storePointerBlock(pointerBlock)
    val maybeSkipToNode = skipOrValue(prefixPath, nodeToPB)
    (
      incomingTrie :: maybeSkipToExisting :: maybeSkipToIncoming :: nodeToPB :: maybeSkipToNode :: Nil,
      maybeSkipToNode
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO consider an indexedseq
  private def rebalanceInsert(
      newLeaf: Trie,
      currentPath: KeyPath,
      partialTrie: TriePath
  ): List[Trie] = {

    def go(t: Trie, uncommon: KeyPath, existingTrie: TriePath): List[Trie] =
      existingTrie match {
        case TriePath(Vector(), None, Nil) => // kickstart a trie
          t :: skip(t, uncommon) :: Nil

        case TriePath(Vector(Skip(a, _), Leaf(_)), None, common)
            if common == a.toSeq.toList => // update 1 element trie
          t :: skip(t, common) :: Nil

        case TriePath(Vector(), Some(Skip(affix, existingPointer)), Nil) => // split skip at root
          val affixBytes                   = affix.toSeq.toList
          val prefixPath                   = commonPrefix(affixBytes, currentPath)
          val incomingHead :: incomingTail = currentPath.drop(prefixPath.size)
          val existingHead :: existingTail = affixBytes.drop(prefixPath.size)
          divideSkip(
            incomingTail,
            incomingHead,
            newLeaf,
            existingTail,
            existingHead,
            existingPointer,
            prefixPath
          )._1

        case TriePath(elems :+ Node(ptr), Some(Skip(affix, existingPointer)), common) => // split existing skip
          val incomingPath                 = uncommon.drop(common.size)
          val affixBytes                   = affix.toSeq.toList
          val prefixPath                   = commonPrefix(incomingPath, affixBytes)
          val incomingHead :: incomingTail = incomingPath.drop(prefixPath.size)
          val existingHead :: existingTail = affixBytes.drop(prefixPath.size)

          val (newElements, topTrie) = divideSkip(
            incomingTail,
            incomingHead,
            newLeaf,
            existingTail,
            existingHead,
            existingPointer,
            prefixPath
          )

          val updatedPointerBlock =
            pointerBlockStore.get(ptr).get.updated((toInt(common.last), topTrie) :: Nil)
          val nodeToExistingPB = storePointerBlock(updatedPointerBlock)
          rehash(common.init, elems, nodeToExistingPB, newElements)

        case TriePath(elems :+ Node(ptr), None, common) => // add to existing node
          val pastPB = pointerBlockStore.get(ptr).get
          val key    = uncommon.drop(common.size)
          val s      = skipOrValue(key, newLeaf)
          val newPB  = pastPB.updated((toInt(common.last), s) :: Nil)
          val node   = storePointerBlock(newPB)
          rehash(common.init, elems, node, newLeaf :: s :: Nil)
        case TriePath(elems :+ (_: Leaf), _, path) => // update value
          rehash(path, elems, newLeaf, Nil)
        case _ => throw new RuntimeException("malformed trie")
      }
    go(newLeaf, currentPath, partialTrie)
  }

  @tailrec
  private def rehash(
      path: KeyPath,
      rest: Vector[Trie],
      lastSeen: Trie,
      acc: List[Trie]
  ): List[Trie] =
    rest match {
      case head :+ Node(ptr) =>
        val pointerBlock =
          pointerBlockStore.get(ptr).get.updated((toInt(path.last), lastSeen) :: Nil)
        val nodeToPB = storePointerBlock(pointerBlock)
        rehash(path.init, head, nodeToPB, acc :+ lastSeen)
      case head :+ Skip(affix, _) =>
        rehash(
          path.dropRight(affix.size.toInt),
          head,
          Skip(affix, Trie.hash(lastSeen)),
          acc :+ lastSeen
        )
      case Vector() => acc :+ lastSeen
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  @tailrec
  private def rebalanceDelete(path: KeyPath, rest: Vector[Trie], acc: Option[Trie]): List[Trie] =
    (path, rest, acc) match {
      case (currentKey, init :+ Skip(affix, _), None) => // remove empty skip
        rebalanceDelete(currentKey.dropRight(affix.size.toInt), init, None)

      case (currentKey, init :+ Skip(affix, _), Some(Skip(eaffix, ep))) => // merge skips
        rehash(currentKey.dropRight(affix.size.toInt), init, Skip(affix ++ eaffix, ep), Nil)

      case (keyInit :+ keyLast, init :+ Node(p), None) => // handle pointer block
        pointerBlockStore.get(p) match {
          case Some(pointerBlock) if pointerBlock.countNonEmpty == 2 =>
            // pointerBlock contains 1 more element, find it, wrap in skip and re-balance
            def getOther =
              pointerBlock
                .updated((toInt(keyLast), EmptyTrie) :: Nil)
                .toVector
                .zipWithIndex
                .filter(v => v._1 != EmptyTrie)
                .head
            val (other, idx) = getOther
            rebalanceDelete(keyInit, init, Some(pointTo(toByte(idx), other)))
          case Some(pb) => // pb contains 3+ elements, drop one, rehash
            val updated = pb.updated((toInt(keyLast), EmptyTrie) :: Nil)
            val node    = storePointerBlock(updated)
            rehash(keyInit, init, node, Nil)
          case None => throw new RuntimeException("malformed trie")
        }

      case (key, _ :+ (_: Node), Some(s: Skip)) =>
        rehash(key, rest, s, Nil)

      case (Nil, Vector(), Some(t)) => t :: Nil

      case (Nil, Vector(), None) => EmptyTrie :: Nil
    }

  def process(actions: List[HistoryAction[K]]): History[K] = {
    // TODO this is an intermediate step to reproduce all the trie behavior
    // will evolve to a fold based implementation with partial tries
    val newRoot = actions.foldLeft(this.root) {
      case (currentRoot, InsertAction(k, value)) =>
        val currentPath = asBytes(k)
        val (_, path)   = traverseTrie(currentPath, currentRoot)
        val elements    = rebalanceInsert(Leaf(value), currentPath, path)
        historyStore.put(elements)
        Trie.hash(elements.last)

      case (currentRoot, DeleteAction(k)) =>
        val currentPath   = asBytes(k)
        val (_, triePath) = traverseTrie(currentPath, currentRoot)
        val elements = triePath match {
          case TriePath(elems :+ (_: Leaf), None, path) if currentPath == path =>
            rebalanceDelete(currentPath, elems, None)
          case _ =>
            Nil
        }
        historyStore.put(elements)
        elements.lastOption.map(Trie.hash).getOrElse(currentRoot)
    }

    this.copy(root = newRoot)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private[history] def traverseTrie(key: KeyPath, start: Blake2b256Hash): (Trie, TriePath) = {
    @tailrec
    def traverse(t: Trie, currentPath: List[Byte], path: TriePath): (Trie, TriePath) =
      (t, currentPath) match {
        case (EmptyTrie, _) => (EmptyTrie, path)
        case (l: Leaf, _)   => (t, path append (Nil, l))
        case (s @ Skip(affix, p), _) if currentPath.startsWith(affix.toSeq) =>
          traverse(
            historyStore.get(p),
            currentPath.drop(affix.size.toInt),
            path append (affix.toSeq.toList, s)
          )
        case (s: Skip, _) => //not a prefix
          (EmptyTrie, path.copy(conflicting = Some(s)))
        case (node @ Node(ptr), h :: tail) =>
          pointerBlockStore.get(ptr) match {
            case Some(pb) => traverse(pb.toVector(toInt(h)), tail, path append (h :: Nil, node))
            case None     => throw new RuntimeException("malformed trie")
          }
        case _ => throw new RuntimeException("malformed trie")
      }
    traverse(historyStore.get(start), key, TriePath.empty)
  }

  private[history] def traverseTrie(key: KeyPath): (Trie, TriePath) =
    traverseTrie(key, root)
}

object History {

  def skipOrFetch(path: List[Byte], pointer: Blake2b256Hash, fetch: Blake2b256Hash => Trie): Trie =
    if (path.isEmpty) {
      fetch(pointer)
    } else {
      Skip(ByteVector(path), pointer)
    }

  def skipOrValue(path: List[Byte], t: Trie): Trie =
    if (path.isEmpty) {
      t
    } else {
      skip(t, path)
    }

  // this mapping is kept explicit on purpose
  @inline
  def toInt(b: Byte): Int =
    java.lang.Byte.toUnsignedInt(b)

  // this mapping is kept explicit on purpose
  @inline
  def toByte(i: Int): Byte =
    i.toByte

  def commonPrefix(l: List[Byte], r: List[Byte]): List[Byte] =
    (l.view, r.view).zipped.takeWhile { case (ll, rr) => ll == rr }.unzip._1.toList

  def skip(t: Trie, affix: List[Byte]): Skip =
    Skip(ByteVector(affix), Trie.hash(t))

  def pointTo(idx: Byte, t: Trie): Trie =
    t match {
      case Skip(affix, p) => Skip(idx +: affix, p)
      case n: Node        => Skip(ByteVector(idx), Trie.hash(n))
      case l: Leaf        => Skip(ByteVector(idx), Trie.hash(l))
      case EmptyTrie      => EmptyTrie
    }

  type KeyPath = List[Byte]

  def asBytes[K](k: K)(implicit codecK: Codec[K]): List[Byte] =
    codecK.encode(k).get.bytes.toSeq.toList

  type PointerBlockPointer = Blake2b256Hash
  type TriePointer         = Blake2b256Hash
  type LeafPointer         = Blake2b256Hash

  object Trie {
    def hash(trie: Trie)(implicit codecTrie: Codec[Trie]): Blake2b256Hash =
      codecTrie
        .encode(trie)
        .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteVector))
        .get
  }

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
