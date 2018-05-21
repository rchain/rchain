package coop.rchain.rspace

import java.lang.{Byte => JByte}

import com.typesafe.scalalogging.Logger
import coop.rchain.shared.AttemptOps._
import scodec.Codec

import scala.annotation.tailrec

package object history {

  private val logger: Logger = Logger[this.type]

  @tailrec
  def getParents[T, K, V](store: ITrieStore[T, K, V],
                          txn: T,
                          path: Seq[Byte],
                          depth: Int,
                          curr: Trie[K, V],
                          acc: Seq[(Int, Node)] = Seq.empty): (Trie[K, V], Seq[(Int, Node)]) =
    curr match {
      case node @ Node(pointerBlock) =>
        val index: Int = JByte.toUnsignedInt(path(depth))
        pointerBlock.toVector(index) match {
          case None =>
            (curr, acc)
          case Some(nextHash) =>
            store.get(txn, nextHash) match {
              case None =>
                throw new LookupException(s"No node at $nextHash")
              case Some(next) =>
                getParents(store, txn, path, depth + 1, next, (index, node) +: acc)
            }
        }
      case leaf =>
        (leaf, acc)
    }

  def commonPrefix(a: Seq[Byte], b: Seq[Byte]): Seq[Byte] =
    a.zip(b).takeWhile { case (l, r) => l == r }.map(_._1)

  def rehash[K, V](trie: Node, nodes: Seq[(Int, Node)])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): Seq[(Blake2b256Hash, Trie[K, V])] =
    nodes.scanLeft((Trie.hash[K, V](trie), trie)) {
      case ((lastHash, _), (offset, Node(pb))) =>
        val node = Node(pb.updated(List((offset, Some(lastHash)))))
        (Trie.hash[K, V](node), node)
    }

  def insertTries[T, K, V](
      store: ITrieStore[T, K, V],
      txn: T,
      rehashedNodes: Seq[(Blake2b256Hash, Trie[K, V])]): Option[Blake2b256Hash] =
    rehashedNodes.foldLeft(None: Option[Blake2b256Hash]) {
      case (_, (hash, trie)) =>
        store.put(txn, hash, trie)
        Some(hash)
    }

  def insert[T, K, V](store: ITrieStore[T, K, V], key: K, value: V)(implicit
                                                                    codecK: Codec[K],
                                                                    codecV: Codec[V]): Unit = {
    // We take the current root hash, preventing other threads from operating on the Trie
    val currentRootHash: Blake2b256Hash = store.workingRootHash.take()
    store.withTxn(store.createTxnWrite()) { (txn: T) =>
      // Get the current root node
      store.get(txn, currentRootHash) match {
        case None =>
          throw new LookupException(s"No node at $currentRootHash")
        case Some(currentRoot) =>
          // Serialize and convert the key to a `Seq[Byte]`.  This becomes our "path" down the Trie.
          val encodedKeyNew = codecK.encode(key).map(_.bytes.toSeq).get
          // Create the new leaf and put it into the store
          val newLeaf     = Leaf(key, value)
          val newLeafHash = Trie.hash(newLeaf)
          store.put(txn, newLeafHash, newLeaf)
          // Using the path we created from the key, get the existing parents of the new leaf.
          val (tip, parents) = getParents(store, txn, encodedKeyNew, 0, currentRoot)
          tip match {
            // If the "tip" is the same as the new leaf, than the given (key, value) pair is
            // already in the Trie, so we put the rootHash back and continue
            case existingLeaf @ Leaf(_, _) if existingLeaf == newLeaf =>
              store.workingRootHash.put(currentRootHash)
              logger.debug(s"workingRootHash: ${store.workingRootHash.get}")
            // If the "tip" is an existing leaf, then we are in a situation where the new leaf
            // shares some common prefix with the existing leaf.
            case existingLeaf @ Leaf(ek, _) =>
              val encodedKeyExisting = codecK.encode(ek).map(_.bytes.toSeq).get
              val sharedPrefix       = commonPrefix(encodedKeyNew, encodedKeyExisting)
              val sharedPrefixLength = sharedPrefix.length
              val sharedPath         = sharedPrefix.drop(parents.length).reverse
              val newLeafIndex       = JByte.toUnsignedInt(encodedKeyNew(sharedPrefixLength))
              val existingLeafIndex  = JByte.toUnsignedInt(encodedKeyExisting(sharedPrefixLength))
              val hd = Node(
                PointerBlock
                  .create()
                  .updated(List((newLeafIndex, Some(newLeafHash)),
                                (existingLeafIndex, Some(Trie.hash[K, V](existingLeaf)))))
              )
              val emptyNode     = Node(PointerBlock.create())
              val emptyNodes    = sharedPath.map((b: Byte) => (JByte.toUnsignedInt(b), emptyNode))
              val nodes         = emptyNodes ++ parents
              val rehashedNodes = rehash[K, V](hd, nodes)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.workingRootHash.put(newRootHash)
              logger.debug(s"workingRootHash: ${store.workingRootHash.get}")
            // If the "tip" is an existing node, then we can add the new leaf's hash to the node's
            // pointer block and rehash.
            case Node(pb) =>
              val pathLength    = parents.length
              val newLeafIndex  = JByte.toUnsignedInt(encodedKeyNew(pathLength))
              val hd            = Node(pb.updated(List((newLeafIndex, Some(newLeafHash)))))
              val rehashedNodes = rehash[K, V](hd, parents)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.workingRootHash.put(newRootHash)
              logger.debug(s"workingRootHash: ${store.workingRootHash.get}")
          }
      }
    }
  }
}
