package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats.Eq
import cats.instances.byte._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.traverse._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib.seq._
import coop.rchain.shared.AttemptOps._
import scodec.Codec

import scala.annotation.tailrec
import scala.collection.immutable

package object history {

  private val logger: Logger = Logger[this.type]

  def initialize[T, K, V](store: ITrieStore[T, K, V])(implicit
                                                      codecK: Codec[K],
                                                      codecV: Codec[V]): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.getRoot(txn) match {
        case None =>
          val root     = Trie.create[K, V]()
          val rootHash = Trie.hash(root)
          store.put(txn, rootHash, root)
          store.putRoot(txn, rootHash)
          logger.debug(s"workingRootHash: $rootHash")
        case Some(_) =>
          ()
      }
    }

  def lookup[T, K, V](store: ITrieStore[T, K, V], key: K)(implicit codecK: Codec[K]): Option[V] = {
    val path = codecK.encode(key).map(_.bytes.toSeq).get
    @tailrec
    def loop(txn: T, depth: Int, curr: Trie[K, V]): Option[V] =
      curr match {
        case Node(pointerBlock) =>
          val index: Int = JByte.toUnsignedInt(path(depth))
          // We use an explicit match here instead of flatMapping in order to make this function
          // tail-recursive
          pointerBlock.toVector(index) match {
            case EmptyPointer =>
              None
            case NodePointer(hash: Blake2b256Hash) =>
              store.get(txn, hash) match {
                case Some(next) => loop(txn, depth + 1, next)
                case None       => throw new LookupException(s"No node at $hash")
              }

            case LeafPointer(hash: Blake2b256Hash) =>
              store.get(txn, hash) match {
                case Some(next) => loop(txn, depth + 1, next)
                case _          => throw new LookupException(s"No node at $hash")
              }
          }
        case Leaf(lk, lv) if key == lk =>
          Some(lv)
        case Leaf(_, _) =>
          None
      }

    store.withTxn(store.createTxnRead()) { (txn: T) =>
      for {
        currentRootHash <- store.getRoot(txn)
        currentRoot     <- store.get(txn, currentRootHash)
        res             <- loop(txn, 0, currentRoot)
      } yield res
    }
  }

  def lookup[T, K, V](store: ITrieStore[T, K, V], keys: immutable.Seq[K])(
      implicit codecK: Codec[K]): Option[immutable.Seq[V]] =
    keys.traverse[Option, V]((k: K) => lookup(store, k))

  @tailrec
  private[this] def getParents[T, K, V](
      store: ITrieStore[T, K, V],
      txn: T,
      path: Seq[Byte],
      depth: Int,
      curr: Trie[K, V],
      acc: Seq[(Int, Node)] = Seq.empty): (Trie[K, V], Seq[(Int, Node)]) =
    curr match {
      case node @ Node(pointerBlock) =>
        val index: Int = JByte.toUnsignedInt(path(depth))
        pointerBlock.toVector(index) match {
          case EmptyPointer =>
            (curr, acc)
          case NodePointer(nextHash) =>
            store.get(txn, nextHash) match {
              case None =>
                throw new LookupException(s"No node at $nextHash")
              case Some(next) =>
                getParents(store, txn, path, depth + 1, next, (index, node) +: acc)
            }
          case LeafPointer(nextHash) =>
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

  // TODO(ht): make this more efficient
  private[this] def commonPrefix[A: Eq](a: Seq[A], b: Seq[A]): Seq[A] =
    a.zip(b).takeWhile { case (l, r) => l === r }.map(_._1)

  private[this] def rehash[K, V](trie: Node, nodes: Seq[(Int, Node)])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): Seq[(Blake2b256Hash, Trie[K, V])] =
    nodes.scanLeft((Trie.hash[K, V](trie), trie)) {
      case ((lastHash, _), (offset, Node(pb))) =>
        val node = Node(pb.updated(List((offset, NodePointer(lastHash)))))
        (Trie.hash[K, V](node), node)
    }

  private[this] def insertTries[T, K, V](
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
                                                                    codecV: Codec[V]): Unit =
    store.withTxn(store.createTxnWrite()) { (txn: T) =>
      // Get the current root hash
      val currentRootHash: Blake2b256Hash =
        store.getRoot(txn).getOrElse(throw new InsertException("could not get root"))
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
            // If the "tip" is the same as the new leaf, then the given (key, value) pair is
            // already in the Trie, so we put the rootHash back and continue
            case existingLeaf @ Leaf(_, _) if existingLeaf == newLeaf =>
              logger.debug(s"workingRootHash: $currentRootHash")
            // If the "tip" is an existing leaf with a different key than the new leaf, then
            // we are in a situation where the new leaf shares some common prefix with the
            // existing leaf.
            case existingLeaf @ Leaf(ek, _) if key != ek =>
              val encodedKeyExisting = codecK.encode(ek).map(_.bytes.toSeq).get
              val sharedPrefix       = commonPrefix(encodedKeyNew, encodedKeyExisting)
              val sharedPrefixLength = sharedPrefix.length
              val sharedPath         = sharedPrefix.drop(parents.length).reverse
              val newLeafIndex       = JByte.toUnsignedInt(encodedKeyNew(sharedPrefixLength))
              val existingLeafIndex  = JByte.toUnsignedInt(encodedKeyExisting(sharedPrefixLength))
              val hd = Node(
                PointerBlock
                  .create()
                  .updated(List((newLeafIndex, LeafPointer(newLeafHash)),
                                (existingLeafIndex, LeafPointer(Trie.hash[K, V](existingLeaf)))))
              )
              val emptyNode     = Node(PointerBlock.create())
              val emptyNodes    = sharedPath.map((b: Byte) => (JByte.toUnsignedInt(b), emptyNode))
              val nodes         = emptyNodes ++ parents
              val rehashedNodes = rehash[K, V](hd, nodes)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            // If the "tip" is an existing leaf with the same key as the new leaf, but the
            // existing leaf and new leaf have different values, then we are in the situation
            // where we are "updating" an existing leaf
            case Leaf(ek, ev) if key == ek && value != ev =>
              // Update the pointer block of the immediate parent at the given index
              // to point to the new leaf instead of the existing leaf
              val (hd, tl) = parents match {
                case (idx, Node(pointerBlock)) +: remaining =>
                  (Node(pointerBlock.updated(List((idx, LeafPointer(newLeafHash))))), remaining)
                case Seq() =>
                  throw new InsertException("A leaf had no parents")
              }
              val rehashedNodes = rehash[K, V](hd, tl)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            // If the "tip" is an existing node, then we can add the new leaf's hash to the node's
            // pointer block and rehash.
            case Node(pb) =>
              val pathLength    = parents.length
              val newLeafIndex  = JByte.toUnsignedInt(encodedKeyNew(pathLength))
              val hd            = Node(pb.updated(List((newLeafIndex, LeafPointer(newLeafHash)))))
              val rehashedNodes = rehash[K, V](hd, parents)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
          }
      }
    }

  @tailrec
  private[this] def propagateLeafUpward[T, K, V](
      hash: Blake2b256Hash,
      parents: Seq[(Int, Node)]): (Node, Seq[(Int, Node)]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to point to the Leaf.
      case Seq((byte, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((byte, LeafPointer(hash))))), Seq.empty[(Int, Node)])
      // Otherwise,
      case (byte, Node(pointerBlock)) +: tail =>
        // Get the children of the immediate parent
        pointerBlock.children match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the leaf we are trying to propagate up the trie.
          case Vector() => throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // propagate upwards, and we can go ahead and do that.
          case Vector(_) => propagateLeafUpward(hash, tail)
          // Otherwise, if there are > 2 children, we can update the parent node's Vector
          // at the given index to point to the leaf.
          case _ => (Node(pointerBlock.updated(List((byte, LeafPointer(hash))))), tail)
        }
    }

  @tailrec
  private[this] def deleteLeaf[T, K, V](parents: Seq[(Int, Node)]): (Node, Seq[(Int, Node)]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to `None`
      case Seq((index, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((index, EmptyPointer)))), Seq.empty[(Int, Node)])
      // Otherwise,
      case (byte, Node(pointerBlock)) +: tail =>
        val updated = (Node(pointerBlock.updated(List((byte, EmptyPointer)))), tail)
        // Get the children of the immediate parent
        pointerBlock.childrenWithIndex match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the thing we are trying to delete.
          case Vector() =>
            throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // delete, and we can go ahead and move up the trie.
          case Vector(_) =>
            deleteLeaf(tail)
          // If there are two children, then we know that one of them points down to the thing
          // we are trying to delete.  We then decide how to handle the other child based on
          // whether or not it is a Node or a Leaf
          case c @ Vector(_, _) =>
            val otherPointer = c.collect { case (child, childByte) if childByte != byte => child }.head
            otherPointer match {
              // If the other child is a Node, then we leave it intact, and update the parent node's
              // Vector at the given index to `None`.
              case NodePointer(_) => updated
              // If the other child is a Leaf, then we must propagate it up the trie.
              case LeafPointer(otherHash) => propagateLeafUpward(otherHash, tail)
            }
          // Otherwise if there are > 2 children, update the parent node's Vector at the given
          // index to `None`.
          case _ =>
            updated
        }
    }

  def delete[T, K, V](store: ITrieStore[T, K, V], key: K, value: V)(implicit
                                                                    codecK: Codec[K],
                                                                    codecV: Codec[V]): Boolean =
    store.withTxn(store.createTxnWrite()) { (txn: T) =>
      // We take the current root hash, preventing other threads from operating on the Trie
      val currentRootHash: Blake2b256Hash =
        store.getRoot(txn).getOrElse(throw new InsertException("could not get root"))
      // Get the current root node
      store.get(txn, currentRootHash) match {
        case None =>
          throw new LookupException(s"No node at $currentRootHash")
        case Some(currentRoot) =>
          // Serialize and convert the key to a `Seq[Byte]`.  This becomes our "path" down the Trie.
          val encodedKey = codecK.encode(key).map(_.bytes.toSeq).get
          // Using this path, get the parents of the given leaf.
          val (tip, parents) = getParents(store, txn, encodedKey, 0, currentRoot)
          tip match {
            // If the "tip" is a node, a leaf with a given key and value does not exist
            // so we put the current root hash back and return false.
            case Node(_) =>
              logger.debug(s"workingRootHash: $currentRootHash")
              false
            // If the "tip" is equal to a leaf containing the given key and value, commence
            // with the deletion process.
            case leaf @ Leaf(_, _) if leaf == Leaf(key, value) =>
              val (hd, nodesToRehash) = deleteLeaf(parents)
              val rehashedNodes       = rehash[K, V](hd, nodesToRehash)
              val newRootHash         = insertTries[T, K, V](store, txn, rehashedNodes).get
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
              true
            // The entry is not in the trie
            case Leaf(_, _) => false
          }
      }
    }
  import scodec.Codec
  import scodec.bits.{BitVector, ByteVector}
  import scodec.codecs._

  implicit val codecPointer: Codec[Pointer] =
    discriminated[Pointer]
      .by(uint8)
      .subcaseP(0) {
        case value: LeafPointer => value
      }(Blake2b256Hash.codecBlake2b256Hash.as[LeafPointer])
      .subcaseP(1) {
        case node: NodePointer => node
      }(Blake2b256Hash.codecBlake2b256Hash.as[NodePointer])
      .subcaseP(2) {
        case nothing: EmptyPointer.type => nothing
      }(provide(EmptyPointer))
}
