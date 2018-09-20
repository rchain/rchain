package coop.rchain.rspace

import java.lang.{Byte => JByte}

import internal._
import cats.Eq
import cats.instances.byte._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.traverse._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib.seq._
import scodec.Codec
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.immutable

package object history {

  private[this] type Parents[K, V] = Seq[(Int, Trie[K, V])]

  private[this] implicit class ParentsOps[K, V](val parents: Parents[K, V]) extends AnyVal {

    def countPathLength: Long =
      parents
        .foldLeft(0L)(
          (acc, el) =>
            el match {
              case (_, s: Skip) => acc + s.affix.size
              case _            => acc + 1L
            }
        )

  }

  private val logger: Logger = Logger[this.type]

  def initialize[T, K, V](store: ITrieStore[T, K, V], branch: Branch)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): Boolean =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.getRoot(txn, branch) match {
        case None =>
          val root     = Trie.create[K, V]()
          val rootHash = Trie.hash(root)
          store.put(txn, rootHash, root)
          store.putRoot(txn, branch, rootHash)
          store.putEmptyRoot(txn, rootHash)
          logger.debug(s"workingRootHash: $rootHash, setup the empty root in trie")
          true
        case Some(_) =>
          false
      }
    }

  private[this] def lookup[T, K, V](
      txn: T,
      store: ITrieStore[T, K, V],
      branchRootHash: Blake2b256Hash,
      key: K
  )(implicit codecK: Codec[K]): Option[V] = {
    val path = codecK.encode(key).map(_.bytes.toSeq).get

    @tailrec
    def loop(depth: Int, curr: Trie[K, V]): Option[V] =
      curr match {
        case Skip(affix, pointer) =>
          store.get(txn, pointer.hash) match {
            case Some(next) => loop(depth + affix.length.toInt, next)
            case None       => throw new LookupException(s"No node at ${pointer.hash}")
          }

        case Node(pointerBlock) =>
          val index: Int = JByte.toUnsignedInt(path(depth))
          // We use an explicit match here instead of flatMapping in order to make this function
          // tail-recursive
          pointerBlock.toVector(index) match {
            case EmptyPointer =>
              None
            case pointer: NonEmptyPointer =>
              store.get(txn, pointer.hash) match {
                case Some(next) => loop(depth + 1, next)
                case None       => throw new LookupException(s"No node at ${pointer.hash}")
              }
          }
        case Leaf(lk, lv) if key == lk =>
          Some(lv)
        case Leaf(_, _) =>
          None
      }

    for {
      currentRoot <- store.get(txn, branchRootHash)
      res         <- loop(0, currentRoot)
    } yield res
  }

  def lookup[T, K, V](store: ITrieStore[T, K, V], rootHash: Blake2b256Hash, key: K)(
      implicit codecK: Codec[K]
  ): Option[V] =
    store.withTxn(store.createTxnRead()) { (txn: T) =>
      lookup(txn, store, rootHash, key)
    }

  def lookup[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K)(
      implicit codecK: Codec[K]
  ): Option[V] =
    store.withTxn(store.createTxnRead()) { (txn: T) =>
      store.getRoot(txn, branch).flatMap(lookup(txn, store, _, key))
    }

  def lookup[T, K, V](store: ITrieStore[T, K, V], branch: Branch, keys: immutable.Seq[K])(
      implicit codecK: Codec[K]
  ): Option[immutable.Seq[V]] =
    if (keys.isEmpty) {
      throw new IllegalArgumentException("keys can't be empty")
    } else {
      store.withTxn(store.createTxnRead()) { (txn: T) =>
        keys.traverse[Option, V](
          (k: K) => store.getRoot(txn, branch).flatMap(lookup(txn, store, _, k))
        )
      }
    }

  private[this] def getParents[T, K, V](
      store: ITrieStore[T, K, V],
      txn: T,
      path: Seq[Byte],
      curr: Trie[K, V]
  ): (Trie[K, V], Parents[K, V]) = {

    @tailrec
    def parents(depth: Int, curr: Trie[K, V], acc: Parents[K, V]): (Trie[K, V], Parents[K, V]) =
      curr match {
        case node @ Node(pointerBlock) =>
          val index: Int = JByte.toUnsignedInt(path(depth))
          pointerBlock.toVector(index) match {
            case EmptyPointer =>
              (curr, acc)
            case next: NonEmptyPointer =>
              store.get(txn, next.hash) match {
                case None =>
                  throw new LookupException(s"No node at ${next.hash}")
                case Some(trie) =>
                  parents(depth + 1, trie, (index, node) +: acc)
              }
          }
        case s @ Skip(affix, pointer) =>
          val subPath = ByteVector(path).slice(depth.toLong, depth.toLong + affix.size)
          if (subPath === affix) {
            store.get(txn, pointer.hash) match {
              case None =>
                throw new LookupException(s"No node at ${pointer.hash}")
              case Some(next) =>
                val index: Int = JByte.toUnsignedInt(path(depth))
                parents(affix.size.toInt + depth, next, (index, s) +: acc)
            }
          } else {
            (s, acc)
          }
        case leaf =>
          (leaf, acc)
      }
    parents(0, curr, Seq.empty)
  }

  // TODO(ht): make this more efficient
  private[this] def commonPrefix[A: Eq](a: Seq[A], b: Seq[A]): Seq[A] =
    a.zip(b).takeWhile { case (l, r) => l === r }.map(_._1)

  private[this] def rehash[K, V](trie: Trie[K, V], parents: Parents[K, V])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): Seq[(Blake2b256Hash, Trie[K, V])] =
    parents.scanLeft((Trie.hash[K, V](trie), trie)) {
      // node with children, just rehash
      case ((lastHash, _), (offset, Node(pointerBlock))) =>
        val node = Node(pointerBlock.updated((offset, NodePointer(lastHash))))
        (Trie.hash[K, V](node), node)
      //point at node from skip
      case ((lastHash, _: Node), (_, s: Skip)) =>
        val node = s.copy(pointer = NodePointer(lastHash))
        (Trie.hash[K, V](node), node)
    }

  def insert[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K, value: V)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): Unit =
    store.withTxn(store.createTxnWrite()) { (txn: T) =>
      // Get the current root hash
      val currentRootHash: Blake2b256Hash =
        store.getRoot(txn, branch).getOrElse(throw new InsertException("could not get root"))
      // Get the current root node
      store.get(txn, currentRootHash) match {
        case None =>
          throw new LookupException(s"No node at $currentRootHash")
        case Some(currentRoot) =>
          // Serialize and convert the key to a `Seq[Byte]`.  This becomes our "path" down the Trie.
          val encodedKeyNew        = codecK.encode(key).map(_.bytes.toSeq).get
          val encodedKeyByteVector = ByteVector(encodedKeyNew)
          // Create the new leaf and put it into the store
          val newLeaf     = Leaf(key, value)
          val newLeafHash = Trie.hash(newLeaf)
          store.put(txn, newLeafHash, newLeaf)
          // Using the path we created from the key, get the existing parents of the new leaf.
          val (tip, parents) = getParents(store, txn, encodedKeyNew, currentRoot)
          val maybeNewNodes: Option[Seq[(Blake2b256Hash, Trie[K, V])]] = tip match {
            // If the "tip" is the same as the new leaf, then the given (key, value) pair is
            // already in the Trie, so we put the rootHash back and continue
            case existingLeaf @ Leaf(_, _) if existingLeaf == newLeaf =>
              logger.debug(s"workingRootHash: $currentRootHash")
              None
            // If the "tip" is an existing leaf with a different key than the new leaf, then
            // we are in a situation where the new leaf shares some common prefix with the
            // existing leaf.
            // post skip note: due to the nature of skip nodes, this case is not possible
            // with the current implementation of getParents
            case Leaf(ek, _) if key != ek =>
              throw new InsertException("Trie parents tip should not hold a leaf.")
            // If the "tip" is an existing leaf with the same key as the new leaf, but the
            // existing leaf and new leaf have different values, then we are in the situation
            // where we are "updating" an existing leaf
            case Leaf(ek, ev) if key == ek && value != ev =>
              // Update the pointer block of the immediate parent at the given index
              // to point to the new leaf instead of the existing leaf
              Some(updateLeaf(newLeafHash, parents))
            // If the "tip" is an existing node, then we can add the new leaf's hash to the node's
            // pointer block and rehash.
            case Node(pb) =>
              Some(insertLeafAsNodeChild(encodedKeyByteVector, newLeafHash, parents, pb))
            // If the tip is a skip node -> there is no Node in the Trie for this Leaf.
            // need to:
            // - shorten (a new skip that holds the common path)
            // - remove (no new skip, just the new trie)
            // and rectify the structure
            case Skip(affix, ptr) =>
              Some(insertLeafOntoSkipNode(encodedKeyByteVector, newLeafHash, parents, affix, ptr))
          }
          maybeNewNodes match {
            case Some(nodes) =>
              val newRootHash = insertTries(store, txn, nodes).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            case None =>
              logger.debug(s"insert did not change the trie")
          }
      }
    }

  private[this] def insertLeafOntoSkipNode[V, K, T](
      encodedKeyByteVector: ByteVector,
      newLeafHash: Blake2b256Hash,
      parents: Parents[K, V],
      affix: ByteVector,
      ptr: NonEmptyPointer
  )(implicit codecK: Codec[K], codecV: Codec[V]) = {
    val pathLength = parents.countPathLength
    val subPath    = encodedKeyByteVector.slice(pathLength, pathLength + affix.size)
    val pathRemaining =
      encodedKeyByteVector.slice(pathLength + affix.size, encodedKeyByteVector.length)
    val sharedPrefix        = commonPrefix(affix.toSeq, subPath.toSeq)
    val sharedSubPathLength = sharedPrefix.length.toLong
    val oldSuffix           = affix.splitAt(sharedSubPathLength)._2
    val newSuffix           = subPath.splitAt(sharedSubPathLength)._2

    val (existingPtr, newPtr, newParents) = (oldSuffix.size, newSuffix.size) match {
      case (0, 0)               => throw new InsertException("Found empty affixes")
      case (ol, nl) if ol != nl => throw new InsertException("Affixes have different lengths")
      case (len, _) if len == 1 =>
        val (newPtr, newParents) =
          setupSkipNode(LeafPointer(newLeafHash), pathRemaining)(codecK, codecV)
        (ptr, newPtr, newParents)
      case (len, _) if len > 1 =>
        val (newSkip, newParents) =
          setupSkipNode(LeafPointer(newLeafHash), newSuffix.drop(1) ++ pathRemaining)(
            codecK,
            codecV
          )
        val (oldSkip, oldParents) =
          setupSkipNode(ptr, oldSuffix.drop(1))(codecK, codecV)
        (oldSkip, newSkip, newParents ++ oldParents)
      case _ => throw new InsertException("Affix corrupt in skip node")
    }

    val oldNodeIndex = JByte.toUnsignedInt(oldSuffix(0))
    val newNodeIndex = JByte.toUnsignedInt(newSuffix(0))
    val newCombinedNode = Node(
      PointerBlock.create((oldNodeIndex, existingPtr), (newNodeIndex, newPtr))
    )

    val (toBeAdded, skips) = if (sharedSubPathLength > 0) {
      val combinedHash = Trie.hash(newCombinedNode)(codecK, codecV)
      (
        Skip(ByteVector(sharedPrefix), NodePointer(combinedHash)),
        Seq((combinedHash, newCombinedNode))
      )
    } else {
      (newCombinedNode, Seq.empty)
    }
    val rehashedNodes = rehash[K, V](toBeAdded, parents)
    skips ++ newParents ++ rehashedNodes
  }

  private[this] def insertLeafAsNodeChild[V, K, T](
      encodedKeyByteVector: ByteVector,
      newLeafHash: Blake2b256Hash,
      parents: Parents[K, V],
      pb: PointerBlock
  )(implicit codecK: Codec[K], codecV: Codec[V]) = {
    val pathLength    = parents.countPathLength
    val newLeafIndex  = JByte.toUnsignedInt(encodedKeyByteVector(pathLength))
    val remainingPath = encodedKeyByteVector.splitAt(pathLength + 1)._2
    val (ptr, newParents) =
      setupSkipNode(LeafPointer(newLeafHash), remainingPath)(codecK, codecV)
    val hd            = Node(pb.updated(List((newLeafIndex, ptr))))
    val rehashedNodes = rehash[K, V](hd, parents)
    newParents ++ rehashedNodes
  }

  private[this] def updateLeaf[V, K, T](
      newLeafHash: Blake2b256Hash,
      parents: Parents[K, V]
  )(implicit codecK: Codec[K], codecV: Codec[V]) = {
    val (hd, tl) = parents match {
      case (idx, Node(pointerBlock)) +: remaining =>
        (Node(pointerBlock.updated((idx, LeafPointer(newLeafHash)))), remaining)
      case (_, Skip(affix, _)) +: remaining =>
        (Skip(affix, LeafPointer(newLeafHash)), remaining)
      case Seq() =>
        throw new InsertException("A leaf had no parents")
    }
    val rehashedNodes = rehash[K, V](hd, tl)
    rehashedNodes
  }

  private[this] def collapseAndUpdatePointerBlock[T, K, V](
      ptr: NonEmptyPointer,
      incomingAffix: ByteVector,
      parents: Parents[K, V]
  )(
      implicit codecK: Codec[K],
      codecV: Codec[V]
  ): (Trie[K, V], Parents[K, V], Seq[(Blake2b256Hash, Trie[K, V])]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to point to the node.
      case Seq((byte, Node(pointerBlock))) =>
        val (newPtr, newParents) = setupSkipNode(ptr, incomingAffix)(codecK, codecV)
        (Node(pointerBlock.updated((byte, newPtr))), Seq.empty[(Int, Node)], newParents)
      // if the node is a Skip it can append the collapsed affix
      case (_, Skip(affix, _)) +: tail =>
        (Skip(affix ++ incomingAffix, ptr), tail, Seq.empty)
      // otherwise it has to be a Node
      case (byte, Node(pointerBlock)) +: tail =>
        // Get the children of the immediate parent
        pointerBlock.children match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the leaf we are trying to propagate up the trie.
          case Vector() => throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // propagate upwards, and we can go ahead and do that.
          // post skip node: this case got swallowed in deleteLeaf
          case Vector(_) =>
            throw new DeleteException(
              "PointerBlock with one child on propagation signifies a malformed trie."
            )
          // Otherwise, if there are > 2 children, we can update the parent node's Vector
          // at the given index to point to the propagated node.
          case _ =>
            val (newPtr, newParents) = setupSkipNode(ptr, incomingAffix)(codecK, codecV)
            (Node(pointerBlock.updated((byte, newPtr))), tail, newParents)
        }
    }

  private[this] def setupSkipNode[V, K, T](
      ptr: NonEmptyPointer,
      incomingAffix: ByteVector
  )(implicit codecK: Codec[K], codecV: Codec[V]) =
    if (incomingAffix.size > 0) {
      val skip     = Skip(incomingAffix, ptr)
      val skipHash = Trie.hash(skip)(codecK, codecV)
      (NodePointer(skipHash), Seq((skipHash, skip)))
    } else {
      (ptr, Seq.empty)
    }

  private[this] def insertTries[T, K, V](
      store: ITrieStore[T, K, V],
      txn: T,
      rehashedNodes: Seq[(Blake2b256Hash, Trie[K, V])]
  ): Option[Blake2b256Hash] =
    rehashedNodes.foldLeft(None: Option[Blake2b256Hash]) {
      case (_, (hash, trie)) =>
        store.put(txn, hash, trie)
        Some(hash)
    }

  @tailrec
  private[this] def deleteLeaf[T, K, V](store: ITrieStore[T, K, V], txn: T, parents: Parents[K, V])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): (Trie[K, V], Parents[K, V], Seq[(Blake2b256Hash, Trie[K, V])]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to `EmptyPointer`
      case Seq((index, Node(pointerBlock))) =>
        (Node(pointerBlock.updated((index, EmptyPointer))), Seq.empty[(Int, Node)], Seq.empty)
      // A skip pointing at a leaf needs to be collapsed
      case (_, Skip(_, LeafPointer(_))) +: tail =>
        deleteLeaf(store, txn, tail)
      // Otherwise a Node needs to be handled
      case (byte, Node(pointerBlock)) +: tail =>
        // Get the children of the immediate parent
        pointerBlock.childrenWithIndex match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the thing we are trying to delete.
          case Vector() => throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // delete, and we can go ahead and move up the trie.
          // post skip node: a Node with one child should be already collapsed to skip -> leaf
          case Vector(_) =>
            deleteLeaf(store, txn, tail)
          // If there are two children, then we know that one of them points down to the thing
          // we are trying to delete.  We then decide how to handle the other child based on
          // whether or not it is a Node or a Leaf
          case c @ Vector(_, _) =>
            val (otherPtr, otherByte) = c.collect {
              case (child, childByte) if childByte != byte => (child, childByte)
            }.head
            otherPtr match {
              // If the other child is a Node we propagate the structure as it should get collapsed
              case NodePointer(hash) =>
                store.get(txn, hash) match {
                  case Some(Node(_)) =>
                    collapseAndUpdatePointerBlock(otherPtr, ByteVector(otherByte), tail)
                  case Some(Skip(affix, ptr)) =>
                    collapseAndUpdatePointerBlock(ptr, ByteVector(otherByte) ++ affix, tail)
                  case _ =>
                    throw new DeleteException("Given pointer could not be processed in delete.")
                }
              // If the other child is a Leaf, then we must propagate it up the trie.
              case lp: LeafPointer =>
                collapseAndUpdatePointerBlock(lp, ByteVector(otherByte), tail)
            }
          // Otherwise if there are > 2 children, update the parent node's Vector at the given
          // index to `EmptyPointer`.
          case _ => (Node(pointerBlock.updated((byte, EmptyPointer))), tail, Seq.empty)
        }
    }

  def delete[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K, value: V)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]
  ): Boolean =
    store.withTxn(store.createTxnWrite()) { (txn: T) =>
      // We take the current root hash, preventing other threads from operating on the Trie
      val currentRootHash: Blake2b256Hash =
        store.getRoot(txn, branch).getOrElse(throw new InsertException("could not get root"))
      // Get the current root node
      store.get(txn, currentRootHash) match {
        case None =>
          throw new LookupException(s"No node at $currentRootHash")
        case Some(currentRoot) =>
          // Serialize and convert the key to a `Seq[Byte]`.  This becomes our "path" down the Trie.
          val encodedKey = codecK.encode(key).map(_.bytes.toSeq).get
          // Using this path, get the parents of the given leaf.
          val (tip, parents) = getParents(store, txn, encodedKey, currentRoot)
          tip match {
            // If the "tip" is a node, a leaf with a given key and value does not exist
            // so we put the current root hash back and return false.
            case Node(_) =>
              logger.debug(s"workingRootHash: $currentRootHash")
              false
            // If the "tip" is equal to a leaf containing the given key and value, commence
            // with the deletion process.
            case leaf @ Leaf(_, _) if leaf == Leaf(key, value) =>
              val (hd, nodesToRehash, newNodes) = deleteLeaf(store, txn, parents)
              val rehashedNodes                 = rehash[K, V](hd, nodesToRehash)
              val nodesToInsert                 = newNodes ++ rehashedNodes
              val newRootHash                   = insertTries[T, K, V](store, txn, nodesToInsert).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
              true
            // The entry is not in the trie
            case Leaf(_, _) =>
              logger.debug(s"workingRootHash: $currentRootHash")
              false
          }
      }
    }

  import scodec.Codec
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

  implicit val codecNonEmptyPointer: Codec[NonEmptyPointer] =
    discriminated[NonEmptyPointer]
      .by(uint8)
      .subcaseP(0) {
        case value: LeafPointer => value
      }(Blake2b256Hash.codecBlake2b256Hash.as[LeafPointer])
      .subcaseP(1) {
        case node: NodePointer => node
      }(Blake2b256Hash.codecBlake2b256Hash.as[NodePointer])
}
