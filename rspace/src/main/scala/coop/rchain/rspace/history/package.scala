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
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.immutable

package object history {

  private[this] type Parents[K, V] = Seq[(Int, Trie[K, V])]

  private[this] implicit class ParentsOps[K, V](val parents: Seq[(Int, Trie[K, V])])
      extends AnyVal {

    def countPathLength =
      parents
        .foldLeft(0)((acc, el) =>
          el match {
            case (_, s: Skip) => acc + s.affix.size.toInt
            case _            => acc + 1
        })

  }

  private val logger: Logger = Logger[this.type]

  def initialize[T, K, V](store: ITrieStore[T, K, V], branch: Branch)(implicit
                                                                      codecK: Codec[K],
                                                                      codecV: Codec[V]): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.getRoot(txn, branch) match {
        case None =>
          val root     = Trie.create[K, V]()
          val rootHash = Trie.hash(root)
          store.put(txn, rootHash, root)
          store.putRoot(txn, branch, rootHash)
          logger.debug(s"workingRootHash: $rootHash")
        case Some(_) =>
          ()
      }
    }

  def lookup[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K)(
      implicit codecK: Codec[K]): Option[V] = {
    val path = codecK.encode(key).map(_.bytes.toSeq).get

    @tailrec
    def loop(txn: T, depth: Int, curr: Trie[K, V]): Option[V] =
      curr match {
        case Skip(affix, pointer) =>
          store.get(txn, pointer.hash) match {
            case Some(next) => loop(txn, depth + affix.length.toInt, next)
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
                case Some(next) => loop(txn, depth + 1, next)
                case None       => throw new LookupException(s"No node at ${pointer.hash}")
              }
          }
        case Leaf(lk, lv) if key == lk =>
          Some(lv)
        case Leaf(_, _) =>
          None
      }

    store.withTxn(store.createTxnRead()) { (txn: T) =>
      for {
        currentRootHash <- store.getRoot(txn, branch)
        currentRoot     <- store.get(txn, currentRootHash)
        res             <- loop(txn, 0, currentRoot)
      } yield res
    }
  }

  def lookup[T, K, V](store: ITrieStore[T, K, V], branch: Branch, keys: immutable.Seq[K])(
      implicit codecK: Codec[K]): Option[immutable.Seq[V]] =
    keys.traverse[Option, V]((k: K) => lookup(store, branch, k))

  private[this] def getParents[T, K, V](store: ITrieStore[T, K, V],
                                        txn: T,
                                        path: Seq[Byte],
                                        curr: Trie[K, V]): (Trie[K, V], Parents[K, V]) = {

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
                case Some(next) =>
                  parents(depth + 1, next, (index, node) +: acc)
              }
          }
        case s @ Skip(affix, pointer) =>
          val subPath = ByteVector(path.splitAt(depth)._2.take(affix.size.toInt))
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
      codecV: Codec[V]): Seq[(Blake2b256Hash, Trie[K, V])] = {
    val lastOpt = parents.lastOption.map(_._2)
    parents.scanLeft((Trie.hash[K, V](trie), trie)) {
      // root
      case ((lastHash, _), (offset, current @ Node(pb))) if lastOpt.contains(current) =>
        val node = Node(pb.updated(List((offset, NodePointer(lastHash)))))
        (Trie.hash[K, V](node), node)
      // no children - collapse to skip
      case ((lastHash, node), (offset, Node(pb))) if pb.children.isEmpty =>
        val b = ByteVector(offset)
        val skip = node match {
          case Leaf(_, _)           => Skip(b, LeafPointer(lastHash))
          case Node(_)              => Skip(b, NodePointer(lastHash))
          case Skip(affix, pointer) => Skip(b ++ affix, pointer)
        }
        (Trie.hash[K, V](skip), skip)
      // node with children, just rehash
      case ((lastHash, _), (offset, Node(pb))) =>
        val node = Node(pb.updated(List((offset, NodePointer(lastHash)))))
        (Trie.hash[K, V](node), node)
      //point at node from skip
      case ((lh, Node(_)), (_, Skip(oldAffix, _))) =>
        val ns = Skip(oldAffix, NodePointer(lh))
        (Trie.hash[K, V](ns), ns)
    }
  }

  def insert[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K, value: V)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): Unit =
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
          val encodedKeyNew = codecK.encode(key).map(_.bytes.toSeq).get
          // Create the new leaf and put it into the store
          val newLeaf     = Leaf(key, value)
          val newLeafHash = Trie.hash(newLeaf)
          store.put(txn, newLeafHash, newLeaf)
          // Using the path we created from the key, get the existing parents of the new leaf.
          val (tip, parents) = getParents(store, txn, encodedKeyNew, currentRoot)
          tip match {
            // If the "tip" is the same as the new leaf, then the given (key, value) pair is
            // already in the Trie, so we put the rootHash back and continue
            case existingLeaf @ Leaf(_, _) if existingLeaf == newLeaf =>
              logger.debug(s"workingRootHash: $currentRootHash")
            // If the "tip" is an existing leaf with a different key than the new leaf, then
            // we are in a situation where the new leaf shares some common prefix with the
            // existing leaf.
            case existingLeaf @ Leaf(ek, _) if key != ek =>
              val wholePathLength = encodedKeyNew.size

              val encodedKeyExisting = codecK.encode(ek).map(_.bytes.toSeq).get
              val sharedPrefix       = commonPrefix(encodedKeyNew, encodedKeyExisting)
              val sharedPrefixLength = sharedPrefix.length
              val sharedPath         = sharedPrefix.drop(parents.length).reverse
              val newLeafIndex       = JByte.toUnsignedInt(encodedKeyNew(sharedPrefixLength))
              val existingLeafIndex  = JByte.toUnsignedInt(encodedKeyExisting(sharedPrefixLength))

              val pathLeft = wholePathLength - sharedPrefixLength
              val update = if (pathLeft > 1) {
                val pathLength  = parents.countPathLength
                val commonAffix = ByteVector(encodedKeyNew.splitAt(pathLength + 1)._2)
                val skipExistingPtr =
                  setupSkipNode(store, txn, LeafPointer(newLeafHash), commonAffix)
                val skipNewPtr =
                  setupSkipNode(store, txn, LeafPointer(Trie.hash[K, V](existingLeaf)), commonAffix)
                List((newLeafIndex, skipNewPtr), (existingLeafIndex, skipExistingPtr))
              } else {
                List((newLeafIndex, LeafPointer(newLeafHash)),
                     (existingLeafIndex, LeafPointer(Trie.hash[K, V](existingLeaf))))
              }
              val hd = Node(
                PointerBlock
                  .create()
                  .updated(update))

              val emptyNode  = Node(PointerBlock.create())
              val emptyNodes = sharedPath.map((b: Byte) => (JByte.toUnsignedInt(b), emptyNode))
              val nodes      = emptyNodes ++ parents

              val rehashedNodes = rehash[K, V](hd, nodes)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, branch, newRootHash)
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
                case (_, Skip(affix, _)) +: remaining =>
                  (Skip(affix, LeafPointer(newLeafHash)), remaining)
                case Seq() =>
                  throw new InsertException("A leaf had no parents")
              }
              val rehashedNodes = rehash[K, V](hd, tl)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            // If the "tip" is an existing node, then we can add the new leaf's hash to the node's
            // pointer block and rehash.
            case Node(pb) =>
              val pathLength      = parents.countPathLength
              val wholePathLength = encodedKeyNew.size
              val pathLeft        = wholePathLength - pathLength
              val newLeafIndex    = JByte.toUnsignedInt(encodedKeyNew(pathLength))
              logger.debug(
                s"Node state, current:$pathLength, left: $pathLeft, whole: $wholePathLength")
              val ptr = if (pathLeft > 1) {
                val skip = Skip(ByteVector(encodedKeyNew.splitAt(pathLength + 1)._2),
                                LeafPointer(newLeafHash))
                val skipHash = Trie.hash(skip)(codecK, codecV)
                store.put(txn, skipHash, skip)
                NodePointer(skipHash)
              } else {
                LeafPointer(newLeafHash)
              }
              val hd            = Node(pb.updated(List((newLeafIndex, ptr))))
              val rehashedNodes = rehash[K, V](hd, parents)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            // If the tip is a skip node -> there is no Node for this Leaf.
            // need to shorten this skip and rectify the structure
            case Skip(affix, ptr) =>
              val pathLength          = parents.countPathLength
              val subPath             = ByteVector(encodedKeyNew.splitAt(pathLength)._2.take(affix.size.toInt)) //subpath does not match affix
              val pathLeft            = ByteVector(encodedKeyNew.splitAt(pathLength + affix.size.toInt)._2)
              val sharedPrefix        = commonPrefix(affix.toSeq, subPath.toSeq)
              val sharedSubpathLength = sharedPrefix.length.toLong
              val oldSuffix           = affix.splitAt(sharedSubpathLength)._2
              val newSuffix           = subPath.splitAt(sharedSubpathLength)._2

              val pn = (oldSuffix.size, newSuffix.size) match {
                case (0, 0) => throw new InsertException("Found empty affixes")
                case (ol, nl) if (ol == nl) && (ol == 1) =>
                  val oldLeafIndex = JByte.toUnsignedInt(oldSuffix(0))
                  val newLeafIndex = JByte.toUnsignedInt(newSuffix(0))
                  val newPtr = if (pathLeft.size > 0) {
                    setupSkipNode(store, txn, LeafPointer(newLeafHash), pathLeft)
                  } else {
                    LeafPointer(newLeafHash)
                  }
                  Node(
                    PointerBlock
                      .create()
                      .updated(List((oldLeafIndex, ptr), (newLeafIndex, newPtr))))
                case (ol, nl) if (ol == nl) && (ol > 1) =>
                  val oldLeafIndex = JByte.toUnsignedInt(oldSuffix(0))
                  val newLeafIndex = JByte.toUnsignedInt(newSuffix(0))
                  val newNode = setupSkipNode(store,
                                              txn,
                                              LeafPointer(newLeafHash),
                                              newSuffix.drop(1) ++ pathLeft)
                  val oldNode = setupSkipNode(store, txn, ptr, oldSuffix.drop(1))
                  Node(
                    PointerBlock
                      .create()
                      .updated(List((oldLeafIndex, oldNode), (newLeafIndex, newNode))))
                case _ => throw new InsertException("Affixes have different lengths")
              }

              val pnHash = Trie.hash(pn)(codecK, codecV)
              store.put(txn, pnHash, pn)

              val add = if (sharedSubpathLength > 0) {
                Skip(ByteVector(sharedPrefix), NodePointer(pnHash))
              } else {
                pn
              }
              //analyze:
              val rehashedNodes = rehash[K, V](add, parents)
              val newRootHash   = insertTries(store, txn, rehashedNodes).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
          }
      }
    }

  @tailrec
  private[this] def propagateTrieUpward[T, K, V](store: ITrieStore[T, K, V],
                                                 txn: T,
                                                 ptr: NonEmptyPointer,
                                                 incomingAffix: ByteVector,
                                                 parents: Parents[K, V])(
      implicit codecK: Codec[K],
      codecV: Codec[V]): (Trie[K, V], Parents[K, V]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to point to the node.
      case Seq((byte, Node(pointerBlock))) =>
        ptr match {
          case _: LeafPointer =>
            //TODO assumption - key > 2. bad assumption
            val nodePtr: NodePointer = setupSkipNode(store, txn, ptr, incomingAffix)
            (Node(pointerBlock.updated(List((byte, nodePtr)))), Seq.empty[(Int, Node)])
          case _: NodePointer =>
            val setPtr =
              if (incomingAffix.length > 0) {
                // collapsing a bridging node into a skip node
                setupSkipNode(store, txn, ptr, incomingAffix)
              } else {
                store.get(txn, ptr.hash)
                ptr
              }
            (Node(pointerBlock.updated(List((byte, setPtr)))), Seq.empty[(Int, Node)])
        }

      // Otherwise,
      case (_, Skip(affix, _)) +: tail =>
        (Skip(affix ++ incomingAffix, ptr), tail)
      case (byte, Node(pointerBlock)) +: tail =>
        // Get the children of the immediate parent
        pointerBlock.children match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the leaf we are trying to propagate up the trie.
          case Vector() => throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // propagate upwards, and we can go ahead and do that.
          case Vector(_) =>
            propagateTrieUpward(store, txn, ptr, ByteVector(byte), tail)
          // Otherwise, if there are > 2 children, we can update the parent node's Vector
          // at the given index to point to the leaf.
          case _ =>
            val setPtr = if (incomingAffix.length > 0) {
              setupSkipNode(store, txn, ptr, incomingAffix)
            } else {
              ptr
            }
            (Node(pointerBlock.updated(List((byte, setPtr)))), tail)
        }
    }

  private[this] def setupSkipNode[V, K, T](
      store: ITrieStore[T, K, V],
      txn: T,
      ptr: NonEmptyPointer,
      incomingAffix: ByteVector)(implicit codecK: Codec[K], codecV: Codec[V]) = {
    val skip     = Skip(incomingAffix, ptr)
    val skipHash = Trie.hash(skip)(codecK, codecV)
    store.put(txn, skipHash, skip)
    NodePointer(skipHash)
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

  @tailrec
  private[this] def deleteLeaf[T, K, V](store: ITrieStore[T, K, V], txn: T, parents: Parents[K, V])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): (Trie[K, V], Parents[K, V]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to `EmptyPointer`
      case Seq((index, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((index, EmptyPointer)))), Seq.empty[(Int, Node)])
      // Otherwise,
      case (_, Skip(_, LeafPointer(_))) +: tail =>
        deleteLeaf(store, txn, tail)
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
            deleteLeaf(store, txn, tail)
          // If there are two children, then we know that one of them points down to the thing
          // we are trying to delete.  We then decide how to handle the other child based on
          // whether or not it is a Node or a Leaf
          case c @ Vector(_, _) =>
            val (otherPointer, otherByte) = c.collect {
              case (child, childByte) if childByte != byte => (child, childByte)
            }.head
            otherPointer match {
              // If the other child is a Node, then we leave it intact, and update the parent node's
              // Vector at the given index to `None`.
              case NodePointer(hash) =>
                store.get(txn, hash) match {
                  case Some(Node(_)) =>
                    propagateTrieUpward(store, txn, otherPointer, ByteVector(otherByte), tail)
                  case Some(Skip(affix, ptr)) =>
                    propagateTrieUpward(store, txn, ptr, ByteVector(otherByte) ++ affix, tail)
                  case _ => throw new DeleteException("corrupt tree")
                }
              // If the other child is a Leaf, then we must propagate it up the trie.
              case lp: LeafPointer =>
                // TODO affix
                propagateTrieUpward(store, txn, lp, ByteVector(otherByte), tail)
            }
          // Otherwise if there are > 2 children, update the parent node's Vector at the given
          // index to `EmptyPointer`.
          case _ =>
            updated
        }
    }

  def delete[T, K, V](store: ITrieStore[T, K, V], branch: Branch, key: K, value: V)(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): Boolean =
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
              val (hd, nodesToRehash) = deleteLeaf(store, txn, parents)
              val rehashedNodes       = rehash[K, V](hd, nodesToRehash)
              val newRootHash         = insertTries[T, K, V](store, txn, rehashedNodes).get
              store.putRoot(txn, branch, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
              true
            // The entry is not in the trie
            case Leaf(_, _) => false
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
