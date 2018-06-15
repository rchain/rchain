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
    def loop(txn: T, depth: Int, curr: Trie[K, V]): Option[V] = {
      println("loop", depth, curr)
      curr match {
        case Skip(affix, pointer) =>
          println("?", depth, affix.length)
          store.get(txn, pointer.hash) match {
            case Some(next) => loop(txn, depth + affix.length.toInt, next)
            case None       => throw new LookupException(s"No node at ${pointer.hash}")
          }

        case Node(pointerBlock) =>
          println("pb", pointerBlock)
          println("depth", depth)
          val index: Int = JByte.toUnsignedInt(path(depth))
          println("index", index)
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
    }

    store.withTxn(store.createTxnRead()) { (txn: T) =>
      println("------------------")
      println("looking for", key)
      println("path", path)
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

  private[this] def getParents[T, K, V](store: ITrieStore[T, K, V],
                                        txn: T,
                                        path: Seq[Byte],
                                        curr: Trie[K, V]): (Trie[K, V], Seq[(Int, Trie[K, V])]) = {

    @tailrec
    def parents(depth: Int,
                curr: Trie[K, V],
                acc: Seq[(Int, Trie[K, V])]): (Trie[K, V], Seq[(Int, Trie[K, V])]) =
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

  private[this] def rehash[K, V](trie: Trie[K, V], parents: Seq[(Int, Trie[K, V])])(
      implicit
      codecK: Codec[K],
      codecV: Codec[V]): Seq[(Blake2b256Hash, Trie[K, V])] = {
    val lastOpt = parents.lastOption.map(_._2)
    parents.scanLeft((Trie.hash[K, V](trie), trie)) {
      // root
      case ((lastHash, _), (offset, current @ Node(pb))) if lastOpt.contains(current) =>
        val node = Node(pb.updated(List((offset, NodePointer(lastHash)))))
        (Trie.hash[K, V](node), node)
      case ((lastHash, x), (offset, Node(pb))) if pb.children.isEmpty =>
        val b = ByteVector(offset)
        val node = x match {
          case Leaf(key, value)     => Skip(b, LeafPointer(lastHash))
          case Node(pointerBlock)   => Skip(b, NodePointer(lastHash))
          case Skip(affix, pointer) => Skip(b ++ affix, pointer)
        }
        (Trie.hash[K, V](node), node)
      // intermediate
      case ((lastHash, _), (offset, Node(pb))) =>
        val node = Node(pb.updated(List((offset, NodePointer(lastHash)))))
        (Trie.hash[K, V](node), node)
      //skip meets skip
      case ((lh, s2 @ Skip(affix, ptr)), (offset, s @ Skip(oldAffix, _))) =>
        //TODO fix affix
        val ns = Skip(oldAffix.drop(1), ptr)
        (Trie.hash[K, V](ns), ns)
      //point at node from skip
      case ((lh, n2 @ Node(pb)), (offset, s @ Skip(oldAffix, _))) =>
        val ns = Skip(oldAffix, NodePointer(lh))
        (Trie.hash[K, V](ns), ns)
    }
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
              val pathLength = parents
                .map {
                  case (_, s: Skip) => s.affix.size
                  case _            => 1
                }
                .sum
                .toInt
              val wholePathLength = encodedKeyNew.size

              val encodedKeyExisting = codecK.encode(ek).map(_.bytes.toSeq).get
              val sharedPrefix       = commonPrefix(encodedKeyNew, encodedKeyExisting)
              val sharedPrefixLength = sharedPrefix.length
              val sharedPath         = sharedPrefix.drop(parents.length).reverse
              val newLeafIndex       = JByte.toUnsignedInt(encodedKeyNew(sharedPrefixLength))
              val existingLeafIndex  = JByte.toUnsignedInt(encodedKeyExisting(sharedPrefixLength))

              val pathLeft = wholePathLength - sharedPrefixLength
              val hd = if (pathLeft > 1) {
                val commonAffix = ByteVector(encodedKeyNew.splitAt(pathLength + 1)._2)
                val skipNew     = Skip(commonAffix, LeafPointer(newLeafHash))

                val skipExisting = Skip(commonAffix, LeafPointer(Trie.hash[K, V](existingLeaf)))

                val skipNewHash      = Trie.hash(skipNew)(codecK, codecV)
                val skipExistingHash = Trie.hash(skipExisting)(codecK, codecV)

                store.put(txn, skipNewHash, skipNew)
                store.put(txn, skipExistingHash, skipExisting)
                Node(
                  PointerBlock
                    .create()
                    .updated(List((newLeafIndex, NodePointer(skipNewHash)),
                                  (existingLeafIndex, NodePointer(skipExistingHash))))
                )
              } else {
                Node(
                  PointerBlock
                    .create()
                    .updated(List((newLeafIndex, LeafPointer(newLeafHash)),
                                  (existingLeafIndex, LeafPointer(Trie.hash[K, V](existingLeaf)))))
                )
              }

              val emptyNode  = Node(PointerBlock.create())
              val emptyNodes = sharedPath.map((b: Byte) => (JByte.toUnsignedInt(b), emptyNode))
              val nodes      = emptyNodes ++ parents

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
                case (idx, Skip(affix, _)) +: remaining =>
                  (Skip(affix, LeafPointer(newLeafHash)), remaining)
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
              val pathLength = parents
                .map {
                  case (_, s: Skip) => s.affix.size
                  case _            => 1
                }
                .sum
                .toInt
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
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
            // If the tip is a skip node -> there is no Node for this Leaf.
            // need to shorten this skip and rectify the structure
            case Skip(affix, ptr) =>
              val pathLength = parents
                .map {
                  case (_, s: Skip) => s.affix.size
                  case _            => 1
                }
                .sum
                .toInt
              val wholePathLength = encodedKeyNew.size

              val subPath             = ByteVector(encodedKeyNew.splitAt(pathLength)._2.take(affix.size.toInt)) //subpath does not match affix
              val pathLeft            = ByteVector(encodedKeyNew.splitAt(pathLength + affix.size.toInt)._2)
              val sharedPrefix        = commonPrefix(affix.toSeq, subPath.toSeq)
              val sharedSubpathLength = sharedPrefix.length
              val oldSuffix           = affix.splitAt(sharedSubpathLength)._2
              val newSuffix           = subPath.splitAt(sharedSubpathLength)._2

              val pn = (oldSuffix.size, newSuffix.size) match {
                case (0, 0) => throw new RuntimeException("corrupt structure")
                case (ol, nl) if (ol == nl) && (ol == 1) => {
                  val oldLeafIndex = JByte.toUnsignedInt(oldSuffix(0))
                  val newLeafIndex = JByte.toUnsignedInt(newSuffix(0))
                  val newPtr = if (pathLeft.size > 0) {
                    val newSkip     = Skip(pathLeft, LeafPointer(newLeafHash))
                    val newSkipHash = Trie.hash(newSkip)(codecK, codecV)
                    store.put(txn, newSkipHash, newSkip)
                    NodePointer(newSkipHash)
                  } else {
                    LeafPointer(newLeafHash)
                  }
                  Node(
                    PointerBlock
                      .create()
                      .updated(List((oldLeafIndex, ptr), (newLeafIndex, newPtr))))
                }
                case (ol, nl) if (ol == nl) && (ol > 1) => {
                  val oldLeafIndex = JByte.toUnsignedInt(oldSuffix(0))
                  val newLeafIndex = JByte.toUnsignedInt(newSuffix(0))
                  val newSkip      = Skip(newSuffix.drop(1) ++ pathLeft, LeafPointer(newLeafHash))
                  val newSkipHash  = Trie.hash(newSkip)(codecK, codecV)
                  store.put(txn, newSkipHash, newSkip)
                  val oldSkip     = Skip(oldSuffix.drop(1), ptr)
                  val oldSkipHash = Trie.hash(oldSkip)(codecK, codecV)
                  store.put(txn, oldSkipHash, oldSkip)
                  Node(
                    PointerBlock
                      .create()
                      .updated(List((oldLeafIndex, NodePointer(oldSkipHash)),
                                    (newLeafIndex, NodePointer(newSkipHash)))))
                }
                case _ => throw new RuntimeException("is this possible?")
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
              store.putRoot(txn, newRootHash)
              logger.debug(s"workingRootHash: $newRootHash")
          }
      }
    }

  @tailrec
  private[this] def propagateLeafUpward[T, K, V](
      hash: Blake2b256Hash,
      parents: Seq[(Int, Trie[K, V])]): (Trie[K, V], Seq[(Int, Trie[K, V])]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to point to the Leaf.
      case Seq((byte, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((byte, LeafPointer(hash))))), Seq.empty[(Int, Node)])
      //I'm a leaf, I encounter a skip block. I got here because all the kids were single child nodes or me - kill the skip block
      case (byte, s @ Skip(affix, ptr)) +: tail =>
        (Skip(affix ++ ByteVector(byte), LeafPointer(hash)), tail)

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
  private[this] def propagateTrieUpward[T, K, V](
      ptr: NonEmptyPointer,
      incomingAffix: ByteVector,
      parents: Seq[(Int, Trie[K, V])]): (Trie[K, V], Seq[(Int, Trie[K, V])]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to point to the node.
      case Seq((byte, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((byte, ptr)))), Seq.empty[(Int, Node)])
      // Otherwise,
      case (byte, s @ Skip(affix, _)) +: tail =>
        (Skip(affix ++ incomingAffix, ptr), tail)
      case (byte, Node(pointerBlock)) +: tail =>
        // Get the children of the immediate parent
        pointerBlock.children match {
          // If there are no children, then something is wrong, because one of the children
          // should point down to the leaf we are trying to propagate up the trie.
          case Vector() => throw new DeleteException("PointerBlock has no children")
          // If there are is only one child, then we know that it is the thing we are trying to
          // propagate upwards, and we can go ahead and do that.
          case Vector(_) => propagateTrieUpward(ptr, ByteVector(byte), tail)
          // Otherwise, if there are > 2 children, we can update the parent node's Vector
          // at the given index to point to the leaf.
          case _ => (Node(pointerBlock.updated(List((byte, ptr)))), tail)
        }
    }
  @tailrec
  private[this] def deleteLeaf[T, K, V](
      store: ITrieStore[T, K, V],
      txn: T,
      parents: Seq[(Int, Trie[K, V])]): (Trie[K, V], Seq[(Int, Trie[K, V])]) =
    parents match {
      // If the list parents only contains a single Node, we know we are at the root, and we
      // can update the Vector at the given index to `EmptyPointer`
      case Seq((index, Node(pointerBlock))) =>
        (Node(pointerBlock.updated(List((index, EmptyPointer)))), Seq.empty[(Int, Node)])
      // Otherwise,
      case (byte, s @ Skip(_, LeafPointer(lh))) +: tail =>
        val updated = (s, tail)
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
                  case Some(Node(pb)) =>
                    propagateTrieUpward(otherPointer, ByteVector(otherByte), tail)
                  case Some(Skip(affix, ptr)) =>
                    propagateTrieUpward(ptr, ByteVector(otherByte) ++ affix, tail)
                  case None => throw new DeleteException("corrupt tree")
                }
              // If the other child is a Leaf, then we must propagate it up the trie.
              case LeafPointer(otherHash) =>
                // TODO affix
                propagateLeafUpward(otherHash, tail)
            }
          // Otherwise if there are > 2 children, update the parent node's Vector at the given
          // index to `EmptyPointer`.
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
              store.putRoot(txn, newRootHash)
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
