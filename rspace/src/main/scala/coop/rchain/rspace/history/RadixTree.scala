package coop.rchain.rspace.history

import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.Codec
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object RadixTree {

  /**
    * Radix Tree structure (Node and Leaf)
    */
  sealed trait TreeR
  final case class NodeR(prefix: ByteVector, refs: Vector[ByteVector]) extends TreeR
  final case class LeafR(prefix: ByteVector, value: ByteVector)        extends TreeR

  object codecs {
    import scodec.codecs._

    val codecByteVector: Codec[ByteVector] = variableSizeBytes(uint8, bytes)

    // Binary codecs for Tree

    val codecNodeR: Codec[NodeR] =
      (codecByteVector :: vectorOfN(provide(256), codecByteVector)).as[NodeR]

    val codecLeafR: Codec[LeafR] = (codecByteVector :: codecByteVector).as[LeafR]

    val codecTreeR: DiscriminatorCodec[TreeR, Int] =
      discriminated[TreeR]
        .by(uint2)
        .subcaseP(tag = 0) {
          case s: LeafR => s
        }(codecLeafR)
        .subcaseP(tag = 1) {
          case n: NodeR => n
        }(codecNodeR)
  }

  def commonPrefix(b1: ByteVector, b2: ByteVector) = {
    @tailrec
    def loop(
        common: ByteVector,
        l: ByteVector,
        r: ByteVector
    ): (ByteVector, ByteVector, ByteVector) =
//      assert(l.size >= r.size, s"Key ${b1.toHex} shorter then stored prefix ${b2.toHex}.")
      if (r.isEmpty) {
        (common, l, r)
      } else {
        val lHead = l.head
        val rHead = r.head
        if (lHead == rHead) loop(common :+ lHead, l.tail, r.tail)
        else (common, l, r)
      }

    loop(ByteVector.empty, b1, b2)
  }

  def hashFn(bs: ByteVector): Blake2b256Hash =
    Blake2b256Hash.create(bs)

  def hashTree(tree: TreeR) = {
    val newRefsSer = codecs.codecTreeR.encode(tree).require.toByteVector
    (hashFn(newRefsSer), newRefsSer)
  }

  // Default values for empty Tree/Node

  val emptyTreeRefs: Vector[ByteVector] = (0 to 255).map(_ => ByteVector.empty).toVector

  val emptyTree: TreeR = {
    val emptyVec = ByteVector.empty
    NodeR(prefix = emptyVec, refs = emptyTreeRefs)
  }

  class RadixTreeImpl(store: TrieMap[ByteVector, ByteVector]) {

    import codecs._

    def toInt(b: Byte): Int = b & 0xff

    def loadRefOpt(treeKey: ByteVector): Option[TreeR] = {
      val nodeData = store.get(treeKey)
      nodeData.map { data =>
        codecTreeR.decode(data.toBitVector).require.value
      }
    }

    def loadRef(treeKey: ByteVector): TreeR =
      loadRefOpt(treeKey).get

    def saveRef(tree: TreeR): Blake2b256Hash = {
      val (hash, bytes) = hashTree(tree)
      store.update(hash.bytes, bytes)
      hash
    }

    def loadRefWithDefault(hash: Blake2b256Hash): TreeR =
      loadRefOpt(hash.bytes).getOrElse(emptyTree)

    /**
      * Read key with prefix
      */
    @tailrec
    final def read(tree: TreeR, prefix: ByteVector): Option[ByteVector] =
      tree match {
        case NodeR(nPrefix, nRefs) =>
          assert(prefix.size > nPrefix.size, "Key is shorter then stored prefix.")

          val (_, prefixRest, nPrefixRest) = commonPrefix(prefix, nPrefix)

          if (nPrefixRest.isEmpty) {
            // Found prefix
            val idx        = toInt(prefixRest.head)
            val prefixTail = prefixRest.tail
            val ref        = nRefs(idx)
            if (ref.isEmpty) {
              none
            } else {
              val node = loadRef(ref)
              read(node, prefixTail)
            }
          } else {
            // Key not found
            none
          }

        case LeafR(nPrefix, value) =>
          if (nPrefix == prefix) value.some
          else none
      }

    /**
      * Write value on a key with prefix
      */
    def write(tree: TreeR, prefix: ByteVector, value: Option[ByteVector]): TreeR =
      value.map(update(tree, prefix, _)).getOrElse(delete(tree, prefix))

    /**
      * Insert or update key with prefix
      *
      * NOTE: Not tail recursive but it should be safe because tree can be only _prefix_ length deep.
      */
    def update(tree: TreeR, prefix: ByteVector, value: ByteVector): TreeR =
      tree match {
        case NodeR(nPrefix, nRefs) =>
          assert(prefix.size > nPrefix.size, "Storing shorter prefix then already stored.")

          val (commPrefix, prefixNew, nPrefixNew) = commonPrefix(prefix, nPrefix)

          if (nPrefixNew.isEmpty) {
            // Insert value in existing refs
            val (prefixNewIdx, prefixNewTail) = (toInt(prefixNew.head), prefixNew.tail)
            val ref                           = nRefs(prefixNewIdx)
            val node =
              if (ref == ByteVector.empty) {
                // No existing node, insert leaf
                LeafR(prefix.tail, value)
              } else {
                // Found node by prefix in refs, load update recursively
                val idxTree = loadRef(ref)
                val newTree = update(idxTree, prefixNewTail, value)
                newTree
              }
            val newRef  = saveRef(node)
            val newRefs = nRefs.updated(prefixNewIdx, newRef.bytes)
            NodeR(commPrefix ++ nPrefixNew, newRefs)
          } else {
            // Move existing refs, create sub-node for existing refs and leaf for insert
            val (nPrefixNewIdx, nPrefixNewTail) = (toInt(nPrefixNew.head), nPrefixNew.tail)
            val thisNode                        = NodeR(nPrefixNewTail, nRefs)
            val thisNodeRef                     = saveRef(thisNode)

            val (prefixNewIdx, prefixNewTail) = (toInt(prefixNew.head), prefixNew.tail)
            val newNode                       = LeafR(prefixNewTail, value)
            val newNodeRef                    = saveRef(newNode)

            val newRefs = emptyTreeRefs
              .updated(nPrefixNewIdx, thisNodeRef.bytes)
              .updated(prefixNewIdx, newNodeRef.bytes)

            NodeR(commPrefix, newRefs)
          }

        case LeafR(nPrefix, nValue) =>
          if (nPrefix == prefix) {
            // Update value
            LeafR(prefix, value)
          } else {
            val (commPrefix, prefixNew, nPrefixNew) = commonPrefix(prefix, nPrefix)

            // Create sub-node, insert existing and new leaf
            val (nPrefixNewIdx, nPrefixNewTail) = (toInt(nPrefixNew.head), nPrefixNew.tail)
            val thisNode                        = LeafR(nPrefixNewTail, nValue)
            val thisNodeRef                     = saveRef(thisNode)

            val (prefixNewIdx, prefixNewTail) = (toInt(prefixNew.head), prefixNew.tail)
            val newNode                       = LeafR(prefixNewTail, value)
            val newNodeRef                    = saveRef(newNode)

            val newRefs = emptyTreeRefs
              .updated(nPrefixNewIdx, thisNodeRef.bytes)
              .updated(prefixNewIdx, newNodeRef.bytes)

            NodeR(commPrefix, newRefs)
          }
      }

    /**
      * Delete key with prefix
      *
      * NOTE: Not tail recursive but it should be safe because tree can be only _prefix_ length deep.
      */
    def delete(tree: TreeR, prefix: ByteVector): TreeR =
      tree match {
        case existingNode @ NodeR(nPrefix, nRefs) =>
          assert(prefix.size > nPrefix.size, "Storing shorter prefix then already stored.")

          val (commPrefix, prefixNew, nPrefixNew) = commonPrefix(prefix, nPrefix)

          if (nPrefixNew.isEmpty) {
            // Match prefix with existing node, search in refs
            val (prefixNewIdx, prefixNewTail) = (toInt(prefixNew.head), prefixNew.tail)
            val ref                           = nRefs(prefixNewIdx)
            val node =
              if (ref == ByteVector.empty) {
                // Not found in existing refs
                emptyTree
              } else {
                // Found node by prefix in refs, load and delete recursively
                val idxTree = loadRef(ref)
                val newTree = delete(idxTree, prefixNewTail)
                newTree
              }
            // Check if node is empty
            if (node == emptyTree) {
              // Saving empty node
              val newRefs = nRefs.updated(prefixNewIdx, ByteVector.empty)
              if (newRefs == emptyTreeRefs) {
                // If all refs are empty, return whole empty node
                emptyTree
              } else {
                // New refs are not empty, update in existing node
                val nonEmptyRefs = newRefs.iterator.filter(_.nonEmpty).take(2).toList
                if (nonEmptyRefs.size == 1) {
                  // Only one refs, contract node
                  val ref           = nonEmptyRefs.head
                  val idx           = newRefs.indexOf(ref)
                  val refNode       = loadRef(ref)
                  val prefixWithIdx = nPrefix ++ ByteVector(idx)
                  // Update prefix
                  refNode match {
                    case NodeR(refPrefix, refRefs) =>
                      val newPrefix = prefixWithIdx ++ refPrefix
                      NodeR(newPrefix, refRefs)
                    case LeafR(refPrefix, refValue) =>
                      val newPrefix = prefixWithIdx ++ refPrefix
                      LeafR(newPrefix, refValue)
                  }
                } else {
                  // More then one refs, update existing node
                  NodeR(nPrefix, newRefs)
                }
              }
            } else {
              // Not empty node, save changes
              val newRef  = saveRef(node)
              val newRefs = nRefs.updated(prefixNewIdx, newRef.bytes)
              NodeR(commPrefix ++ nPrefixNew, newRefs)
            }
          } else {
            // Missing node, nothing to delete
            existingNode
          }

        case existingLeaf @ LeafR(nPrefix, _) =>
          if (nPrefix == prefix) {
            // Delete value
            emptyTree
          } else {
            existingLeaf
          }
      }

    /**
      * Pretty print radix tree
      */
    final def print(
        tree: TreeR,
        indentLevel: Int = 0,
        prefix: ByteVector = ByteVector.empty,
        treeKey: ByteVector = ByteVector.empty
    ): Unit = {
      val indent         = Seq.fill(indentLevel * 2)(" ").mkString
      def prn(s: String) = println(s"$indent$s")
      tree match {
        case NodeR(nPrefix, nRefs) =>
          prn(s"${prefix.toHex}${nPrefix.toHex} [${treeKey.toHex}]")

          nRefs.zipWithIndex.foreach {
            case (ref, idx) =>
              if (ref.nonEmpty) {
                val refNode = loadRef(ref)
                print(refNode, indentLevel + 1, ByteVector(idx), ref)
              }
          }

        case LeafR(nPrefix, value) =>
          prn(s"${prefix.toHex}${nPrefix.toHex}: ${value.toHex} [${treeKey.toHex}]")
      }
    }

  }

}
