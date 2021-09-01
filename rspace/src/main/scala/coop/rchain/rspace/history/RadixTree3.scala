package coop.rchain.rspace.history

import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.Codec
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object RadixTree3 {

  /**
    * Child Node structure (Empty,Leaf and NodePointer)
    * There is only one element type of element in Tree - it's Node = Vector[Child]
    */
  sealed trait Child
  final case object EmptyChild                                  extends Child
  final case class Leaf(prefix: ByteVector, value: ByteVector)  extends Child
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector) extends Child

  val numChilds = 256

  // Default values for empty Node
  val emptyNode: Vector[Child] = (0 until numChilds).map(_ => EmptyChild).toVector

  object codecs {
    import scodec.codecs._

    val codecByteVector: Codec[ByteVector] = variableSizeBytes(uint8, bytes)

    // Binary codecs for Tree

    val codecLeaf: Codec[Leaf] = (codecByteVector :: codecByteVector).as[Leaf]

    val codecNodePrt: Codec[NodePtr] = (codecByteVector :: codecByteVector).as[NodePtr]

    val codecChild: DiscriminatorCodec[Child, Int] =
      discriminated[Child]
        .by(uint8)
        .subcaseP(tag = 0) {
          case e: EmptyChild.type => e
        }(provide(EmptyChild))
        .subcaseP(tag = 1) {
          case l: Leaf => l
        }(codecLeaf)
        .subcaseP(tag = 2) {
          case n: NodePtr => n
        }(codecNodePrt)

    val codecNode: Codec[Vector[Child]] =
      vectorOfN(provide(numChilds), codecChild).as[Vector[Child]]
  }

  def commonPrefix(b1: ByteVector, b2: ByteVector): (ByteVector, ByteVector, ByteVector) = {
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

  def hashNode(node: Vector[Child]): (Blake2b256Hash, ByteVector) = {
    val newChildSer = codecs.codecNode.encode(node).require.toByteVector
    (hashFn(newChildSer), newChildSer)
  }

  class RadixTreeImpl(store: TrieMap[ByteVector, ByteVector]) {

    import codecs._

    def toInt(b: Byte): Int = b & 0xff

    def loadNodeOpt(nodePtr: ByteVector): Option[Vector[Child]] = {
      val nodeData = store.get(nodePtr)
      nodeData.map { data =>
        codecNode.decode(data.toBitVector).require.value
      }
    }

    def loadNode(nodePtr: ByteVector): Vector[Child] = {
      val node = loadNodeOpt(nodePtr)
      assert(node != none, s"Missing node in database. ptr=$nodePtr")
      node.get
    }

    def saveNode(node: Vector[Child]): Blake2b256Hash = {
      val (hash, bytes) = hashNode(node)
      store.update(hash.bytes, bytes)
      hash
    }

    def loadNodeWithDefault(hash: Blake2b256Hash): Vector[Child] =
      loadNodeOpt(hash.bytes).getOrElse(emptyNode)

    /**
      * Read leafValue with prefix
      */
    @tailrec
    final def read(curNode: Vector[Child], radixKey: ByteVector): Option[ByteVector] =
      if (radixKey.isEmpty) none
      else {
        curNode(toInt(radixKey.head)) match {
          case EmptyChild => none

          case Leaf(leafPrefix, value) =>
            if (leafPrefix == radixKey.tail) value.some //Happy end
            else none

          case NodePtr(ptrPrefix, ptr) =>
            val (_, prefixRest, ptrPrefixRest) = commonPrefix(radixKey.tail, ptrPrefix)
            if (ptrPrefixRest.isEmpty) read(loadNode(ptr), prefixRest)
            else none
        }
      }

    /**
      * Write value or delete on a key with prefix
      * Returns updated rootNode
      */
    def write(
        rootNode: Vector[Child],
        radixKey: ByteVector,
        radixValue: Option[ByteVector]
    ): Vector[Child] =
      radixValue.map(update(rootNode, radixKey, _)).getOrElse(delete(rootNode, radixKey))

    /**
      * Insert or update key with prefix
      * Returns updated curNode
      *
      * NOTE: Not tail recursive but it should be safe because tree can be only _prefix_ length deep.
      */
    def update(
        curNode: Vector[Child],
        prefix: ByteVector,
        insValue: ByteVector
    ): Vector[Child] = {
      assert(prefix.nonEmpty, "RadixKeys should be not empty")
      val (idx, insPrefix) = (toInt(prefix.head), prefix.tail)
      val newChild = curNode(idx) match {
        case EmptyChild => Leaf(insPrefix, insValue) //update EmptyChild to Leaf

        case Leaf(leafPrefix, leafValue) =>
          assert(leafPrefix.size == insPrefix.size, "All Radix keys should be same length")
          if (leafPrefix == insPrefix) Leaf(insPrefix, insValue) //update Leaf
          else {
            // Create child node, insert existing and new leaf in this node
            // intentionally not recursive for speed up
            val (commPrefix, insPrefixRest, leafPrefixRest) = commonPrefix(insPrefix, leafPrefix)
            val newNode = emptyNode
              .updated(toInt(leafPrefixRest.head), Leaf(leafPrefixRest.tail, leafValue))
              .updated(toInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            NodePtr(commPrefix, saveNode(newNode).bytes)
          }

        case NodePtr(ptrPrefix, ptr) =>
          assert(ptrPrefix.size < insPrefix.size, "Radix key should be longer than NodePtr key")
          val (commPrefix, insPrefixRest, ptrPrefixRest) = commonPrefix(insPrefix, ptrPrefix)
          if (ptrPrefixRest.isEmpty) {
            //add new node to existing child node
            val updatedChildNode = update(loadNode(ptr), insPrefixRest, insValue)
            NodePtr(commPrefix, saveNode(updatedChildNode).bytes)
          } else {
            // Create child node, insert existing Ptr and new leaf in this node
            val newNode = emptyNode
              .updated(toInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
              .updated(toInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            NodePtr(commPrefix, saveNode(newNode).bytes)
          }
      }
      curNode.updated(idx, newChild)
    }

    /**
      * Create node from child.
      * If child is NodePtr and prefix is empty - load node
      */
    def createNodeFromChild(child: Child): Vector[Child] =
      child match {
        case EmptyChild => emptyNode
        case Leaf(leafPrefix, leafValue) =>
          assert(
            leafPrefix.nonEmpty,
            "Impossible to create a node. LeafPrefix should be non empty."
          )
          emptyNode.updated(toInt(leafPrefix.head), Leaf(leafPrefix.tail, leafValue))
        case NodePtr(nodePtrPrefix, ptr) =>
          if (nodePtrPrefix.isEmpty) loadNode(ptr)
          else emptyNode.updated(toInt(nodePtrPrefix.head), NodePtr(nodePtrPrefix.tail, ptr))
      }

    /**
      * Optimize and save Node, create Child from this Node
      */
    def createChildFromNode(node: Vector[Child], childPrefix: ByteVector): Child = {
      val nonEmptyChilds = node.iterator.filter(_ != EmptyChild).take(2).toList
      nonEmptyChilds.size match {
        case 0 => EmptyChild //all childs are empty
        case 1 => //only one child is not empty - merge child and parrent nodes
          val childIdx = node.indexOf(nonEmptyChilds.head)
          nonEmptyChilds.head match {
            case EmptyChild => EmptyChild
            case Leaf(leafPrefix, value) =>
              Leaf(childPrefix ++ ByteVector(childIdx) ++ leafPrefix, value)
            case NodePtr(nodePtrPrefix, ptr) =>
              NodePtr(childPrefix ++ ByteVector(childIdx) ++ nodePtrPrefix, ptr)
          }
        case 2 => //2 or more shilds are not empty
          NodePtr(childPrefix, saveNode(node).bytes)
      }
    }

    /**
      * Delete key with prefix
      * Returns updated curNode
      *
      * NOTE: Not tail recursive but it should be safe because tree can be only _prefix_ length deep.
      */
    def delete(curNode: Vector[Child], prefix: ByteVector): Vector[Child] = {
      assert(prefix.nonEmpty, "RadixKeys should be non empty")
      val (idx, delPrefix) = (toInt(prefix.head), prefix.tail)
      val curChild         = curNode(idx)
      val newChild = curChild match {
        case EmptyChild => EmptyChild //Not found

        case existingLeaf @ Leaf(leafPrefix, _) =>
          if (leafPrefix == delPrefix) EmptyChild //Happy end
          else existingLeaf                       //Not found

        case existingNodePtr @ NodePtr(ptrPrefix, ptrPtr) =>
          val (_, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
          if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty) existingNodePtr //Not found
          else
            createChildFromNode(delete(loadNode(ptrPtr), delPrefixRest), ptrPrefix) //if ptrPrefixRest is empty
      }
      if (newChild != curChild) {
        curNode.updated(idx, newChild)
      } else curNode //Node is not changed
    }

    /**
      * Pretty print radix tree
      */
    final def print(curNode: Vector[Child], indentLevel: Int = 0): Unit = {
      val indent               = Seq.fill(indentLevel * 2)(" ").mkString
      def prn(s: String): Unit = println(s"$indent$s")
      curNode.zipWithIndex foreach {
        case (EmptyChild, _) => Unit

        case (Leaf(leafPrefix, value), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${leafPrefix.toHex}: ${value.toHex}")

        case (NodePtr(ptrPrefix, ptr), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${ptrPrefix.toHex} [${ptr.toHex}]")
          print(loadNode(ptr), indentLevel + 1)
      }
    }
  }
}
