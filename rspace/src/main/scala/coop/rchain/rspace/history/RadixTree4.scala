package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.Codec
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.language.higherKinds

object RadixTree4 {

  /**
    * Child Node structure (Empty,Leaf and NodePointer)
    * There is only one element type of element in Tree - it's Node = Vector[Child]
    */
  sealed trait Child
  final case object EmptyChild                                  extends Child
  final case class Leaf(prefix: ByteVector, value: ByteVector)  extends Child
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector) extends Child

  type Node = Vector[Child]

  final case class NodePathRecord(idx: Int, node: Node)
  type NodePath = List[NodePathRecord]
  val emptyNodePath: NodePath = List.empty

  val numChilds = 256

  // Default values for empty Node
  val emptyNode: Node = (0 until numChilds).map(_ => EmptyChild).toVector

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

    /**
      * Encoded format:
... do how ScodecSerialize.scala      */
    val codecNode: Codec[Node] =
      vectorOfN(provide(numChilds), codecChild).as[Node]
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

  def hashNode(node: Node): (Blake2b256Hash, ByteVector) = {
    val newChildSer = codecs.codecNode.encode(node).require.toByteVector
    (hashFn(newChildSer), newChildSer)
  }

  class RadixTree4Impl[F[_]: Sync](store: RadixStore[F]) {

    import codecs._

    def toInt(b: Byte): Int = b & 0xff

    def loadNodeOpt(nodePtr: ByteVector): F[Option[Node]] =
      store
        .get(Seq(nodePtr))
        .map(nodeData => nodeData.head.map(v => codecNode.decode(v.toBitVector).require.value))

    def loadNode(nodePtr: ByteVector): F[Node] =
      loadNodeOpt(nodePtr).map { nodeOpt =>
        assert(nodeOpt != none, s"Missing node in database. ptr=$nodePtr")
        nodeOpt.get
      }

    def saveNode(node: Node): F[Blake2b256Hash] = {
      val (hash, bytes) = hashNode(node)
      store.put(Seq((hash.bytes, bytes))).map(_ => hash)
    }

    def loadNodeWithDefault(hash: Blake2b256Hash): F[Node] =
      loadNodeOpt(hash.bytes).map(n => n.getOrElse(emptyNode))

    /**
      * Read leafValue with prefix
      */
    final def read(startNode: Node, startKey: ByteVector): F[Option[ByteVector]] = {
      type Params = (Node, ByteVector)
      def go(params: Params): F[Either[Params, Option[ByteVector]]] =
        params match {
          case (_, ByteVector.empty) => Sync[F].pure(None.asRight)
          case (curNode, radixKey) =>
            curNode(toInt(radixKey.head)) match {
              case EmptyChild => Sync[F].pure(None.asRight)

              case Leaf(leafPrefix, value) =>
                if (leafPrefix == radixKey.tail) Sync[F].pure(value.some.asRight) //Happy end
                else Sync[F].pure(None.asRight)

              case NodePtr(ptrPrefix, ptr) =>
                val (_, prefixRest, ptrPrefixRest) = commonPrefix(radixKey.tail, ptrPrefix)
                if (ptrPrefixRest.isEmpty) loadNode(ptr).map(n => (n, prefixRest).asLeft)
                else Sync[F].pure(None.asRight)
            }
        }
      Sync[F].tailRecM(startNode, startKey)(go)
    }

    /**
      * Create node from child.
      * If child is NodePtr and prefix is empty - load node
      */
    def createNodeFromChild(child: Child): F[Node] =
      child match {
        case EmptyChild => Sync[F].pure(emptyNode)
        case Leaf(leafPrefix, leafValue) =>
          assert(
            leafPrefix.nonEmpty,
            "Impossible to create a node. LeafPrefix should be non empty."
          )
          Sync[F].pure(emptyNode.updated(toInt(leafPrefix.head), Leaf(leafPrefix.tail, leafValue)))
        case NodePtr(nodePtrPrefix, ptr) =>
          if (nodePtrPrefix.isEmpty) loadNode(ptr)
          else
            Sync[F].pure(
              emptyNode.updated(toInt(nodePtrPrefix.head), NodePtr(nodePtrPrefix.tail, ptr))
            )
      }

    /**
      * Optimize and save Node, create Child from this Node
      */
    def saveNodeAndCreateChild(
        node: Node,
        childPrefix: ByteVector,
        optimization: Boolean
    ): F[Child] =
      if (optimization) {
        val nonEmptyChilds = node.iterator.filter(_ != EmptyChild).take(2).toList
        nonEmptyChilds.size match {
          case 0 => Sync[F].pure(EmptyChild) //all childs are empty
          case 1 => //only one child is not empty - merge child and parrents nodes
            val childIdx = node.indexOf(nonEmptyChilds.head)
            nonEmptyChilds.head match {
              case EmptyChild => Sync[F].pure(EmptyChild)
              case Leaf(leafPrefix, value) =>
                Sync[F].pure(Leaf(childPrefix ++ ByteVector(childIdx) ++ leafPrefix, value))
              case NodePtr(nodePtrPrefix, ptr) =>
                Sync[F].pure(NodePtr(childPrefix ++ ByteVector(childIdx) ++ nodePtrPrefix, ptr))
            }
          case 2 => //2 or more shilds are not empty
            saveNode(node).map(ptr => NodePtr(childPrefix, ptr.bytes))
        }
      } else saveNode(node).map(ptr => NodePtr(childPrefix, ptr.bytes))

    //        var childNode = nodePath(nodePath.size).node
    //        nodePath.indices.reverse.foreach { i =>
    //          if (i >= 2) {
    //            val (idxChild, parentNode) = (nodePath(i - 1).idx, nodePath(i - 1).node)
    //            parentNode(idxChild) match {
    //              case NodePtr(ptrPrefix, _) =>
    //                saveNodeAndCreateChild(childNode, ptrPrefix, optimization).map(
    //                  newChild => childNode = parentNode.updated(idxChild, newChild)
    //                )
    //            }
    //          }
    //        }
    //        childNode

    def saveNodePath(nodePath: NodePath, optimization: Boolean): F[Node] =
      nodePath.size match {
        case 0 => Sync[F].pure(emptyNode)
        case 1 => Sync[F].pure(nodePath.head.node) //not save, return top node
        case _ => //save and rehash nodes from bottom to top-1, return top node
          val indexSeq   = nodePath.indices.filter(_ >= 1)
          val bottomNode = Sync[F].pure(nodePath.last.node)
          indexSeq.foldRight(bottomNode) {
            case (i, fChildNode) =>
              val (idxChild, parentNode) = (nodePath(i - 1).idx, nodePath(i - 1).node)
              parentNode(idxChild) match {
                case EmptyChild =>
                  assert(assertion = false, "Wrong nodePath")
                  Sync[F].pure(emptyNode)
                case Leaf(_, _) =>
                  assert(assertion = false, "Wrong nodePath")
                  Sync[F].pure(emptyNode)
                case NodePtr(ptrPrefix, _) =>
                  for {
                    childNode <- fChildNode
                    newChild  <- saveNodeAndCreateChild(childNode, ptrPrefix, optimization)
                  } yield parentNode.updated(idxChild, newChild)
              }
          }
      }

    /**
      * Insert or update key with prefix
      * Returns updated curNode
      */
    def update(
        startNode: Node,
        startPrefix: ByteVector,
        insValue: ByteVector
    ): F[Node] = {
      type Params = (Node, ByteVector, ByteVector, NodePath)

      def go(params: Params): F[Either[Params, NodePath]] = {
        val (curNode, prefix, insValue, nodePath) = params
        assert(prefix.nonEmpty, "RadixKeys should be not empty")
        val (idx, insPrefix) = (toInt(prefix.head), prefix.tail)
        curNode(idx) match {
          case EmptyChild => //update EmptyChild to Leaf
            Sync[F].pure(
              (nodePath :+ NodePathRecord(idx, curNode.updated(idx, Leaf(insPrefix, insValue)))).asRight
            )

          case Leaf(leafPrefix, leafValue) =>
            assert(leafPrefix.size == insPrefix.size, "All Radix keys should be same length")
            if (leafPrefix == insPrefix) //update Leaf
              Sync[F].pure(
                (nodePath :+ NodePathRecord(idx, curNode.updated(idx, Leaf(insPrefix, insValue)))).asRight
              )
            else {
              // Create child node, insert existing and new leaf in this node
              // intentionally not recursive for speed up
              val (commPrefix, insPrefixRest, leafPrefixRest) = commonPrefix(insPrefix, leafPrefix)
              val newLeafIdx                                  = toInt(insPrefixRest.head)
              val newNode = emptyNode
                .updated(toInt(leafPrefixRest.head), Leaf(leafPrefixRest.tail, leafValue))
                .updated(newLeafIdx, Leaf(insPrefixRest.tail, insValue))
              Sync[F].pure(
                (nodePath :+ NodePathRecord(
                  idx,
                  curNode.updated(idx, NodePtr(commPrefix, ByteVector.empty))
                ) :+ NodePathRecord(newLeafIdx, newNode)).asRight
              )
            }

          case NodePtr(ptrPrefix, ptrPtr) =>
            assert(ptrPrefix.size < insPrefix.size, "Radix key should be longer than NodePtr key")
            val (commPrefix, insPrefixRest, ptrPrefixRest) = commonPrefix(insPrefix, ptrPrefix)
            if (ptrPrefixRest.isEmpty) {
              //add new node to existing child node
              loadNode(ptrPtr).map(
                n => (n, insPrefixRest, insValue, nodePath :+ NodePathRecord(idx, curNode)).asLeft
              ) //Dip
            } else {
              // Create child node, insert existing Ptr and new leaf in this node
              val newLeafIdx = toInt(insPrefixRest.head)
              val newNode = emptyNode
                .updated(toInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptrPtr))
                .updated(newLeafIdx, Leaf(insPrefixRest.tail, insValue))
              Sync[F].pure(
                (nodePath :+ NodePathRecord(
                  idx,
                  curNode.updated(idx, NodePtr(commPrefix, ByteVector.empty))
                ) :+ NodePathRecord(newLeafIdx, newNode)).asRight
              )
            }
        }
      }
      //Create nodePath (accumulate nodes)
      val fNodePath = Sync[F].tailRecM(startNode, startPrefix, insValue, emptyNodePath)(go)
      //Save and rehash nodes from accumulator
      for {
        nodePath <- fNodePath
        result   <- saveNodePath(nodePath, optimization = false)
      } yield if (nodePath.nonEmpty) result else startNode
    }

    /**
      * Delete key with prefix
      * Returns updated curNode
      */
    def delete(
        startNode: Node,
        startPrefix: ByteVector
    ): F[Node] = {
      type Params = (Node, ByteVector, NodePath)
      def go(params: Params): F[Either[Params, NodePath]] = {
        val (curNode, prefix, nodePath) = params
        assert(prefix.nonEmpty, "RadixKeys should be non empty")
        val (idx, delPrefix) = (toInt(prefix.head), prefix.tail)
        curNode(idx) match {
          case EmptyChild => Sync[F].pure(emptyNodePath.asRight) //Not found

          case Leaf(leafPrefix, _) =>
            if (leafPrefix == delPrefix) //Happy end
              Sync[F].pure(
                (nodePath :+ NodePathRecord(idx, curNode.updated(idx, EmptyChild))).asRight
              )
            else Sync[F].pure(emptyNodePath.asRight) //Not found

          case NodePtr(ptrPrefix, ptrPtr) =>
            val (_, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
            if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty)
              Sync[F].pure(emptyNodePath.asRight) //Not found
            else
              loadNode(ptrPtr).map(
                n => (n, delPrefixRest, nodePath :+ NodePathRecord(idx, curNode)).asLeft
              ) //Dip
        }
      }
      //Create nodePath (accumulate nodes)
      val fNodePath = Sync[F].tailRecM(startNode, startPrefix, emptyNodePath)(go)
      //Save and rehash nodes from accumulator
      for {
        nodePath <- fNodePath
        result   <- saveNodePath(nodePath, optimization = true)
      } yield if (nodePath.nonEmpty) result else startNode
    }

    /**
      * Write value or delete on a key with prefix
      * Returns updated rootNode
      */
    def write(
        rootNode: Node,
        radixKey: ByteVector,
        radixValue: Option[ByteVector]
    ): F[Node] =
      radixValue.map(update(rootNode, radixKey, _)).getOrElse(delete(rootNode, radixKey))

    /**
      * Pretty print radix tree
      */
    final def print(startNode: Node): F[Unit] = {
      type Params = (NodePath, Int)
      def go(params: Params): F[Either[Params, Unit]] = {
        val (nodePath, indentLevel) = params
        if (nodePath.isEmpty) Sync[F].pure(().asRight)
        else {
          val curNode              = nodePath.last.node
          val startIdx             = nodePath.last.idx + 1
          val indent               = Seq.fill(indentLevel * 2)(" ").mkString
          def prn(s: String): Unit = println(s"$indent$s")
          curNode.zipWithIndex.filter(_._2 >= startIdx) foreach {

            case (EmptyChild, _) => Unit

            case (Leaf(leafPrefix, value), idx) =>
              prn(s"${idx.toHexString.toUpperCase}${leafPrefix.toHex}: ${value.toHex}")

            case (NodePtr(ptrPrefix, ptr), idx) =>
              prn(s"${idx.toHexString.toUpperCase}${ptrPrefix.toHex} [${ptr.toHex}]")
              loadNode(ptr).map { childNode =>
                val newNodePath = nodePath.dropRight(1) :+
                  NodePathRecord(idx, curNode) :+ NodePathRecord(-1, childNode)
                (newNodePath, indentLevel + 1).asLeft
              }
          }
          Sync[F].pure((nodePath.dropRight(1), indentLevel - 1).asLeft)
        }
      }
      Sync[F].tailRecM(emptyNodePath :+ NodePathRecord(-1, startNode), 0)(go)
    }
  }
}
