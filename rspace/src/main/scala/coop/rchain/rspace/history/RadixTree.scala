package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.language.higherKinds

/**
  * Radix Tree implementation
  */
object RadixTree {

  /**
    * Node structure (EmptyItem, Leaf and NodePointer)
    * There is only one type of element in Tree [[Node]] = Vector[Item]
    */
  sealed trait Item
  final case object EmptyItem                                   extends Item
  final case class Leaf(prefix: ByteVector, value: ByteVector)  extends Item
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector) extends Item

  type Node = Vector[Item]

  /**
    * number of items always constant
    */
  val numItems = 256

  /**
    * Empty node consists only of [[EmptyItem]]'s
    */
  val emptyNode: Node = (0 until numItems).map(_ => EmptyItem).toVector

  /**
    * Binary codecs for serializing/deserializing Node in Radix tree
    * Coding structure for items:
    *   EmptyItem                   - Empty (not encode)

    *   Leaf(prefix,value)    -> [item number] [second byte] [prefix0]..[prefixM] [value0]..[value31]
    *                               where is: [second byte] -> bit7 = 0 (Leaf identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    *
    *   NodePtr(prefix,ptr)   -> [item number] [second byte] [prefix0]..[prefixM] [ptr0]..[ptr31]
    *                               where is: [second byte] -> bit7 = 1 (NodePtr identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    */
  object codecs {
    private val defSize = 32

    def encode(node: Node): Array[Byte] = {
      //calculating size of buffer
      val sizeNode = node.size

      @tailrec
      def loopSize(index: Int, size: Int): Int =
        if (sizeNode > index)
          node(index) match {
            case EmptyItem => loopSize(index + 1, size)

            case Leaf(leafPrefix, value) =>
              val sizePrefix = leafPrefix.size.toInt
              assert(sizePrefix <= 127, "error during serialization (size of prefix more than 127)")
              assert(
                value.size == defSize,
                "error during serialization (size of leafValue not equal 32)"
              )
              loopSize(index + 1, size + 2 + sizePrefix + defSize)

            case NodePtr(ptrPrefix, ptr) =>
              val sizePrefix = ptrPrefix.size.toInt
              assert(sizePrefix <= 127, "error during serialization (size of prefix more than 127)")
              assert(
                ptr.size == defSize,
                "error during serialization (size of ptrPrefix not equal 32)"
              )
              loopSize(index + 1, size + 2 + sizePrefix + defSize)
          } else size

      val size = loopSize(0, 0)
      //put in the buffer

      val arr = new Array[Byte](size)
      @tailrec
      def loopFill(numItem: Int, idx0: Int): Array[Byte] =
        if (idx0 == size) arr
        else {
          node(numItem) match {
            case EmptyItem => loopFill(numItem + 1, idx0)

            case Leaf(prefix, value) =>
              arr(idx0) = numItem.toByte
              val idxSecondByte   = idx0 + 1
              val idxPrefixStart  = idxSecondByte + 1
              val prefixSize: Int = prefix.size.toInt
              //size & 01111111b
              arr(idxSecondByte) = (prefixSize & 0x7F).toByte //second byte - type and size of prefix
              for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
              val idxValueStart = idxPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(idxValueStart + i) = value(i.toLong)
              loopFill(numItem + 1, idxValueStart + defSize)
            case NodePtr(prefix, ptr) =>
              arr(idx0) = numItem.toByte
              val idxSecondByte   = idx0 + 1
              val prefixSize: Int = prefix.size.toInt
              //10000000b | (size & 01111111b)
              arr(idxSecondByte) = (0x80 | (prefixSize & 0x7F)).toByte //second byte - type and size of prefix
              val idxPrefixStart = idxSecondByte + 1
              for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
              val idxPtrStart = idxPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(idxPtrStart + i) = ptr(i.toLong)
              loopFill(numItem + 1, idxPtrStart + defSize)
          }
        }
      loopFill(0, 0)
    }

    def decode(arr: Array[Byte]): Node = {
      val maxSize = arr.length
      @tailrec
      def loop(idx0: Int, node: Node): Node =
        if (idx0 == maxSize) node
        else {
          val (idx0Next, nodeNext) = try {
            val numItem: Int = byteToInt(arr(idx0))
            assert(
              node(numItem) == EmptyItem,
              "error during deserialization (wrong number of item)"
            )
            val idx1       = idx0 + 1
            val secondByte = arr(idx1)

            val prefixSize: Int = secondByte & 0x7F
            val prefix          = new Array[Byte](prefixSize)
            val idxPrefixStart  = idx1 + 1
            for (i <- 0 until prefixSize) prefix(i) = arr(idxPrefixStart + i)
            val valOrPtr         = new Array[Byte](defSize)
            val idxValOrPtrStart = idxPrefixStart + prefixSize
            for (i <- 0 until defSize) valOrPtr(i) = arr(idxValOrPtrStart + i)
            val idx0Next = idxValOrPtrStart + defSize
            val item = if ((secondByte & 0x80) == 0x00) { //Leaf
              Leaf(ByteVector(prefix), ByteVector(valOrPtr))
            } else { //NodePtr
              NodePtr(ByteVector(prefix), ByteVector(valOrPtr))
            }
            (idx0Next, node.updated(numItem, item))
          } catch {
            case _: Exception =>
              assert(assertion = false, "error during deserialization (out of range)")
              (maxSize, emptyNode)
          }
          loop(idx0Next, nodeNext)
        }
      loop(0, emptyNode)
    }
  }

  /**
    * Find the common part of b1 and b2.
    * Return (Common part , rest of b1, rest of b2).
    */
  def commonPrefix(b1: ByteVector, b2: ByteVector): (ByteVector, ByteVector, ByteVector) = {
    @tailrec
    def go(
        common: ByteVector,
        l: ByteVector,
        r: ByteVector
    ): (ByteVector, ByteVector, ByteVector) =
      if (r.isEmpty) {
        (common, l, r)
      } else {
        val lHead = l.head
        val rHead = r.head
        if (lHead == rHead) go(common :+ lHead, l.tail, r.tail)
        else (common, l, r)
      }
    go(ByteVector.empty, b1, b2)
  }

  /**
    * Hashing serial data.
    * Return Blake2b256 hash of input data
    */
  def hashNode(node: Node): (ByteVector, Array[Byte]) = {
    import coop.rchain.crypto.hash.Blake2b256
    val bytes = codecs.encode(node)
    (ByteVector(Blake2b256.hash(bytes)), bytes)
  }

  def byteToInt(b: Byte): Int = b & 0xff

  class RadixTreeImpl[F[_]: Sync: Parallel](store: RadixStore[F]) {

    /**
      * Load and decode serializing data from KVDB.
      */
    private def loadNodeFromStore(nodePtr: ByteVector): F[Option[Node]] =
      for {
        nodeOpt <- store.get(Seq(nodePtr))
        r = nodeOpt.head match {
          case None       => None
          case Some(node) => Some(codecs.decode(node))
        }
      } yield r

    /**
      * Cache for storing read and decoded nodes.
      * In cache load kv-pair (hash, node).
      * Where hash  - Blake2b256Hash of serializing nodes data,
      *       node - deserialized data of this node.
      */
    private val cacheR: TrieMap[ByteVector, Node] = TrieMap.empty

    /**
      * Load one node from [[cacheR]].
      * If there is no such record in cache - load and decode from KVDB, then save to cacheR.
      * If there is no such record in KVDB - execute assert (if set noAssert flag - return emptyNode).
      */
    def loadNode(nodePtr: ByteVector, noAssert: Boolean = false): F[Node] =
      for {
        cacheNodeOpt <- Sync[F].delay(cacheR.get(nodePtr))
        res <- cacheNodeOpt match {
                case None =>
                  for {
                    storeNodeOpt <- loadNodeFromStore(nodePtr)
                    r = storeNodeOpt match {
                      case None =>
                        assert(noAssert, s"Missing node in database. ptr=${nodePtr.toHex}")
                        emptyNode
                      case Some(storeNode) =>
                        cacheR.update(nodePtr, storeNode)
                        storeNode
                    }
                  } yield r
                case Some(cacheNode) => Sync[F].pure(cacheNode)
              }
      } yield res

    /**
      * Clear [[cacheR]] (cache for storing read nodes).
      */
    def clearReadCache(): Unit = cacheR.clear()

    /**
      * Cache for storing serializing nodes. For subsequent unloading in KVDB
      * In cache store kv-pair (hash, bytes).
      * Where hash -  Blake2b256Hash of bytes,
      *       bytes - serializing data of nodes.
      */
    private val cacheW: TrieMap[ByteVector, Array[Byte]] = TrieMap.empty

    /**
      * Serializing and hashing one [[Node]].
      * Serializing data load in [[cacheW]].
      * If detected collision with older cache data - executing assert
      */
    def saveNode(node: Node): ByteVector = {
      val (hash, bytes) = hashNode(node)
      cacheR.get(hash) match { // collision alarm
        case Some(v) =>
          assert(
            v == node,
            s"collision in cache (record with key = ${hash.toHex} has already existed)"
          )
        case None => cacheR.update(hash, node)
      }
      cacheW.update(hash, bytes)
      hash
    }

    /**
      * Save all cacheW to KVDB
      * If detected collision with older KVDB data - execute assert
      */
    def commit(): F[Unit] = {
      val kvPairs = cacheW.toList
      for {
        ifAbsent          <- store.contains(kvPairs.map(_._1))
        kvIfAbsent        = kvPairs zip ifAbsent
        kvExist           = kvIfAbsent.filter(_._2).map(_._1)
        valueExistInStore <- store.get(kvExist.map(_._1))
        kvvExist          = kvExist zip valueExistInStore.map(_.getOrElse(Array.empty))
        kvCollision       = kvvExist.filter(kvv => !(kvv._1._2 sameElements kvv._2)).map(_._1)
        kvAbsent = if (kvCollision.isEmpty) kvIfAbsent.filterNot(_._2).map(_._1)
        else {
          assert(
            assertion = false,
            s"${kvCollision.length} collisions in KVDB (first collision with key = ${kvCollision.head._1.toHex})"
          )
          List.empty
        }
        _ <- store.put(kvAbsent)
      } yield ()
    }

    /**
      * Clear [[cacheW]] (cache for storing data to write in KVDB).
      */
    def clearWriteCache(): Unit = cacheW.clear()

    /**
      * Read leaf data with prefix. If data not found, returned [[None]]
      */
    final def read(startNode: Node, startPrefix: ByteVector): F[Option[ByteVector]] = {
      type Params = (Node, ByteVector)
      def go(params: Params): F[Either[Params, Option[ByteVector]]] =
        params match {
          case (_, ByteVector.empty) => Sync[F].pure(None.asRight) //Not found
          case (curNode, prefix) =>
            curNode(byteToInt(prefix.head)) match {
              case EmptyItem => Sync[F].pure(None.asRight) //Not found

              case Leaf(leafPrefix, value) =>
                if (leafPrefix == prefix.tail) Sync[F].pure(value.some.asRight) //Happy end
                else Sync[F].pure(None.asRight)                                 //Not found

              case NodePtr(ptrPrefix, ptr) =>
                val (_, prefixRest, ptrPrefixRest) = commonPrefix(prefix.tail, ptrPrefix)
                if (ptrPrefixRest.isEmpty) loadNode(ptr).map(n => (n, prefixRest).asLeft) //Deeper
                else Sync[F].pure(None.asRight)                                           //Not found
            }
        }
      Sync[F].tailRecM(startNode, startPrefix)(go)
    }

    /**
      * Create node from [[Item]].
      * If item is NodePtr and prefix is empty - load child node
      */
    private def createNodeFromItem(item: Item): F[Node] =
      item match {
        case EmptyItem => emptyNode.pure
        case Leaf(leafPrefix, leafValue) =>
          assert(
            leafPrefix.nonEmpty,
            "Impossible to create a node. LeafPrefix should be non empty."
          )
          emptyNode.updated(byteToInt(leafPrefix.head), Leaf(leafPrefix.tail, leafValue)).pure
        case NodePtr(nodePtrPrefix, ptr) =>
          if (nodePtrPrefix.isEmpty) loadNode(ptr)
          else
            emptyNode.updated(byteToInt(nodePtrPrefix.head), NodePtr(nodePtrPrefix.tail, ptr)).pure
      }

    /**
      * Optimize and save Node, create item from this Node
      */
    private def saveNodeAndCreateItem(
        node: Node,
        prefix: ByteVector,
        optimization: Boolean = true
    ): Item =
      if (optimization) {
        val nonEmptyItems = node.iterator.filter(_ != EmptyItem).take(2).toList
        nonEmptyItems.size match {
          case 0 => EmptyItem //all items are empty
          case 1 => //only one item is not empty - merge child and parent nodes
            val idxItem = node.indexOf(nonEmptyItems.head)
            nonEmptyItems.head match {
              case EmptyItem => EmptyItem
              case Leaf(leafPrefix, value) =>
                Leaf(prefix ++ ByteVector(idxItem) ++ leafPrefix, value)
              case NodePtr(nodePtrPrefix, ptr) =>
                NodePtr(prefix ++ ByteVector(idxItem) ++ nodePtrPrefix, ptr)
            }
          case 2 => //2 or more items are not empty
            NodePtr(prefix, saveNode(node))
        }
      } else NodePtr(prefix, saveNode(node))

    /**
      * Save new leaf value to this part of tree (start from curItems).
      * Rehash and save all depend node to cacheW. Return updated curItems.
      * If exist leaf with same prefix but different value - update leaf value.
      * If exist leaf with same prefix and same value - return [[None]].
      */
    def update(
        curItem: Item,
        insPrefix: ByteVector,
        insValue: ByteVector
    ): F[Option[Item]] =
      curItem match {
        case EmptyItem =>
          (Leaf(insPrefix, insValue): Item).some.pure //update EmptyItem to Leaf

        case Leaf(leafPrefix, leafValue) =>
          assert(leafPrefix.size == insPrefix.size, "All Radix keys should be same length")
          if (leafPrefix == insPrefix) {
            if (insValue == leafValue) none[Item].pure
            else (Leaf(insPrefix, insValue): Item).some.pure
          } //update Leaf
          else {
            // Create child node, insert existing and new leaf in this node
            // intentionally not recursive for speed up
            val (commPrefix, insPrefixRest, leafPrefixRest) = commonPrefix(insPrefix, leafPrefix)
            val newNode = emptyNode
              .updated(byteToInt(leafPrefixRest.head), Leaf(leafPrefixRest.tail, leafValue))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            saveNodeAndCreateItem(newNode, commPrefix, optimization = false).some.pure
          }

        case NodePtr(ptrPrefix, ptr) =>
          assert(ptrPrefix.size < insPrefix.size, "Radix key should be longer than NodePtr key")
          val (commPrefix, insPrefixRest, ptrPrefixRest) = commonPrefix(insPrefix, ptrPrefix)
          if (ptrPrefixRest.isEmpty) {
            val (childItemIdx, childInsPrefix) =
              (byteToInt(insPrefixRest.head), insPrefixRest.tail)
            //add new node to existing child node
            for {
              childNode    <- loadNode(ptr)
              childItemOpt <- update(childNode(childItemIdx), childInsPrefix, insValue) //deeper
              returnedItem = childItemOpt match {
                case None => none
                case Some(childItem) =>
                  val updatedChildNode = childNode.updated(childItemIdx, childItem)
                  saveNodeAndCreateItem(updatedChildNode, commPrefix, optimization = false).some
              }
            } yield returnedItem
          } else {
            // Create child node, insert existing Ptr and new leaf in this node
            val newNode = emptyNode
              .updated(byteToInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            saveNodeAndCreateItem(newNode, commPrefix, optimization = false).some.pure
          }
      }

    /**
      * Delete leaf value from this part of tree (start from curItem).
      * Rehash and save all depend node to cacheW. Return updated curItem.
      * If not found leaf with  delPrefix - return [[None]].
      */
    def delete(curItem: Item, delPrefix: ByteVector): F[Option[Item]] =
      curItem match {
        case EmptyItem => none[Item].pure //Not found

        case Leaf(leafPrefix, _) =>
          if (leafPrefix == delPrefix) (EmptyItem: Item).some.pure //Happy end
          else none[Item].pure                                     //Not found

        case NodePtr(ptrPrefix, ptr) =>
          val (commPrefix, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
          if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty) none[Item].pure //Not found
          else {
            val (childItemIdx, childDelPrefix) =
              (byteToInt(delPrefixRest.head), delPrefixRest.tail)
            for {
              childNode    <- loadNode(ptr)
              childItemOpt <- delete(childNode(childItemIdx), childDelPrefix) //deeper
              returnedChild = childItemOpt match {
                case None => none
                case Some(childItem) =>
                  saveNodeAndCreateItem(childNode.updated(childItemIdx, childItem), commPrefix).some
              }
            } yield returnedChild
          }
      }

    /**
      * Parallel processing of HistoryActions in this part of tree (start from curNode).
      * New data load to [[cacheW]].
      * Return updated curNode. if no action was taken - return [[None]].
      */
    def makeActions(curNode: Node, curActions: List[HistoryAction]): F[Option[Node]] = {
      assert(
        !curActions.exists(_.key.isEmpty),
        "The length of all prefixes in the subtree must be the same"
      )
      val groups = curActions.groupBy(_.key.head).toList
      val newGroupItemsF = groups.map {
        case (groupIdx, groupActions) =>
          val index = byteToInt(groupIdx)
          val item  = curNode(index)
          if (groupActions.length == 1) {
            val newItem = groupActions.head match {
              case InsertAction(key, hash) =>
                update(item, ByteVector(key).tail, hash.bytes)
              case DeleteAction(key) => delete(item, ByteVector(key).tail)
            }
            newItem.map((index, _))
          } else {
            val notExistInsertAction = groupActions.collectFirst { case _: InsertAction => true }.isEmpty
            val clearGroupActions =
              if (item == EmptyItem && notExistInsertAction) groupActions.collect {
                case v: InsertAction => v
              } else groupActions
            if (clearGroupActions.isEmpty) (index, none[Item]).pure
            else {
              val newActions = clearGroupActions.map {
                case InsertAction(key, hash) => InsertAction(key.tail, hash)
                case DeleteAction(key)       => DeleteAction(key.tail)
              }
              for {
                createdNode <- createNodeFromItem(curNode(index))
                newNodeOpt  <- makeActions(createdNode, newActions)
                newItem = newNodeOpt match {
                  case None          => none
                  case Some(newNode) => saveNodeAndCreateItem(newNode, ByteVector.empty).some
                }
              } yield (index, newItem)
            }
          }
      }
      for {
        newGroupItems <- newGroupItemsF.parSequence
        newCurNode = newGroupItems.foldLeft(curNode) {
          case (tempNode, (index, newItemOpt)) =>
            newItemOpt.map(tempNode.updated(index, _)).getOrElse(tempNode)
        }
      } yield if (newCurNode != curNode) newCurNode.some else none
    }

    /**
      * Pretty print radix tree
      */
    final def print(curNode: Node, indentLevel: Int = 0): Unit = {
      val indent               = Seq.fill(indentLevel * 2)(" ").mkString
      def prn(s: String): Unit = println(s"$indent$s")
      curNode.zipWithIndex foreach {
        case (EmptyItem, _) => Unit

        case (Leaf(leafPrefix, value), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${leafPrefix.toHex}: ${value.toHex}")

        case (NodePtr(ptrPrefix, ptr), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${ptrPrefix.toHex} [${ptr.toHex}]")
          loadNode(ptr).map(print(_, indentLevel + 1))
      }
    }
  }
}
