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
    * Child Node structure (Empty,Leaf and NodePointer)
    * There is only one type of element in Tree [[Node]] = Vector[Child]
    */
  sealed trait Child
  final case object EmptyChild                                                     extends Child
  final case class Leaf(prefix: ByteVector, value: ByteVector, varLength: Boolean) extends Child
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector)                    extends Child

  type Node = Vector[Child]

  /**
    * number of children always constant
    */
  val numChildren = 256

  /**
    * Empty node consists only of [[EmptyChild]]'s
    */
  val emptyNode: Node = (0 until numChildren).map(_ => EmptyChild).toVector

  /**
    * Binary codecs for serializing/deserializing Node in Radix tree
    * Child coding structure:
    *   EmptyChild                   - Empty (not encode)

    *   Leaf(prefix,value,varLength)
    *           if(varLength) -> [child number] [second byte] [prefix0]..[prefixM] [sizeValue=N] [value0]..[valueN]
    *           else          -> [child number] [second byte] [prefix0]..[prefixM] [value0]..[value31]
    *                            [second byte] -> bit7 = 0 (Leaf identifier)
    *                                             bit6 = varLength flag (0 - value have length 32 bytes
    *                                                                    1  - value have variable length 0..255 bytes)
    *                                             bit5 = reserv
    *                                             bit4..bit0 - prefix length = M (from 0 to 31)
    *
    *   NodePtr(prefix,ptr)   -> [child number] [second byte] [prefix0]..[prefixM] [ptr0]..[ptr31]
    *                            [second byte] -> bit7 = 1 (NodePtr identifier)
    *                                             bit6 = reserv
    *                                             bit5 = reserv
    *                                             bit4..bit0 - prefix length = M (from 0 to 31)
    */
  object codecs {
    def encode(node: Node): Array[Byte] = {
      //calculating size of buffer
      val sizeNode = node.size

      @tailrec
      def loopSize(index: Int, size: Int): Int =
        if (sizeNode > index)
          node(index) match {
            case EmptyChild => loopSize(index + 1, size)

            case Leaf(leafPrefix, value, varLength) =>
              val sizePrefix = leafPrefix.size.toInt
              val sizeValue  = value.size.toInt
              val sizeLength = if (varLength) {
                assert(
                  value.size <= 255,
                  "error during deserialization (size of value more than 255)"
                )
                1
              } else {
                assert(
                  sizeValue == 32,
                  "error during deserialization (size of leafValue not equal 32)"
                )
                0
              }
              loopSize(index + 1, size + 2 + sizePrefix + +sizeLength + sizeValue)

            case NodePtr(ptrPrefix, ptr) =>
              val sizePrefix = ptrPrefix.size.toInt
              assert(sizePrefix <= 31, "error during deserialization (size of prefix more than 31)")
              val sizePtr = ptr.length.toInt
              assert(sizePtr == 32, "error during deserialization (size of ptrPrefix not equal 32)")
              loopSize(index + 1, size + 2 + sizePrefix + sizePtr)
          } else size

      val size = loopSize(0, 0)
      //put in the buffer

      val arr = new Array[Byte](size)
      @tailrec
      def loopFill(numChild: Int, idx0: Int): Array[Byte] =
        if (idx0 == size) arr
        else {
          node(numChild) match {
            case EmptyChild => loopFill(numChild + 1, idx0)

            case Leaf(prefix, value, varLength) =>
              arr(idx0) = numChild.toByte
              val idxSecondByte   = idx0 + 1
              val idxPrefixStart  = idxSecondByte + 1
              val prefixSize: Int = prefix.size.toInt
              if (varLength) {
                //01000000b | (size & 00011111b)
                arr(idxSecondByte) = (0x40 | (prefixSize & 0x1F)).toByte //second byte - type and size of prefix
                for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
                val idxValueSize = idxPrefixStart + prefixSize
                val valueSize    = value.size.toInt
                arr(idxValueSize) = valueSize.toByte
                val idxValueStart = idxValueSize + 1
                for (i <- 0 until valueSize) arr(idxValueStart + i) = value(i.toLong)
                loopFill(numChild + 1, idxValueStart + valueSize)
              } else {
                //size & 00011111b
                arr(idxSecondByte) = (prefixSize & 0x1F).toByte //second byte - type and size of prefix
                for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
                val idxValueStart = idxPrefixStart + prefixSize
                for (i <- 0 until 32) arr(idxValueStart + i) = value(i.toLong)
                loopFill(numChild + 1, idxValueStart + 32)
              }
            case NodePtr(prefix, ptr) =>
              arr(idx0) = numChild.toByte
              val idxSecondByte   = idx0 + 1
              val prefixSize: Int = prefix.size.toInt
              //10000000b | (size & 00011111b)
              arr(idxSecondByte) = (0x80 | (prefixSize & 0x1F)).toByte //second byte - type and size of prefix
              val idxPrefixStart = idxSecondByte + 1
              for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
              val idxPtrStart = idxPrefixStart + prefixSize
              for (i <- 0 until 32) arr(idxPtrStart + i) = ptr(i.toLong)
              loopFill(numChild + 1, idxPtrStart + 32)
          }
        }
      loopFill(0, 0)
    }

    def decode(arr: Array[Byte]): Node = {
      val maxSize = arr.length
      @tailrec //todo it's need?
      def loop(idx0: Int, node: Node): Node =
        if (idx0 == maxSize) node
        else {
          val (idx0Next, nodeNext) = try {
            val childNum: Int = byteToInt(arr(idx0))
            assert(
              node(childNum) == EmptyChild,
              "error during deserialization (wrong child number)"
            )
            val idx1       = idx0 + 1
            val secondByte = arr(idx1)
            assert((secondByte & 0x20) == 0x00, "error during deserialization (wrong type format)")

            val prefixSize: Int = secondByte & 0x1F
            val prefix          = new Array[Byte](prefixSize)
            val idxPrefixStart  = idx1 + 1
            for (i <- 0 until prefixSize) prefix(i) = arr(idxPrefixStart + i)

            if ((secondByte & 0x80) == 0x00) {   //Leaf
              if ((secondByte & 0x40) != 0x00) { //leafValue have variable length
                val idxleafValueSize   = idxPrefixStart + prefixSize
                val leafValueSize: Int = byteToInt(arr(idxleafValueSize))
                val leafValue          = new Array[Byte](leafValueSize)
                val idxleafValueStart  = idxleafValueSize + 1
                for (i <- 0 until leafValueSize) leafValue(i) = arr(idxleafValueStart + i)
                val idx0Next = idxleafValueStart + leafValueSize
                val nodeNext =
                  node.updated(
                    childNum,
                    Leaf(ByteVector(prefix), ByteVector(leafValue), varLength = true)
                  )
                (idx0Next, nodeNext)
              } else { //leafValue is 32 byte ptr
                val leafValue         = new Array[Byte](32)
                val idxleafValueStart = idxPrefixStart + prefixSize
                for (i <- 0 until 32) leafValue(i) = arr(idxleafValueStart + i)
                val idx0Next = idxleafValueStart + 32
                val nodeNext =
                  node.updated(
                    childNum,
                    Leaf(ByteVector(prefix), ByteVector(leafValue), varLength = false)
                  )
                (idx0Next, nodeNext)
              }
            } else { //NodePtr
              val ptr         = new Array[Byte](32)
              val idxPtrStart = idxPrefixStart + prefixSize
              for (i <- 0 until 32) ptr(i) = arr(idxPtrStart + i)
              val idx0Next = idxPtrStart + 32
              val nodeNext =
                node.updated(
                  childNum,
                  NodePtr(ByteVector(prefix), ByteVector(ptr))
                )
              (idx0Next, nodeNext)
            }
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
      store.get(Seq(nodePtr)).map(_.head.map(v => codecs.decode(v)))

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
        maybeCacheValue <- Sync[F].delay(cacheR.get(nodePtr))
        cacheValue = maybeCacheValue match {
          case None =>
            loadNodeFromStore(nodePtr).map {
              case None =>
                assert(noAssert, s"Missing node in database. ptr=${nodePtr.toHex}")
                emptyNode
              case Some(storeNode) =>
                cacheR.update(nodePtr, storeNode)
                storeNode
            }
          case Some(v) => Sync[F].pure(v)
        }
        result <- cacheValue
      } yield result

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
              case EmptyChild => Sync[F].pure(None.asRight) //Not found

              case Leaf(leafPrefix, value, _) =>
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
      * Create node from [[Child]].
      * If child is NodePtr and prefix is empty - load node
      */
    private def createNodeFromChild(child: Child): F[Node] =
      child match {
        case EmptyChild => emptyNode.pure
        case Leaf(leafPrefix, leafValue, varLength) =>
          assert(
            leafPrefix.nonEmpty,
            "Impossible to create a node. LeafPrefix should be non empty."
          )
          emptyNode
            .updated(byteToInt(leafPrefix.head), Leaf(leafPrefix.tail, leafValue, varLength))
            .pure
        case NodePtr(nodePtrPrefix, ptr) =>
          if (nodePtrPrefix.isEmpty) loadNode(ptr)
          else
            emptyNode
              .updated(byteToInt(nodePtrPrefix.head), NodePtr(nodePtrPrefix.tail, ptr))
              .pure
      }

    /**
      * Optimize and save Node, create Child from this Node
      */
    private def saveNodeAndCreateChild(
        node: Node,
        childPrefix: ByteVector,
        optimization: Boolean = true
    ): Child =
      if (optimization) {
        val nonEmptyChildren = node.iterator.filter(_ != EmptyChild).take(2).toList
        nonEmptyChildren.size match {
          case 0 => EmptyChild //all children are empty
          case 1 => //only one child is not empty - merge child and parent nodes
            val childIdx = node.indexOf(nonEmptyChildren.head)
            nonEmptyChildren.head match {
              case EmptyChild => EmptyChild
              case Leaf(leafPrefix, value, varLength) =>
                Leaf(childPrefix ++ ByteVector(childIdx) ++ leafPrefix, value, varLength)
              case NodePtr(nodePtrPrefix, ptr) =>
                NodePtr(childPrefix ++ ByteVector(childIdx) ++ nodePtrPrefix, ptr)
            }
          case 2 => //2 or more children are not empty
            NodePtr(childPrefix, saveNode(node))
        }
      } else NodePtr(childPrefix, saveNode(node))

    /**
      * Save new leaf value to this part of tree (start from curChild).
      * Rehash and save all depend node to cacheW. Return updated curChild.
      * If exist leaf with same prefix but different value - update leaf value.
      * If exist leaf with same prefix and same value - return [[None]].
      */
    def update(
        curChild: Child,
        insPrefix: ByteVector,
        insValue: ByteVector,
        varLength: Boolean
    ): F[Option[Child]] =
      curChild match {
        case EmptyChild =>
          (Leaf(insPrefix, insValue, varLength): Child).some.pure //update EmptyChild to Leaf

        case Leaf(leafPrefix, leafValue, flag) =>
          assert(leafPrefix.size == insPrefix.size, "All Radix keys should be same length")
          if (leafPrefix == insPrefix) {
            if (insValue == leafValue) none[Child].pure
            else (Leaf(insPrefix, insValue, varLength): Child).some.pure
          } //update Leaf
          else {
            // Create child node, insert existing and new leaf in this node
            // intentionally not recursive for speed up
            val (commPrefix, insPrefixRest, leafPrefixRest) = commonPrefix(insPrefix, leafPrefix)
            val newNode = emptyNode
              .updated(byteToInt(leafPrefixRest.head), Leaf(leafPrefixRest.tail, leafValue, flag))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue, varLength))
            saveNodeAndCreateChild(newNode, commPrefix, optimization = false).some.pure
          }

        case NodePtr(ptrPrefix, ptr) =>
          assert(ptrPrefix.size < insPrefix.size, "Radix key should be longer than NodePtr key")
          val (commPrefix, insPrefixRest, ptrPrefixRest) = commonPrefix(insPrefix, ptrPrefix)
          if (ptrPrefixRest.isEmpty) {
            val (childChildIdx, childInsPrefix) =
              (byteToInt(insPrefixRest.head), insPrefixRest.tail)
            //add new node to existing child node
            for {
              childNode     <- loadNode(ptr)
              childChildOpt <- update(childNode(childChildIdx), childInsPrefix, insValue, varLength) //deeper
              returnedChild = childChildOpt match {
                case None => none
                case Some(childChild) =>
                  val updatedChildNode = childNode.updated(childChildIdx, childChild)
                  saveNodeAndCreateChild(updatedChildNode, commPrefix, optimization = false).some
              }
            } yield returnedChild
          } else {
            // Create child node, insert existing Ptr and new leaf in this node
            val newNode = emptyNode
              .updated(byteToInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue, varLength))
            saveNodeAndCreateChild(newNode, commPrefix, optimization = false).some.pure
          }
      }

    /**
      * Delete leaf value from this part of tree (start from curChild).
      * Rehash and save all depend node to cacheW. Return updated curChild.
      * If not found leaf with  delPrefix - return [[None]].
      */
    def delete(curChild: Child, delPrefix: ByteVector): F[Option[Child]] =
      curChild match {
        case EmptyChild => none[Child].pure //Not found

        case Leaf(leafPrefix, _, _) =>
          if (leafPrefix == delPrefix) (EmptyChild: Child).some.pure //Happy end
          else none[Child].pure                                      //Not found

        case NodePtr(ptrPrefix, ptr) =>
          val (commPrefix, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
          if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty) none[Child].pure //Not found
          else {
            val (childChildIdx, childDelPrefix) =
              (byteToInt(delPrefixRest.head), delPrefixRest.tail)
            for {
              childNode     <- loadNode(ptr)
              childChildOpt <- delete(childNode(childChildIdx), childDelPrefix) //deeper
              returnedChild = childChildOpt match {
                case None => none
                case Some(childChild) =>
                  saveNodeAndCreateChild(childNode.updated(childChildIdx, childChild), commPrefix).some
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
      val groups = curActions.groupBy(_.key.head).toList
      val newGroupChildrenF = groups.map {
        case (groupIdx, groupActions) =>
          val index = byteToInt(groupIdx)
          val child = curNode(index)
          if (groupActions.length == 1) {
            val newChild = groupActions.head match {
              case InsertAction(key, hash) =>
                update(child, ByteVector(key).tail, hash.bytes, varLength = false)
              case DeleteAction(key) => delete(child, ByteVector(key).tail)
            }
            newChild.map((index, _))
          } else {
            val notExistInsertAction = groupActions.collectFirst { case _: InsertAction => true }.isEmpty
            val clearGroupActions =
              if (child == EmptyChild && notExistInsertAction) groupActions.collect {
                case v: InsertAction => v
              } else groupActions
            if (clearGroupActions.isEmpty) (index, none[Child]).pure
            else {
              val newActions = clearGroupActions.map {
                case InsertAction(key, hash) => InsertAction(key.tail, hash)
                case DeleteAction(key)       => DeleteAction(key.tail)
              }
              for {
                createdNode <- createNodeFromChild(curNode(index))
                newNodeOpt  <- makeActions(createdNode, newActions)
                newChild = newNodeOpt match {
                  case None          => none
                  case Some(newNode) => saveNodeAndCreateChild(newNode, ByteVector.empty).some
                }
              } yield (index, newChild)
            }
          }
      }
      for {
        newGroupChildren <- newGroupChildrenF.parSequence
        newCurNode = newGroupChildren.foldLeft(curNode) {
          case (tempNode, (index, newChildOpt)) =>
            newChildOpt.map(tempNode.updated(index, _)).getOrElse(tempNode)
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
        case (EmptyChild, _) => Unit

        case (Leaf(leafPrefix, value, _), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${leafPrefix.toHex}: ${value.toHex}")

        case (NodePtr(ptrPrefix, ptr), idx) =>
          prn(s"${idx.toHexString.toUpperCase}${ptrPrefix.toHex} [${ptr.toHex}]")
          loadNode(ptr).map(print(_, indentLevel + 1))
      }
    }
  }
}
