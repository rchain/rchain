package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import java.nio.ByteBuffer

/**
  * Radix Tree implementation
  */
object RadixTree9 {

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
    */
  object codecs {

    def encode(node: Node): ByteBuffer = {
      //calculating size of buffer
      var size: Int = 0
      node foreach {
        case EmptyChild => Unit

        case Leaf(leafPrefix, value, varLength) =>
          size += 2 // for first and second bytes
          val sizePrefix = leafPrefix.size.toInt
          assert(sizePrefix <= 31, "error during deserialization (size of prefix more than 31)")
          size += sizePrefix
          val sizeValue = value.size.toInt
          if (varLength) {
            assert(value.size <= 255, "error during deserialization (size of value more than 255)")
            size += 1
          } else
            assert(sizeValue == 32, "error during deserialization (size of leafValue not equal 32)")
          size += sizeValue

        case NodePtr(ptrPrefix, ptr) =>
          size += 2 // for first and second bytes
          val sizePrefix = ptrPrefix.size.toInt
          assert(sizePrefix <= 31, "error during deserialization (size of prefix more than 31)")
          size += sizePrefix
          val sizePtr = ptr.size.toInt
          assert(sizePtr == 32, "error during deserialization (size of ptrPrefix not equal 32)")
          size += sizePtr
      }

      //put in the buffer
      val buf = ByteBuffer.allocate(size) //todo DIRECT???
      node.zipWithIndex foreach {
        case (EmptyChild, _) => Unit

        case (Leaf(leafPrefix, value, varLength), idx) =>
          buf.put(idx.toByte) //first byte - number of children

          if (varLength) {
            //size & 00011111b
            buf.put((leafPrefix.size & 0x1F).toByte) //second byte - type and size of prefix
            buf.put(leafPrefix.toArray)              //prefix sequence
            buf.put(value.size.toByte)               //size of value
          } else {
            //01000000b | (size & 00011111b)
            buf.put((0x40 | (leafPrefix.size & 0x1F)).toByte) //second byte - type and size of prefix
            buf.put(leafPrefix.toArray)                       //prefix sequence
          }
          buf.put(value.toArray) //value sequence

        case (NodePtr(ptrPrefix, ptr), idx) =>
          buf.put(idx.toByte) //first byte - number of children
          //10000000b | (size & 00011111b)
          buf.put((0x80 | (ptrPrefix.size & 0x1F)).toByte) //second byte - type and size of prefix
          buf.put(ptrPrefix.toArray)                       //prefix sequence
          buf.put(ptr.toArray)                             //size of value
      }
      buf.rewind
    }

    def decode(buf: ByteBuffer): Node = {
      import scala.collection.mutable.ListBuffer
      buf.rewind
      val node = new ListBuffer[Child]()
      while (buf.position() < buf.capacity()) {
        try {
          val notEmptyChildNum: Int = byteToInt(buf.get())
          assert(notEmptyChildNum >= node.size, "error during deserialization (wrong child number)")

          for (_ <- 0 until notEmptyChildNum - node.size) node += EmptyChild

          val secondByte = buf.get()
          assert((secondByte & 0x20) == 0x00, "error during deserialization (wrong type format)")

          val prefixSize: Int = secondByte & 0x1F
          val prefix          = new Array[Byte](prefixSize)
          buf.get(prefix)
          if ((secondByte & 0x80) == 0x00) {   //Leaf
            if ((secondByte & 0x40) == 0x00) { //leafValue have variable length
              val leafValueSize: Int = buf.get().toInt
              val leafValue          = new Array[Byte](leafValueSize)
              buf.get(leafValue)
              node += Leaf(ByteVector(prefix), ByteVector(leafValue), varLength = false)
            } else { //leafValue is 32 byte ptr
              val leafValue = new Array[Byte](32)
              buf.get(leafValue)
              node += Leaf(ByteVector(prefix), ByteVector(leafValue), varLength = true)
            }
          } else { //NodePtr
            val ptr = new Array[Byte](32)
            buf.get(ptr)
            node += NodePtr(ByteVector(prefix), ByteVector(ptr))
          }
        } catch {
          case _: Exception =>
            assert(assertion = false, "error during deserialization (out of range)")
        }
      }
      assert(
        node.size <= 256,
        "error during deserialization (the number of children more than 256)"
      )
      for (_ <- 0 until 256 - node.size) node += EmptyChild
      buf.rewind
      node.toVector
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
  def hashNode(node: Node): (Blake2b256Hash, ByteBuffer) = {

    val buf = codecs.encode(node)
    val arr = new Array[Byte](buf.remaining())
    buf.get(arr)
    buf.rewind
    (Blake2b256Hash.create(arr), buf)

//    import coop.rchain.crypto.hash.Blake2b256
//    buf.get(arr)
//    (Blake2b256.hash(arr), buf)
  }

  def byteToInt(b: Byte): Int = b & 0xff

  class RadixTreeImpl9[F[_]: Sync: Parallel](store: RadixStore2[F]) {

    /**
      * Load and decode serializing data from KVDB.
      */
    private def loadNodeFromStore(nodePtr: ByteVector): F[Option[Node]] =
      store.get(Seq(nodePtr)).map(_.head.map(v => codecs.decode(v)))

    /**
      * Cache for storing readed and decoded nodes.
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
                assert(noAssert, s"Missing node in database. ptr=$nodePtr")
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
      * Clear [[cacheR]] (cache for storing readed nodes).
      */
    def clearCacheR(): Unit = cacheR.clear()

    /**
      * Cache for storing serializing nodes. For subsequent unloading in KVDB
      * In cache store kv-pair (hash, bytes).
      * Where hash -  Blake2b256Hash of bytes,
      *       bytes - serializing data of nodes.
      */
    private val cacheW: TrieMap[ByteVector, ByteBuffer] = TrieMap.empty

    /**
      * Serializing and hashing one [[Node]].
      * Serializing data load in [[cacheW]].
      * If detected collision with older cache data - executing assert
      */
    def saveNode(node: Node): Blake2b256Hash = {
      val (hash, bytes) = hashNode(node)
      cacheR.get(hash.bytes) match { // collision alarm
        case Some(v) =>
          assert(
            v == node,
            s"collision in cache (record with key = ${hash.bytes} has already existed)"
          )
        case None => cacheR.update(hash.bytes, node)
      }
      cacheW.update(hash.bytes, bytes)
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
        kvvExist          = kvExist zip valueExistInStore.map(_.getOrElse(ByteBuffer.allocate(0)))
        kvCollision       = kvvExist.filter(kvv => kvv._1._2.compareTo(kvv._2) != 0).map(_._1)
        kvAbsent = if (kvCollision.isEmpty) kvIfAbsent.filterNot(_._2).map(_._1)
        else {
          assert(
            assertion = false,
            s"${kvCollision.length} collisions in KVDB (first collision with key = ${kvCollision.head._1})"
          )
          List.empty
        }
        _ <- store.put(kvAbsent)
      } yield ()
    }

    /**
      * Clear [[cacheW]] (cache for storing data to write in KVDB).
      */
    def clearPutCache(): Unit = cacheW.clear()

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
                if (ptrPrefixRest.isEmpty) loadNode(ptr).map(n => (n, prefixRest).asLeft) //Deepper
                else Sync[F].pure(None.asRight)                                           //Not found
            }
        }
      Sync[F].tailRecM(startNode, startPrefix)(go)
    }

    /**
      * Create node from [[Child]].
      * If child is NodePtr and prefix is empty - load node
      */
    def createNodeFromChild(child: Child): F[Node] =
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
    def saveNodeAndCreateChild(
        node: Node,
        childPrefix: ByteVector,
        optimization: Boolean = true
    ): Child =
      if (optimization) {
        val nonEmptyChilds = node.iterator.filter(_ != EmptyChild).take(2).toList
        nonEmptyChilds.size match {
          case 0 => EmptyChild //all children are empty
          case 1 => //only one child is not empty - merge child and parent nodes
            val childIdx = node.indexOf(nonEmptyChilds.head)
            nonEmptyChilds.head match {
              case EmptyChild => EmptyChild
              case Leaf(leafPrefix, value, varLength) =>
                Leaf(childPrefix ++ ByteVector(childIdx) ++ leafPrefix, value, varLength)
              case NodePtr(nodePtrPrefix, ptr) =>
                NodePtr(childPrefix ++ ByteVector(childIdx) ++ nodePtrPrefix, ptr)
            }
          case 2 => //2 or more children are not empty
            NodePtr(childPrefix, saveNode(node).bytes)
        }
      } else NodePtr(childPrefix, saveNode(node).bytes)

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
