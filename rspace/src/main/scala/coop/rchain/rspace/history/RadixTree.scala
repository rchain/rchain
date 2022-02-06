package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import cats.{Monad, Parallel}
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueTypedStore
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

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
    private val defSize  = 32
    private val headSize = 2 //2 bytes: first - item number, second - second byte

    def encode(node: Node): ByteVector = {
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
              loopSize(index + 1, size + headSize + sizePrefix + defSize)

            case NodePtr(ptrPrefix, ptr) =>
              val sizePrefix = ptrPrefix.size.toInt
              assert(sizePrefix <= 127, "error during serialization (size of prefix more than 127)")
              assert(
                ptr.size == defSize,
                "error during serialization (size of ptrPrefix not equal 32)"
              )
              loopSize(index + 1, size + headSize + sizePrefix + defSize)
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
      ByteVector(loopFill(0, 0))
    }

    def decode(bv: ByteVector): Node = {
      val arr     = bv.toArray
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
  def hashNode(node: Node): (ByteVector, ByteVector) = {
    import coop.rchain.crypto.hash.Blake2b256
    val bytes = codecs.encode(node)
    (ByteVector(Blake2b256.hash(bytes)), bytes)
  }

  def byteToInt(b: Byte): Int = b & 0xff

  /**
    * Which element counts the skip&take counter?
    */
  sealed trait CountableItem
  final case object calculateNodes extends CountableItem
  final case object calculateLeafs extends CountableItem

  /**
    * Data for return
    */
  final case class ExportData(
      nP: Seq[ByteVector], //nodePrefixes
      nK: Seq[ByteVector], //nodeKVDBKeys
      nV: Seq[ByteVector], //nodeKVDBValues
      lP: Seq[ByteVector], //leafPrefixes
      lV: Seq[ByteVector]  //leafValues (it's ptr for datastore)
  )

  /**
    * Which data is need to export?
    */
  final case class ExportDataSettings(
      expNP: Boolean,
      expNK: Boolean,
      expNV: Boolean,
      expLP: Boolean,
      expLV: Boolean
  )

  /**
    * Sequential export algorithm
    * returns the data and prefix of the last processed item.
    * If all bonds in the tree are processed, returns None as prefix
    */
  def sequentialExport[F[_]: Monad](
      rootHash: ByteVector,
      lastPrefix: Option[ByteVector], //describes the path of root to last processed element (if None - start from root)
      skipSize: Int,                  //how many elements to skip
      takeSize: Int,                  //how many elements to take
      getNodeDataFromStore: ByteVector => F[Option[ByteVector]],
      settings: ExportDataSettings,
      countableItem: CountableItem = calculateNodes //don't used (will be use in the future)
  ): F[(ExportData, Option[ByteVector])] = {
    final case class NodeData(
        prefix: ByteVector,         //a prefix that describes the path of root to node
        decoded: Node,              //deserialized data (from parsing)
        lastItemIndex: Option[Byte] //last processed item number
    )
    type Path = Vector[NodeData] //sequence used in recursions

    /**
      * Create path from root to lastPrefix node
      */
    final case class NodePathData(
        nodeHash: ByteVector,   //hash of node for load
        nodePrefix: ByteVector, //prefix of this node
        restPrefix: ByteVector, //a prefix that describes the rest of the Path
        path: Path              //return path
    )
    def initNodePath(
        params: NodePathData
    ): F[Either[NodePathData, Path]] =
      params match {
        case NodePathData(hash, nodePrefix, tempPrefix, path) =>
          for {
            nodeOpt <- getNodeDataFromStore(hash)
            decoded = {
              assert(nodeOpt.isDefined, s"Export error: Node with key ${hash.toHex} not found")
              val node = nodeOpt.get
              codecs.decode(node)
            }
            r = if (tempPrefix.isEmpty)
              (NodeData(nodePrefix, decoded, none) +: path).asRight //happy end
            else                                                    //go dipper
              decoded(byteToInt(tempPrefix.head)) match {
                case NodePtr(ptrPrefix, ptr) =>
                  val (prefixCommon, prefixRest, ptrPrefixRest) =
                    commonPrefix(tempPrefix.tail, ptrPrefix)
                  assert(
                    ptrPrefixRest.isEmpty,
                    s"Export error: Node with prefix ${(nodePrefix ++ tempPrefix).toHex} not found"
                  )
                  NodePathData(
                    ptr,
                    (nodePrefix :+ tempPrefix.head) ++ prefixCommon,
                    prefixRest,
                    NodeData(nodePrefix, decoded, tempPrefix.head.some) +: path
                  ).asLeft
                case _ =>
                  assert(
                    assertion = false,
                    s"Export error: Node with prefix ${(nodePrefix ++ tempPrefix).toHex} not found"
                  )
                  Vector().asRight //Not found
              }
          } yield r
      }

    /**
      * Main loop
      */
    type LoopData = (
        Path,       // path of node from current to root
        (Int, Int), // Skip & take counter
        ExportData  // Result of export
    )
    def loop(params: LoopData): F[Either[LoopData, (ExportData, Option[ByteVector])]] = {
      val (path, (skip, take), expData) = params
      if (path.isEmpty) Monad[F].pure((expData, none).asRight) //end of Tree
      else {
        val curNodeData   = path.head
        val curNodePrefix = curNodeData.prefix
        val curNode       = curNodeData.decoded

        if ((skip, take) == (0, 0))
          Monad[F].pure((expData, curNodePrefix.some).asRight) //end of skip&take counter
        else {
          @tailrec
          def findNextNotEmptyItem(lastIndexOpt: Option[Byte]): Option[(Byte, Item)] = {
            def incIndex(index: Byte) =
              if (index == 0xFF.toByte) none
              else (byteToInt(index) + 1).toByte.some
            val curIndexOpt = lastIndexOpt.map(incIndex).getOrElse(0.toByte.some)

            curIndexOpt match {
              case None => none
              case Some(curIndex) =>
                val curItem = curNode(byteToInt(curIndex))
                curItem match {
                  case EmptyItem => findNextNotEmptyItem(curIndex.some)
                  case Leaf(_, _) =>
                    if (settings.expLP || settings.expLV)
                      (curIndex, curItem).some
                    else findNextNotEmptyItem(curIndex.some)
                  case NodePtr(_, _) => (curIndex, curItem).some
                }
            }
          }

          val nextNotEmptyItem = findNextNotEmptyItem(curNodeData.lastItemIndex)
          nextNotEmptyItem match {
            case None => Monad[F].pure((path.tail, (skip, take), expData).asLeft)
            case Some((itemIndex, item)) =>
              val newCurNodeData = NodeData(curNodePrefix, curNode, itemIndex.some)
              val newPath        = newCurNodeData +: path.tail
              item match {
                case EmptyItem => Monad[F].pure((newPath, (skip, take), expData).asLeft)
                case Leaf(leafPrefix, leafValue) =>
                  if (skip > 0) Monad[F].pure((newPath, (skip, take), expData).asLeft)
                  else {
                    val newLP =
                      if (settings.expLP) {
                        val newSingleLP = (curNodePrefix :+ itemIndex) ++ leafPrefix
                        expData.lP :+ newSingleLP
                      } else Vector()
                    val newLV =
                      if (settings.expLV) expData.lV :+ leafValue else Vector()
                    val newExportData = ExportData(
                      nP = expData.nP,
                      nK = expData.nK,
                      nV = expData.nV,
                      lP = newLP,
                      lV = newLV
                    )
                    Monad[F].pure((newPath, (skip, take), newExportData).asLeft)
                  }

                case NodePtr(ptrPrefix, ptr) =>
                  for {
                    childNodeOpt <- getNodeDataFromStore(ptr)
                    childNV = {
                      assert(
                        childNodeOpt.isDefined,
                        s"Export error: Node with key ${ptr.toHex} not found"
                      )
                      childNodeOpt.get
                    }
                    childDecoded  = codecs.decode(childNV)
                    childNP       = (curNodePrefix :+ itemIndex) ++ ptrPrefix
                    childNodeData = NodeData(childNP, childDecoded, none)
                    childPath     = childNodeData +: newPath

                    r = if (skip > 0) (childPath, (skip - 1, take), expData).asLeft
                    else {
                      val newNP = if (settings.expNP) expData.nP :+ childNP else Vector()
                      val newNK = if (settings.expNK) expData.nK :+ ptr else Vector()
                      val newNV = if (settings.expNV) expData.nV :+ childNV else Vector()
                      val newData = ExportData(
                        nP = newNP,
                        nK = newNK,
                        nV = newNV,
                        lP = expData.lP,
                        lV = expData.lV
                      )
                      (childPath, (skip, take - 1), newData).asLeft
                    }
                  } yield r
              }
          }
        }
      }
    }

    val rootParams =
      NodePathData(rootHash, ByteVector.empty, lastPrefix.getOrElse(ByteVector.empty), Vector())
    val emptyExportData  = ExportData(Vector(), Vector(), Vector(), Vector(), Vector())
    val emptyExportDataF = emptyExportData.pure

    assert(
      (skipSize, takeSize) != (0, 0),
      s"Export error: invalid initial conditions (skipSize, takeSize)==(0,0)"
    )

    val noRootStart  = (emptyExportDataF, skipSize, takeSize)     //start from next node after lastPrefix
    val skippedStart = (emptyExportDataF, skipSize - 1, takeSize) //skipped node start
    val rootExportData = for {
      rootOpt <- getNodeDataFromStore(rootHash)
      root = {
        assert(
          rootOpt.isDefined,
          s"Export error: root node with key ${rootHash.toHex} not found"
        )
        rootOpt.get
      }
      newNP = if (settings.expNP) Vector(ByteVector.empty) else Vector()
      newNK = if (settings.expNK) Vector(rootHash) else Vector()
      newNV = if (settings.expNV) Vector(root) else Vector()
    } yield ExportData(newNP, newNK, newNV, Vector(), Vector())
    val rootStart = (rootExportData, skipSize, takeSize - 1) //take root

    //defining init data
    val (initExportDataF, initSkipSize, initTakeSize) = lastPrefix
      .map(_ => noRootStart)
      .getOrElse {
        if (skipSize > 0) skippedStart
        else rootStart
      }

    val doExport = for {
      path                  <- rootParams.tailRecM(initNodePath)
      initExportData        <- initExportDataF
      startParams: LoopData = (path, (initSkipSize, initTakeSize), initExportData)
      r                     <- startParams.tailRecM(loop)
    } yield r
    val emptyResult = Monad[F].pure((emptyExportData, none))

    for {
      rootNodeOpt <- getNodeDataFromStore(rootHash)
      r <- if (rootNodeOpt.isDefined) doExport
          else emptyResult
    } yield r
  }

  class RadixTreeImpl[F[_]: Sync: Parallel](store: KeyValueTypedStore[F, ByteVector, ByteVector]) {

    /**
      * Load and decode serializing data from KVDB.
      */
    private def loadNodeFromStore(nodePtr: ByteVector): F[Option[Node]] =
      store.get1(nodePtr).map(_.map(codecs.decode))

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
    def loadNode(nodePtr: ByteVector, noAssert: Boolean = false): F[Node] = {
      def errorMsg(): Unit = assert(noAssert, s"Missing node in database. ptr=${nodePtr.toHex}")
      val cacheMiss = for {
        storeNodeOpt <- loadNodeFromStore(nodePtr)
        _            = storeNodeOpt.map(cacheR.update(nodePtr, _)).getOrElse(errorMsg())
      } yield storeNodeOpt.getOrElse(emptyNode)
      for {
        cacheNodeOpt <- Sync[F].delay(cacheR.get(nodePtr))
        res          <- cacheNodeOpt.map(_.pure).getOrElse(cacheMiss)
      } yield res
    }

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
    private val cacheW: TrieMap[ByteVector, ByteVector] = TrieMap.empty

    /**
      * Serializing and hashing one [[Node]].
      * Serializing data load in [[cacheW]].
      * If detected collision with older cache data - executing assert
      */
    def saveNode(node: Node): ByteVector = {
      val (hash, bytes) = hashNode(node)
      // collision alarm
      def generateErrorMsg(v: Node): Unit = assert(
        v == node,
        s"collision in cache (record with key = ${hash.toHex} has already existed)"
      )
      cacheR.get(hash).map(generateErrorMsg).getOrElse(cacheR.update(hash, node))
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
        kvvExist          = kvExist zip valueExistInStore.map(_.getOrElse(ByteVector.empty))
        kvCollision       = kvvExist.filter(kvv => !(kvv._1._2 == kvv._2)).map(_._1)
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
      def loop(params: Params): F[Either[Params, Option[ByteVector]]] =
        params match {
          case (_, ByteVector.empty) => Sync[F].pure(none.asRight) //Not found
          case (curNode, prefix) =>
            curNode(byteToInt(prefix.head)) match {
              case EmptyItem => Sync[F].pure(none.asRight) //Not found

              case Leaf(leafPrefix, value) =>
                if (leafPrefix == prefix.tail) Sync[F].pure(value.some.asRight) //Happy end
                else Sync[F].pure(none.asRight)                                 //Not found

              case NodePtr(ptrPrefix, ptr) =>
                val (_, prefixRest, ptrPrefixRest) = commonPrefix(prefix.tail, ptrPrefix)
                if (ptrPrefixRest.isEmpty) loadNode(ptr).map(n => (n, prefixRest).asLeft) //Deeper
                else Sync[F].pure(none.asRight)                                           //Not found
            }
        }
      Sync[F].tailRecM(startNode, startPrefix)(loop)
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
        compaction: Boolean = true
    ): Item =
      if (compaction) {
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
            saveNodeAndCreateItem(newNode, commPrefix, compaction = false).some.pure
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
              returnedItem = childItemOpt.map { childItem =>
                val updatedChildNode = childNode.updated(childItemIdx, childItem)
                saveNodeAndCreateItem(updatedChildNode, commPrefix, compaction = false)
              }
            } yield returnedItem
          } else {
            // Create child node, insert existing Ptr and new leaf in this node
            val newNode = emptyNode
              .updated(byteToInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            saveNodeAndCreateItem(newNode, commPrefix, compaction = false).some.pure
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
              returnedChild = childItemOpt.map { childItem =>
                saveNodeAndCreateItem(childNode.updated(childItemIdx, childItem), commPrefix)
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
      val groups = curActions
        .groupBy( //fixme
          _.key.headOption.getOrElse({
            assert(assertion = false, "The length of all prefixes in the subtree must be the same")
            0x00.toByte
          })
        )
        .toList
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
                newItem     = newNodeOpt.map(saveNodeAndCreateItem(_, ByteVector.empty))
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
