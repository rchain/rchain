package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueTypedStore
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object RadixTree {

  /**
    * Described items which contain in [[Node]].
    */
  sealed trait Item

  /**
    * Empty item
    */
  final case object EmptyItem extends Item

  /**
    * Item which contain data.
    */
  final case class Leaf(prefix: ByteVector, value: ByteVector) extends Item

  /**
    * Item which contain pointer for child [[Node]]
    */
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector) extends Item

  /**
    * Base type for nodes in Radix History.
    *
    * Always contains 256 [[Item]]
    */
  type Node = Vector[Item]

  /**
    * Number of items always constant.
    */
  val numItems = 256

  /**
    * Empty node consists only of [[EmptyItem]]'s.
    */
  val emptyNode: Node = (0 until numItems).map(_ => EmptyItem).toVector

  /**
    * Binary codecs for serializing/deserializing Node in Radix tree
    *
    * {{{
    * Coding structure for items:
    *   EmptyItem                   - Empty (not encode)

    *   Leaf(prefix,value)    -> [item number] [second byte] [prefix0]..[prefixM] [value0]..[value31]
    *                               where is: [second byte] -> bit7 = 0 (Leaf identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    *
    *   NodePtr(prefix,ptr)   -> [item number] [second byte] [prefix0]..[prefixM] [ptr0]..[ptr31]
    *                               where is: [second byte] -> bit7 = 1 (NodePtr identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    *
    * For example encode this Node which contains 2 non-empty items (number 1 and number 2):
    * (0)[Empty] (1)[Leaf(prefix:0xFFFF,value:0x00..0001)] (2)[NodePtr(prefix:empty,value:0xFF..FFFF)] (3)...(255)[Empty].
    * Encoded data = 0x0102FFFF0000000000000000000000000000000000000000000000000000000000000001
    *                  0280FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    * where: item 1 (number_secondByte_prefix_value) = 01_02_FFFF_00..0001
    *        item 2 (number_secondByte_prefix_value) = 02_80_empty_FF..FFFF
    * }}}
    */
  object Codecs {
    private val defSize  = 32 // Default size for non-empty item data
    private val headSize = 2  // 2 bytes: first - item number, second - second byte

    /** Serialization [[Node]] to [[ByteVector]]
      */
    def encode(node: Node): ByteVector = {

      // Calculating size of serialized data
      val sizeNode = node.size
      @tailrec
      def loopSize(index: Int, size: Int): Int =
        if (sizeNode > index)
          node(index) match {
            case EmptyItem => loopSize(index + 1, size)

            case Leaf(leafPrefix, value) =>
              val sizePrefix = leafPrefix.size.toInt
              assert(sizePrefix <= 127, "Error during serialization: size of prefix more than 127.")
              assert(
                value.size == defSize,
                "Error during serialization: size of leafValue not equal 32."
              )
              loopSize(index + 1, size + headSize + sizePrefix + defSize)

            case NodePtr(ptrPrefix, ptr) =>
              val sizePrefix = ptrPrefix.size.toInt
              assert(sizePrefix <= 127, "Error during serialization: size of prefix more than 127.")
              assert(
                ptr.size == defSize,
                "Error during serialization: size of ptrPrefix not equal 32."
              )
              loopSize(index + 1, size + headSize + sizePrefix + defSize)
          } else size

      val size = loopSize(0, 0) // Calculating size

      val arr = new Array[Byte](size) //Allocation memory

      // Serialization (fill allocated memory)
      @tailrec
      def loopFill(numItem: Int, idx0: Int): Array[Byte] =
        if (idx0 == size) arr // Happy end (return serializing data).
        else {
          node(numItem) match {
            // If current item is empty - just skip serialization of this item
            case EmptyItem => loopFill(numItem + 1, idx0) // Loop to the next item.

            case Leaf(prefix, value) =>
              // Fill first byte - item number
              arr(idx0) = numItem.toByte
              // Fill second byte - Leaf identifier (most significant bit = 0) and prefixSize (lower 7 bits = size)
              val idxSecondByte   = idx0 + 1
              val prefixSize: Int = prefix.size.toInt
              arr(idxSecondByte) = (prefixSize & 0x7F).toByte // (size & 01111111b)
              // Fill prefix
              val idxPrefixStart = idxSecondByte + 1
              for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
              // Fill leafValue
              val idxValueStart = idxPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(idxValueStart + i) = value(i.toLong)
              loopFill(numItem + 1, idxValueStart + defSize) // Loop to the next item.

            case NodePtr(prefix, ptr) =>
              // Fill first byte - item number
              arr(idx0) = numItem.toByte
              // Fill second byte - NodePtr identifier (most significant bit = 1) and prefixSize (lower 7 bits = size)
              val idxSecondByte   = idx0 + 1
              val prefixSize: Int = prefix.size.toInt
              arr(idxSecondByte) = (0x80 | (prefixSize & 0x7F)).toByte // 10000000b | (size & 01111111b)
              // Fill prefix
              val idxPrefixStart = idxSecondByte + 1
              for (i <- 0 until prefixSize) arr(idxPrefixStart + i) = prefix(i.toLong)
              // Fill ptr
              val idxPtrStart = idxPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(idxPtrStart + i) = ptr(i.toLong)
              loopFill(numItem + 1, idxPtrStart + defSize) // Loop to the next item.
          }
        }
      ByteVector(loopFill(0, 0))
    }

    /** Deserialization [[ByteVector]] to [[Node]]
      */
    def decode(bv: ByteVector): Node = {
      val arr     = bv.toArray
      val maxSize = arr.length
      @tailrec
      // Each loop decodes one non-empty item.
      def loop(idx0: Int, node: Node): Node =
        if (idx0 == maxSize) node // End of deserialization
        else {
          val (idx0Next, nodeNext) = try {
            val numItem: Int = byteToInt(arr(idx0)) // Take first byte - it's item's number
            assert(
              node(numItem) == EmptyItem,
              "Error during deserialization: wrong number of item."
            )
            val idx1       = idx0 + 1
            val secondByte = arr(idx1) // Take second byte

            // Decoding prefix
            val prefixSize: Int = secondByte & 0x7F // Lower 7 bits - it's size of prefix (0..127).
            val prefix          = new Array[Byte](prefixSize)
            val idxPrefixStart  = idx1 + 1
            for (i <- 0 until prefixSize) prefix(i) = arr(idxPrefixStart + i) // Take prefix

            // Decoding leaf or nodePtr data
            val valOrPtr         = new Array[Byte](defSize)
            val idxValOrPtrStart = idxPrefixStart + prefixSize
            for (i <- 0 until defSize)
              valOrPtr(i) = arr(idxValOrPtrStart + i) // Take next 32 bytes - it's data

            val idx0Next = idxValOrPtrStart + defSize // Calculating start position for next loop

            // Decoding type of non-empty item
            val item = if ((secondByte & 0x80) == 0x00) { // If first bit 0 - decoding as Leaf
              Leaf(ByteVector(prefix), ByteVector(valOrPtr))
            } else {
              NodePtr(ByteVector(prefix), ByteVector(valOrPtr)) // If first bit 1 - decoding as NodePtr.
            }

            (idx0Next, node.updated(numItem, item))
          } catch {
            case _: Exception =>
              assert(assertion = false, "Error during deserialization: out of range")
              (maxSize, emptyNode)
          }
          loop(idx0Next, nodeNext) // Try to decode next item.
        }

      loop(0, emptyNode)
    }
  }

  /**
    * Find the common part of b1 and b2.
    *
    * @return (Common part , rest of b1, rest of b2).
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
    *
    * @return Blake2b256 hash of input data.
    */
  def hashNode(node: Node): (ByteVector, ByteVector) = {
    import coop.rchain.crypto.hash.Blake2b256
    val bytes = Codecs.encode(node)
    (ByteVector(Blake2b256.hash(bytes)), bytes)
  }

  def byteToInt(b: Byte): Int = b & 0xff

  /**
    * Data returned after export
    *
    * @param nPrefixes Node prefixes
    * @param nKeys Node KVDB keys
    * @param nValues Node KVDB values
    * @param lPrefixes Leaf prefixes
    * @param lValues Leaf values (it's pointer for data in datastore)
    */
  final case class ExportData(
      nPrefixes: Seq[ByteVector],
      nKeys: Seq[ByteVector],
      nValues: Seq[ByteVector],
      lPrefixes: Seq[ByteVector],
      lValues: Seq[ByteVector]
  )

  /**
    * Settings for [[ExportData]]
    *
    * If false - data will not be exported.
    */
  final case class ExportDataSettings(
      flagNPrefixes: Boolean,
      flagNKeys: Boolean,
      flagNValues: Boolean,
      flagLPrefixes: Boolean,
      flagLValues: Boolean
  )

  /**
    * Sequential export algorithm
    *
    * @param rootHash Root node hash, starting point
    * @param lastPrefix Describes the path of root to last processed element (if None - start from root)
    * @param skipSize Describes how many elements to skip
    * @param takeSize Describes how many elements to take
    * @param getNodeDataFromStore Function to getng data from storage
    * @param settings [[ExportDataSettings]]
    *
    * @return
    * Return the data and prefix of the last processed item.
    * If all bonds in the tree are processed, returns None as prefix.
    * {{{
    * prefix - Prefix that describes the path of root to node
    * decoded - Deserialized data (from parsing)
    * lastItemIndex - Last processed item number
    * }}}
    */
  def sequentialExport[F[_]: Sync](
      rootHash: ByteVector,
      lastPrefix: Option[ByteVector],
      skipSize: Int,
      takeSize: Int,
      getNodeDataFromStore: ByteVector => F[Option[ByteVector]],
      settings: ExportDataSettings
  ): F[(ExportData, Option[ByteVector])] = {
    final case class NodeData(
        prefix: ByteVector,
        decoded: Node,
        lastItemIndex: Option[Byte]
    )
    type Path = Vector[NodeData] // Sequence used in recursions

    final case class NodePathData(
        nodeHash: ByteVector,   // Hash of node for load
        nodePrefix: ByteVector, // Prefix of this node
        restPrefix: ByteVector, // Prefix that describes the rest of the Path
        path: Path              // Return path
    )

    /**
      * Create path from root to lastPrefix node
      */
    def initNodePath(
        params: NodePathData
    ): F[Either[NodePathData, Path]] =
      params match {
        case NodePathData(hash, nodePrefix, tempPrefix, path) =>
          for {
            nodeOpt <- getNodeDataFromStore(hash)
            node <- nodeOpt.liftTo[F](
                     new Exception(s"Export error: node with key ${hash.toHex} not found.")
                   )
            decodedNode = Codecs.decode(node)
            r = if (tempPrefix.isEmpty)
              (NodeData(nodePrefix, decodedNode, none) +: path).asRight // Happy end
            else                                                        // Go dipper
              decodedNode(byteToInt(tempPrefix.head)) match {
                case NodePtr(ptrPrefix, ptr) =>
                  val (prefixCommon, prefixRest, ptrPrefixRest) =
                    commonPrefix(tempPrefix.tail, ptrPrefix)
                  assert(
                    ptrPrefixRest.isEmpty,
                    s"Export error: node with prefix ${(nodePrefix ++ tempPrefix).toHex} not found."
                  )
                  NodePathData(
                    ptr,
                    (nodePrefix :+ tempPrefix.head) ++ prefixCommon,
                    prefixRest,
                    NodeData(nodePrefix, decodedNode, tempPrefix.head.some) +: path
                  ).asLeft
                case _ =>
                  assert(
                    assertion = false,
                    s"Export error: node with prefix ${(nodePrefix ++ tempPrefix).toHex} not found."
                  )
                  Vector().asRight // Not found
              }
          } yield r
      }

    type LoopData = (
        Path,       // Path of node from current to root
        (Int, Int), // Skip & take counter
        ExportData  // Result of export
    )

    /**
      * Find next non-empty item.
      *
      * @param node Node to look for
      * @param lastIdxOpt Last found index (if this node was not searched - [[None]])
      * @return [[Some]](numItem, [[Item]]) if item found, [[None]] if non-empty item not found
      */
    @tailrec
    def findNextNonEmptyItem(node: Node, lastIdxOpt: Option[Byte]): Option[(Byte, Item)] =
      if (lastIdxOpt == 0xFF.toByte.some) none
      else {
        val curIdxInt = lastIdxOpt.map(byteToInt(_) + 1).getOrElse(0)
        val curItem   = node(curIdxInt)
        val curIdx    = curIdxInt.toByte
        curItem match {
          case EmptyItem => findNextNonEmptyItem(node, curIdx.some)
          case Leaf(_, _) =>
            if (settings.flagLPrefixes || settings.flagLValues) (curIdx, curItem).some
            else findNextNonEmptyItem(node, curIdx.some)
          case NodePtr(_, _) => (curIdx, curItem).some
        }
      }

    /**
      * Export one [[Node]] and recursively move to the next. If the Skip counter or take counter is more than 1
      */
    def nodeExport(params: LoopData): F[Either[LoopData, (ExportData, Option[ByteVector])]] = {
      val (path, (skip, take), expData) = params
      if (path.isEmpty) // End of Tree
        (expData, Option.empty[ByteVector]).asRight[LoopData].pure
      else {
        val curNodeData   = path.head
        val curNodePrefix = curNodeData.prefix
        val curNode       = curNodeData.decoded

        if ((skip, take) == (0, 0))
          (expData, curNodePrefix.some).asRight[LoopData].pure // End of skip&take counter
        else {
          val nextNotEmptyItem = findNextNonEmptyItem(curNode, curNodeData.lastItemIndex)
          nextNotEmptyItem match {
            case None =>
              (path.tail, (skip, take), expData).asLeft[(ExportData, Option[ByteVector])].pure
            case Some((itemIndex, item)) =>
              val newCurNodeData = NodeData(curNodePrefix, curNode, itemIndex.some)
              val newPath        = newCurNodeData +: path.tail
              item match {
                case EmptyItem =>
                  (newPath, (skip, take), expData).asLeft[(ExportData, Option[ByteVector])].pure
                case Leaf(leafPrefix, leafValue) =>
                  if (skip > 0)
                    (newPath, (skip, take), expData).asLeft[(ExportData, Option[ByteVector])].pure
                  else {
                    val newLP =
                      if (settings.flagLPrefixes) {
                        val newSingleLP = (curNodePrefix :+ itemIndex) ++ leafPrefix
                        expData.lPrefixes :+ newSingleLP
                      } else Vector()
                    val newLV =
                      if (settings.flagLValues) expData.lValues :+ leafValue else Vector()
                    val newExportData = ExportData(
                      nPrefixes = expData.nPrefixes,
                      nKeys = expData.nKeys,
                      nValues = expData.nValues,
                      lPrefixes = newLP,
                      lValues = newLV
                    )
                    (newPath, (skip, take), newExportData)
                      .asLeft[(ExportData, Option[ByteVector])]
                      .pure
                  }

                case NodePtr(ptrPrefix, ptr) =>
                  for {
                    childNodeOpt <- getNodeDataFromStore(ptr)
                    childNV <- childNodeOpt.liftTo[F](
                                new Exception(s"Export error: Node with key ${ptr.toHex} not found")
                              )
                    childDecoded  = Codecs.decode(childNV)
                    childNP       = (curNodePrefix :+ itemIndex) ++ ptrPrefix
                    childNodeData = NodeData(childNP, childDecoded, none)
                    childPath     = childNodeData +: newPath

                    r = if (skip > 0) (childPath, (skip - 1, take), expData).asLeft
                    else {
                      val newNP =
                        if (settings.flagNPrefixes) expData.nPrefixes :+ childNP else Vector()
                      val newNK = if (settings.flagNKeys) expData.nKeys :+ ptr else Vector()
                      val newNV = if (settings.flagNValues) expData.nValues :+ childNV else Vector()
                      val newData = ExportData(
                        nPrefixes = newNP,
                        nKeys = newNK,
                        nValues = newNV,
                        lPrefixes = expData.lPrefixes,
                        lValues = expData.lValues
                      )
                      (childPath, (skip, take - 1), newData).asLeft
                    }
                  } yield r
              }
          }
        }
      }
    }

    assert(
      (skipSize, takeSize) != (0, 0),
      s"Export error: invalid initial conditions (skipSize, takeSize)==(0,0)."
    )

    val emptyExportData = ExportData(Vector(), Vector(), Vector(), Vector(), Vector())

    def doExport(rootNodeSer: ByteVector) = {
      val rootParams =
        NodePathData(rootHash, ByteVector.empty, lastPrefix.getOrElse(ByteVector.empty), Vector())
      val noRootStart  = (emptyExportData, skipSize, takeSize)     // Start from next node after lastPrefix
      val skippedStart = (emptyExportData, skipSize - 1, takeSize) // Skipped node start
      val rootExportData: ExportData = {
        val newNP = if (settings.flagNPrefixes) Vector(ByteVector.empty) else Vector()
        val newNK = if (settings.flagNKeys) Vector(rootHash) else Vector()
        val newNV = if (settings.flagNValues) Vector(rootNodeSer) else Vector()
        ExportData(newNP, newNK, newNV, Vector(), Vector())
      }
      val rootStart = (rootExportData, skipSize, takeSize - 1) // Take root

      // Defining init data
      val (initExportData, initSkipSize, initTakeSize) = lastPrefix
        .as(noRootStart)
        .getOrElse {
          if (skipSize > 0) skippedStart
          else rootStart
        }
      for {
        path                  <- rootParams.tailRecM(initNodePath)
        startParams: LoopData = (path, (initSkipSize, initTakeSize), initExportData)
        r                     <- startParams.tailRecM(nodeExport)
      } yield r
    }

    val emptyResult = (emptyExportData, none).pure

    for {
      rootNodeSerOpt <- getNodeDataFromStore(rootHash)
      r              <- rootNodeSerOpt.map(doExport).getOrElse(emptyResult)
    } yield r
  }

  /**
    * Radix Tree implementation
    */
  class RadixTreeImpl[F[_]: Sync: Parallel](store: KeyValueTypedStore[F, ByteVector, ByteVector]) {

    /**
      * Load and decode serializing data from KVDB.
      */
    private def loadNodeFromStore(nodePtr: ByteVector): F[Option[Node]] =
      store.get1(nodePtr).map(_.map(Codecs.decode))

    /**
      * Cache for storing read and decoded nodes.
      *
      * Cache stores kv-pairs (hash, node).
      * Where hash - Blake2b256Hash of serializing nodes data,
      *       node - deserialized data of this node.
      */
    private val cacheR: TrieMap[ByteVector, Node] = TrieMap.empty

    /**
      * Load one node from [[cacheR]].
      *
      * If there is no such record in cache - load and decode from KVDB, then save to cacheR.
      * If there is no such record in KVDB - execute assert (if set noAssert flag - return emptyNode).
      */
    def loadNode(nodePtr: ByteVector, noAssert: Boolean = false): F[Node] = {
      def errorMsg(): Unit = assert(noAssert, s"Missing node in database. ptr=${nodePtr.toHex}.")
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
      *
      * Cache stores kv-pairs (hash, bytes).
      * Where hash -  Blake2b256Hash of bytes,
      *       bytes - serializing data of nodes.
      */
    private val cacheW: TrieMap[ByteVector, ByteVector] = TrieMap.empty

    /**
      * Serializing and hashing one [[Node]].
      *
      * Serializing data load in [[cacheW]].
      * If detected collision with older cache data - executing assert
      */
    def saveNode(node: Node): ByteVector = {
      val (hash, bytes) = hashNode(node)
      // Collision alarm
      def generateErrorMsg(v: Node): Unit = assert(
        v == node,
        s"Collision in cache: record with key = ${hash.toHex} has already existed."
      )
      cacheR.get(hash).map(generateErrorMsg).getOrElse(cacheR.update(hash, node))
      cacheW.update(hash, bytes)
      hash
    }

    /**
      * Save all cacheW to KVDB
      *
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
            s"${kvCollision.length} collisions in KVDB (first collision with key = ${kvCollision.head._1.toHex})."
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
          case (_, ByteVector.empty) => Option.empty[ByteVector].asRight[Params].pure // Not found
          case (curNode, prefix) =>
            curNode(byteToInt(prefix.head)) match {
              case EmptyItem => Option.empty[ByteVector].asRight[Params].pure // Not found

              case Leaf(leafPrefix, value) =>
                if (leafPrefix == prefix.tail) value.some.asRight[Params].pure // Happy end
                else Option.empty[ByteVector].asRight[Params].pure             // Not found

              case NodePtr(ptrPrefix, ptr) =>
                val (_, prefixRest, ptrPrefixRest) = commonPrefix(prefix.tail, ptrPrefix)
                if (ptrPrefixRest.isEmpty) loadNode(ptr).map(n => (n, prefixRest).asLeft) // Deeper
                else Option.empty[ByteVector].asRight[Params].pure                        // Not found
            }
        }
      (startNode, startPrefix).tailRecM(loop)
    }

    /**
      * Create node from [[Item]].
      *
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
          case 0 => EmptyItem // All items are empty.
          case 1 => // Only one item is not empty - merge child and parent nodes.
            val idxItem = node.indexOf(nonEmptyItems.head)
            nonEmptyItems.head match {
              case EmptyItem => EmptyItem
              case Leaf(leafPrefix, value) =>
                Leaf(prefix ++ ByteVector(idxItem) ++ leafPrefix, value)
              case NodePtr(nodePtrPrefix, ptr) =>
                NodePtr(prefix ++ ByteVector(idxItem) ++ nodePtrPrefix, ptr)
            }
          case 2 => // 2 or more items are not empty.
            NodePtr(prefix, saveNode(node))
        }
      } else NodePtr(prefix, saveNode(node))

    /**
      * Save new leaf value to this part of tree (start from curItems).
      * Rehash and save all depend node to [[cacheW]].
      *
      * If exist leaf with same prefix but different value - update leaf value.
      * If exist leaf with same prefix and same value - return [[None]].
      * @return Updated current item.
      */
    def update(
        curItem: Item,
        insPrefix: ByteVector,
        insValue: ByteVector
    ): F[Option[Item]] =
      curItem match {
        case EmptyItem =>
          (Leaf(insPrefix, insValue): Item).some.pure // Update EmptyItem to Leaf.

        case Leaf(leafPrefix, leafValue) =>
          assert(leafPrefix.size == insPrefix.size, "All Radix keys should be same length.")
          if (leafPrefix == insPrefix) {
            if (insValue == leafValue) none[Item].pure
            else (Leaf(insPrefix, insValue): Item).some.pure
          } // Update Leaf.
          else {
            // Create child node, insert existing and new leaf in this node.
            // Intentionally not recursive for speed up.
            val (commPrefix, insPrefixRest, leafPrefixRest) = commonPrefix(insPrefix, leafPrefix)
            val newNode = emptyNode
              .updated(byteToInt(leafPrefixRest.head), Leaf(leafPrefixRest.tail, leafValue))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            saveNodeAndCreateItem(newNode, commPrefix, compaction = false).some.pure
          }

        case NodePtr(ptrPrefix, ptr) =>
          assert(ptrPrefix.size < insPrefix.size, "Radix key should be longer than NodePtr key.")
          val (commPrefix, insPrefixRest, ptrPrefixRest) = commonPrefix(insPrefix, ptrPrefix)
          if (ptrPrefixRest.isEmpty) {
            val (childItemIdx, childInsPrefix) =
              (byteToInt(insPrefixRest.head), insPrefixRest.tail)
            // Add new node to existing child node.
            for {
              childNode    <- loadNode(ptr)
              childItemOpt <- update(childNode(childItemIdx), childInsPrefix, insValue) // Deeper
              returnedItem = childItemOpt.map { childItem =>
                val updatedChildNode = childNode.updated(childItemIdx, childItem)
                saveNodeAndCreateItem(updatedChildNode, commPrefix, compaction = false)
              }
            } yield returnedItem
          } else {
            // Create child node, insert existing Ptr and new leaf in this node.
            val newNode = emptyNode
              .updated(byteToInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
              .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
            saveNodeAndCreateItem(newNode, commPrefix, compaction = false).some.pure
          }
      }

    /**
      * Delete leaf value from this part of tree (start from curItem).
      * Rehash and save all depend node to cacheW.
      *
      * If not found leaf with  delPrefix - return [[None]].
      * @return Updated current item.
      */
    def delete(curItem: Item, delPrefix: ByteVector): F[Option[Item]] =
      curItem match {
        case EmptyItem => none[Item].pure // Not found

        case Leaf(leafPrefix, _) =>
          if (leafPrefix == delPrefix) (EmptyItem: Item).some.pure // Happy end
          else none[Item].pure                                     // Not found

        case NodePtr(ptrPrefix, ptr) =>
          val (commPrefix, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
          if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty) none[Item].pure // Not found
          else {
            val (childItemIdx, childDelPrefix) =
              (byteToInt(delPrefixRest.head), delPrefixRest.tail)
            for {
              childNode    <- loadNode(ptr)
              childItemOpt <- delete(childNode(childItemIdx), childDelPrefix) // Deeper
              returnedChild = childItemOpt.map { childItem =>
                saveNodeAndCreateItem(childNode.updated(childItemIdx, childItem), commPrefix)
              }
            } yield returnedChild
          }
      }

    /**
      * Parallel processing of [[HistoryAction]]s in this part of tree (start from curNode).
      *
      * New data load to [[cacheW]].
      * @return Updated curNode. if no action was taken - return [[None]].
      */
    def makeActions(curNode: Node, curActions: List[HistoryAction]): F[Option[Node]] = {
      // Group the actions by the first byte of the prefix.
      val groups = curActions
        .groupBy(_.key.headOption.getOrElse {
          assert(assertion = false, "The length of all prefixes in the subtree must be the same.")
          0x00.toByte // TODO: Need rewrite this place without unused constant
        })
        .toList
      // Process actions within each group.
      val newGroupItemsF = groups.map {
        case (groupIdx, groupActions) =>
          val index = byteToInt(groupIdx)
          val item  = curNode(index)
          if (groupActions.length == 1) {
            // If we have 1 action in group.
            // We can't parallel next and we should use sequential traversing with help update() or delete().
            val newItem = groupActions.head match {
              case InsertAction(key, hash) =>
                update(item, ByteVector(key).tail, hash.bytes)
              case DeleteAction(key) => delete(item, ByteVector(key).tail)
            }
            newItem.map((index, _))
          } else {
            // If we have more than 1 action. We can create more parallel processes.
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
        // Process actions within each group.
        newGroupItems <- newGroupItemsF.parSequence
        // Update all changed items in current node.
        newCurNode = newGroupItems.foldLeft(curNode) {
          case (tempNode, (index, newItemOpt)) =>
            newItemOpt.map(tempNode.updated(index, _)).getOrElse(tempNode)
        }
      } // If current node changing return new node, otherwise return none.
      yield if (newCurNode != curNode) newCurNode.some else none
    }

    /**
      * Pretty printer for Radix tree
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
