package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
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
    * EmptyItem is item which not storing data
    */
  final case object EmptyItem extends Item

  /**
    * Leaf is item for storing radixValue. Contains 2 fields:
    * @param prefix stores a part of the radixKey. Contain from 0 to 127 bytes.
    * @param value stores radixValue. Always contains 32 bytes.
    */
  final case class Leaf(prefix: ByteVector, value: ByteVector) extends Item

  /**
    * NodePtr is pointer to the next child [[Node]]. Contains 2 fields.
    * @param prefix stores a part of the radixKey. Contain from 0 to 127 bytes.
    * @param ptr stores hash of the child node (see section Storing of tree). Always contains 32 bytes.
    */
  final case class NodePtr(prefix: ByteVector, ptr: ByteVector) extends Item

  /**
    * Base type for nodes in Radix History.
    * Node is a sequence of 256 [[Item]]s with index from 0 to 255 : Sequence(item0, item1, ..., item255).
    */
  type Node = Vector[Item]

  /**
    * Number of items always constant.
    */
  val numItems = 256

  /**
    * Empty node consists only of [[EmptyItem]]s.
    */
  val emptyNode: Node = (0 until numItems).map(_ => EmptyItem).toVector

  val emptyRootHash: Blake2b256Hash = Blake2b256Hash.fromByteVector(hashNode(emptyNode)._1)

  /**
    * Binary codecs for serializing/deserializing Node in Radix tree
    *
    * Physically, a tree may be implemented using KV-database (for example LMDB).
    * Each Node is one KV-database entry.
    * The elements are stored as an associative array of KV-pairs (hashNode, serNode), where
    * serNode = serialization(Node) is serialized node data (see section Serialization of node).
    * hashNode = hash(serNode) is a BLAKE-256 hash of serNode.
    *            The result of this hashing is a 32 byte number. Must be unique within the KV-database.
    *
    * {{{
    * Coding structure for items:
    *   EmptyItem                   - Empty (not encode)

    *   Leaf(prefix,value)    -> [item index] [second byte] [prefix0]..[prefixM] [value0]..[value31]
    *                               where is: [second byte] -> bit7 = 0 (Leaf identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    *
    *   NodePtr(prefix,ptr)   -> [item index] [second byte] [prefix0]..[prefixM] [ptr0]..[ptr31]
    *                               where is: [second byte] -> bit7 = 1 (NodePtr identifier)
    *                                                          bit6..bit0 - prefix length = M (from 0 to 127)
    *
    * For example encode this Node which contains 2 non-empty items (index 1 and index 2):
    * (0)[Empty] (1)[Leaf(prefix:0xFFFF,value:0x00..0001)] (2)[NodePtr(prefix:empty,value:0xFF..FFFF)] (3)...(255)[Empty].
    * Encoded data = 0x0102FFFF0000000000000000000000000000000000000000000000000000000000000001
    *                  0280FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    * where: item 1 (index_secondByte_prefix_value) = 01_02_FFFF_00..0001
    *        item 2 (index_secondByte_prefix_value) = 02_80_empty_FF..FFFF
    * }}}
    */
  object Codecs {
    private val defSize  = 32 // Default size for non-empty item data
    private val headSize = 2  // 2 bytes: first - item index, second - second byte

    /** Serialization [[Node]] to [[ByteVector]]
      */
    def encode(node: Node): ByteVector = {

      // Calculating size of serialized data
      val calcSize = node.foldLeft(0) {
        case (size, EmptyItem) => size

        case (size, Leaf(leafPrefix, value)) =>
          val sizePrefix = leafPrefix.size.toInt
          assert(sizePrefix <= 127, "Error during serialization: size of prefix more than 127.")
          assert(
            value.size == defSize,
            "Error during serialization: size of leafValue not equal 32."
          )
          size + headSize + sizePrefix + defSize

        case (size, NodePtr(ptrPrefix, ptr)) =>
          val sizePrefix = ptrPrefix.size.toInt
          assert(sizePrefix <= 127, "Error during serialization: size of prefix more than 127.")
          assert(
            ptr.size == defSize,
            "Error during serialization: size of ptrPrefix not equal 32."
          )
          size + headSize + sizePrefix + defSize
      }

      val arr = new Array[Byte](calcSize) //Allocation memory

      // Leaf second byte: Leaf identifier (most significant bit = 0) and prefixSize (lower 7 bits = size)
      def encodeLeafSecondByte(prefixSize: Int) = (prefixSize & 0x7F).toByte // (size & 01111111b)

      // NodePtr second byte: NodePtr identifier (most significant bit = 1) and prefixSize (lower 7 bits = size)
      def encodeNodePtrSecondByte(prefixSize: Int) =
        (0x80 | (prefixSize & 0x7F)).toByte // 10000000b | (size & 01111111b)

      // Serialization (fill allocated memory)
      @tailrec
      def putItemIntoArray(idxItem: Int, pos0: Int): Array[Byte] =
        if (pos0 == calcSize) arr // Happy end (return serializing data).
        else {
          node(idxItem) match {
            // If current item is empty - just skip serialization of this item
            case EmptyItem => putItemIntoArray(idxItem + 1, pos0) // Loop to the next item.

            case Leaf(prefix, value) =>
              // Fill first byte - item index
              arr(pos0) = idxItem.toByte
              // Fill second byte - Leaf identifier
              val posSecondByte   = pos0 + 1
              val prefixSize: Int = prefix.size.toInt
              arr(posSecondByte) = encodeLeafSecondByte(prefixSize)
              // Fill prefix
              val posPrefixStart = posSecondByte + 1
              for (i <- 0 until prefixSize) arr(posPrefixStart + i) = prefix(i.toLong)
              // Fill leafValue
              val posValueStart = posPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(posValueStart + i) = value(i.toLong)
              putItemIntoArray(idxItem + 1, posValueStart + defSize) // Loop to the next item.

            case NodePtr(prefix, ptr) =>
              // Fill first byte - item index
              arr(pos0) = idxItem.toByte
              // Fill second byte - NodePtr identifier (most significant bit = 1) and prefixSize (lower 7 bits = size)
              val posSecondByte   = pos0 + 1
              val prefixSize: Int = prefix.size.toInt
              arr(posSecondByte) = encodeNodePtrSecondByte(prefixSize)
              // Fill prefix
              val posPrefixStart = posSecondByte + 1
              for (i <- 0 until prefixSize) arr(posPrefixStart + i) = prefix(i.toLong)
              // Fill ptr
              val posPtrStart = posPrefixStart + prefixSize
              for (i <- 0 until defSize) arr(posPtrStart + i) = ptr(i.toLong)
              putItemIntoArray(idxItem + 1, posPtrStart + defSize) // Loop to the next item.
          }
        }
      ByteVector(putItemIntoArray(0, 0))
    }

    /** Deserialization [[ByteVector]] to [[Node]]
      */
    def decode(bv: ByteVector): Node = {
      val arr     = bv.toArray
      val maxSize = arr.length

      // If first bit 0 - return true, otherwise false.
      def isLeaf(secondByte: Byte) = (secondByte & 0x80) == 0x00

      @tailrec
      // Each loop decodes one non-empty item.
      def decodeItem(pos0: Int, node: Node): Node =
        if (pos0 == maxSize) node // End of deserialization
        else {
          val idxItem: Int = byteToInt(arr(pos0)) // Take first byte - it's item's index
          assert(
            node(idxItem) == EmptyItem,
            "Error during deserialization: wrong index of item."
          )
          val pos1       = pos0 + 1
          val secondByte = arr(pos1) // Take second byte

          // Decoding prefix
          val prefixSize: Int = secondByte & 0x7F // Lower 7 bits - it's size of prefix (0..127).
          val prefix          = new Array[Byte](prefixSize)
          val posPrefixStart  = pos1 + 1
          for (i <- 0 until prefixSize) prefix(i) = arr(posPrefixStart + i) // Take prefix

          // Decoding leaf or nodePtr data
          val valOrPtr         = new Array[Byte](defSize)
          val posValOrPtrStart = posPrefixStart + prefixSize
          for (i <- 0 until defSize)
            valOrPtr(i) = arr(posValOrPtrStart + i) // Take next 32 bytes - it's data

          val pos0Next = posValOrPtrStart + defSize // Calculating start position for next loop

          // Decoding type of non-empty item
          val item =
            if (isLeaf(secondByte)) Leaf(ByteVector(prefix), ByteVector(valOrPtr))
            else NodePtr(ByteVector(prefix), ByteVector(valOrPtr))

          val nodeNext = node.updated(idxItem, item)

          decodeItem(pos0Next, nodeNext) // Try to decode next item.
        }

      try {
        decodeItem(0, emptyNode)
      } catch {
        case _: Exception =>
          assert(assertion = false, "Error during deserialization: invalid data format")
          emptyNode
      }
    }
  }

  /**
    * Find the common part of b1 and b2.
    *
    * @return (Common part , rest of b1, rest of b2).
    */
  def commonPrefix(b1: ByteVector, b2: ByteVector): (ByteVector, ByteVector, ByteVector) = {
    @tailrec
    def go(common: ByteVector, l: ByteVector, r: ByteVector): (ByteVector, ByteVector, ByteVector) =
      if (r.isEmpty || l.isEmpty) (common, l, r)
      else {
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
    * @param nodePrefixes Node prefixes
    * @param nodeKeys Node KVDB keys
    * @param nodeValues Node KVDB values
    * @param leafPrefixes Leaf prefixes
    * @param leafValues Leaf values (it's pointer for data in datastore)
    */
  final case class ExportData(
      nodePrefixes: Seq[ByteVector],
      nodeKeys: Seq[ByteVector],
      nodeValues: Seq[ByteVector],
      leafPrefixes: Seq[ByteVector],
      leafValues: Seq[ByteVector]
  )

  /**
    * Settings for [[ExportData]]
    *
    * If false - data will not be exported.
    */
  final case class ExportDataSettings(
      flagNodePrefixes: Boolean,
      flagNodeKeys: Boolean,
      flagNodeValues: Boolean,
      flagLeafPrefixes: Boolean,
      flagLeafValues: Boolean
  )

  /**
    * Sequential export algorithm
    *
    * @param rootHash Root node hash, starting point
    * @param lastPrefix Describes the path of root to last processed element (if None - start from root)
    * @param skipSize Describes how many elements to skip
    * @param takeSize Describes how many elements to take
    * @param getNodeDataFromStore Function to get data from storage
    * @param settings [[ExportDataSettings]]
    *
    * @return
    * Return the data and prefix of the last processed item.
    * If all bonds in the tree are processed, returns None as prefix.
    * {{{
    * prefix - Prefix that describes the path of root to node
    * decoded - Deserialized data (from parsing)
    * lastItemIndex - Last processed item index
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
        hash: ByteVector,       // Hash of node for load
        nodePrefix: ByteVector, // Prefix of this node
        restPrefix: ByteVector, // Prefix that describes the rest of the Path
        path: Path              // Return path
    )

    /**
      * Create path from root to lastPrefix node
      */
    def initNodePath(p: NodePathData): F[Either[NodePathData, Path]] = {
      def processChildItem(node: Node) = {
        val itemIdx = byteToInt(p.restPrefix.head)
        node(itemIdx) match {
          case NodePtr(ptrPrefix, ptr) =>
            val (prefixCommon, prefixRest, ptrPrefixRest) =
              commonPrefix(p.restPrefix.tail, ptrPrefix)
            assert(
              ptrPrefixRest.isEmpty,
              s"Export error: node with prefix ${(p.nodePrefix ++ p.restPrefix).toHex} not found."
            )
            NodePathData(
              ptr,
              (p.nodePrefix :+ p.restPrefix.head) ++ prefixCommon,
              prefixRest,
              NodeData(p.nodePrefix, node, p.restPrefix.head.some) +: p.path
            ).asLeft
          case _ =>
            assert(
              assertion = false,
              s"Export error: node with prefix ${(p.nodePrefix ++ p.restPrefix).toHex} not found."
            )
            Vector().asRight // Not found
        }
      }
      for {
        nodeOpt <- getNodeDataFromStore(p.hash)
        node <- nodeOpt.liftTo[F](
                 new Exception(s"Export error: node with key ${p.hash.toHex} not found.")
               )
        decodedNode = Codecs.decode(node)
      } yield
        if (p.restPrefix.isEmpty)
          (NodeData(p.nodePrefix, decodedNode, none) +: p.path).asRight // Happy end
        else processChildItem(decodedNode)                              // Go dipper
    }

    /**
      * Find next non-empty item.
      *
      * @param node Node to look for
      * @param lastIdxOpt Last found index (if this node was not searched - [[None]])
      * @return [[Some]](idxItem, [[Item]]) if item found, [[None]] if non-empty item not found
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
            if (settings.flagLeafPrefixes || settings.flagLeafValues) (curIdx, curItem).some
            else findNextNonEmptyItem(node, curIdx.some)
          case NodePtr(_, _) => (curIdx, curItem).some
        }
      }

    final case class StepData(
        path: Path,         // Path of node from current to root
        skip: Int,          // Skip counter
        take: Int,          // Take counter
        expData: ExportData // Result of export
    )

    def addLeaf(
        p: StepData,
        leafPrefix: ByteVector,
        leafValue: ByteVector,
        itemIndex: Byte,
        curNodePrefix: ByteVector,
        newPath: Vector[NodeData]
    ): StepData =
      if (p.skip > 0)
        StepData(newPath, p.skip, p.take, p.expData)
      else {
        val newLP = if (settings.flagLeafPrefixes) {
          val newSingleLP = (curNodePrefix :+ itemIndex) ++ leafPrefix
          p.expData.leafPrefixes :+ newSingleLP
        } else Vector()
        val newLV =
          if (settings.flagLeafValues) p.expData.leafValues :+ leafValue
          else Vector()
        val newExportData = ExportData(
          nodePrefixes = p.expData.nodePrefixes,
          nodeKeys = p.expData.nodeKeys,
          nodeValues = p.expData.nodeValues,
          leafPrefixes = newLP,
          leafValues = newLV
        )
        StepData(newPath, p.skip, p.take, newExportData)
      }

    def addNodePtr(
        p: StepData,
        ptrPrefix: ByteVector,
        ptr: ByteVector,
        itemIndex: Byte,
        curNodePrefix: ByteVector,
        newPath: Vector[NodeData]
    ): F[StepData] = {
      def constructNodePtrData(
          childPath: Vector[NodeData],
          childNP: ByteVector,
          childNV: ByteVector
      ) = {
        val newNP =
          if (settings.flagNodePrefixes) p.expData.nodePrefixes :+ childNP
          else Vector()
        val newNK =
          if (settings.flagNodeKeys) p.expData.nodeKeys :+ ptr else Vector()
        val newNV =
          if (settings.flagNodeValues) p.expData.nodeValues :+ childNV
          else Vector()
        val newData = ExportData(
          nodePrefixes = newNP,
          nodeKeys = newNK,
          nodeValues = newNV,
          leafPrefixes = p.expData.leafPrefixes,
          leafValues = p.expData.leafValues
        )
        StepData(childPath, p.skip, p.take - 1, newData)
      }
      for {
        childNodeOpt <- getNodeDataFromStore(ptr)
        childNV <- childNodeOpt.liftTo[F](
                    new Exception(
                      s"Export error: Node with key ${ptr.toHex} not found"
                    )
                  )
        childDecoded  = Codecs.decode(childNV)
        childNP       = (curNodePrefix :+ itemIndex) ++ ptrPrefix
        childNodeData = NodeData(childNP, childDecoded, none)
        childPath     = childNodeData +: newPath
      } yield
        if (p.skip > 0) StepData(childPath, p.skip - 1, p.take, p.expData)
        else constructNodePtrData(childPath, childNP, childNV)
    }
    def addElement(
        p: StepData,
        itemIndex: Byte,
        item: Item,
        curNode: Node,
        curNodePrefix: ByteVector
    ): F[StepData] = Sync[F].defer {
      val newCurNodeData = NodeData(curNodePrefix, curNode, itemIndex.some)
      val newPath        = newCurNodeData +: p.path.tail
      item match {
        case EmptyItem =>
          StepData(newPath, p.skip, p.take, p.expData).pure
        case Leaf(leafPrefix, leafValue) =>
          addLeaf(p, leafPrefix, leafValue, itemIndex, curNodePrefix, newPath).pure
        case NodePtr(ptrPrefix, ptr) =>
          addNodePtr(p, ptrPrefix, ptr, itemIndex, curNodePrefix, newPath)
      }
    }

    /**
      * Export one element (Node or Leaf) and recursively move to the next step.
      */
    def exportStep(p: StepData): F[Either[StepData, (ExportData, Option[ByteVector])]] =
      Sync[F].defer {
        if (p.path.isEmpty) // End of Tree
          (p.expData, Option.empty[ByteVector]).asRight[StepData].pure
        else {
          val curNodeData   = p.path.head
          val curNodePrefix = curNodeData.prefix
          val curNode       = curNodeData.decoded

          if ((p.skip, p.take) == (0, 0))
            (p.expData, curNodePrefix.some).asRight[StepData].pure // End of skip&take counter
          else {
            val nextNotEmptyItemOpt = findNextNonEmptyItem(curNode, curNodeData.lastItemIndex)
            val newStepDataF = nextNotEmptyItemOpt
              .map {
                case (itemIndex, item) =>
                  addElement(p, itemIndex, item, curNode, curNodePrefix)
              }
              .getOrElse(StepData(p.path.tail, p.skip, p.take, p.expData).pure)
            newStepDataF.map(_.asLeft)
          }
        }
      }

    def initConditionsException: F[Unit] =
      new RuntimeException(
        s"Export error: invalid initial conditions (skipSize, takeSize)==(0,0)."
      ).raiseError
    def emptyExportData = ExportData(Vector(), Vector(), Vector(), Vector(), Vector())
    def emptyResult     = (emptyExportData, none).pure

    def doExport(rootNodeSer: ByteVector) =
      for {
        rootParams <- NodePathData(
                       rootHash,
                       ByteVector.empty,
                       lastPrefix.getOrElse(ByteVector.empty),
                       Vector()
                     ).pure
        noRootStart  = (emptyExportData, skipSize, takeSize)     // Start from next node after lastPrefix
        skippedStart = (emptyExportData, skipSize - 1, takeSize) // Skipped node start
        rootExportData = {
          val newNP = if (settings.flagNodePrefixes) Vector(ByteVector.empty) else Vector()
          val newNK = if (settings.flagNodeKeys) Vector(rootHash) else Vector()
          val newNV = if (settings.flagNodeValues) Vector(rootNodeSer) else Vector()
          ExportData(newNP, newNK, newNV, Vector(), Vector())
        }
        rootStart = (rootExportData, skipSize, takeSize - 1) // Take root

        // Defining init data
        (initExportData, initSkipSize, initTakeSize) = lastPrefix
          .as(noRootStart)
          .getOrElse {
            if (skipSize > 0) skippedStart
            else rootStart
          }
        path                  <- rootParams.tailRecM(initNodePath)
        startParams: StepData = StepData(path, initSkipSize, initTakeSize, initExportData)
        r                     <- startParams.tailRecM(exportStep)
      } yield r

    for {
      _              <- initConditionsException.whenA((skipSize, takeSize) == (0, 0))
      rootNodeSerOpt <- getNodeDataFromStore(rootHash)
      r              <- rootNodeSerOpt.map(doExport).getOrElse(emptyResult)
    } yield r
  }

  /**
    * Radix Tree implementation
    * Is a data structure that stores and links together a set
    * of N KV-pairs (radixKey, radixValue): (key0, value0), (key1, value1), ..., (keyN-1, valueN-1).
    * Where: N - number of data in the set,
    *        radixKey - unique identifier of a pair within a set,
    *        radixValue - data of the KV-pair.
    *
    * The entire set is uniquely identified using the rootHash value,
    * which is calculated when building the Tree. After that rootHash is used to access the elements in the set.
    *
    * As a data structure, the version of Radix tree modified for RSpace is used.
    * Radix tree represents a space-optimized Prefix tree in which each node
    * that is the only child is merged with its parent.
    *
    * - radixKey stored implicitly. radixKey is determined by the path to the leaf in the tree.
    * The radixKey is formed by connecting the symbol assigned to the edges of the graph.
    * These edges run from the root to the given leaf.
    *
    * - radixValue is stored explicitly as a leaf.
    *
    * 1 byte is used as symbol in the path. Such symbol can take 256 values from 0x00 to 0xFF.
    * In this case, the maximum number of children for each node is 256.
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
      def cacheMiss =
        for {
          storeNodeOpt <- loadNodeFromStore(nodePtr)
          _            = storeNodeOpt.map(cacheR.update(nodePtr, _)).getOrElse(errorMsg())
        } yield storeNodeOpt.getOrElse(emptyNode)
      for {
        cacheNodeOpt <- Sync[F].delay(cacheR.get(nodePtr))
        r            <- cacheNodeOpt.map(_.pure).getOrElse(cacheMiss)
      } yield r
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
      def checkCollision(v: Node): Unit = assert(
        v == node,
        s"Collision in cache: record with key = ${hash.toHex} has already existed."
      )
      cacheR.get(hash).map(checkCollision).getOrElse(cacheR.update(hash, node))
      cacheW.update(hash, bytes)
      hash
    }

    /**
      * Save all [[cacheW]] to [[store]]
      *
      * If detected collision with older KVDB data - execute Exception
      */
    def commit: F[Unit] = {
      def collisionException(collisions: List[(ByteVector, ByteVector)]): F[Unit] =
        new RuntimeException(
          s"${collisions.length} collisions in KVDB (first collision with key = ${collisions.head._1.toHex})."
        ).raiseError
      for {
        kvPairs           <- Sync[F].delay(cacheW.toList)
        ifAbsent          <- store.contains(kvPairs.map(_._1))
        kvIfAbsent        = kvPairs zip ifAbsent
        kvExist           = kvIfAbsent.filter(_._2).map(_._1)
        valueExistInStore <- store.get(kvExist.map(_._1))
        kvvExist          = kvExist zip valueExistInStore.map(_.getOrElse(ByteVector.empty))
        kvCollision       = kvvExist.filter(kvv => !(kvv._1._2 == kvv._2)).map(_._1)
        _                 <- if (kvCollision.nonEmpty) collisionException(kvCollision) else ().pure
        kvAbsent          = kvIfAbsent.filterNot(_._2).map(_._1)
        _                 <- store.put(kvAbsent)
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

    private def createNodeFromItem(item: Item): Node =
      item match {
        case EmptyItem => emptyNode
        case Leaf(leafPrefix, leafValue) =>
          assert(
            leafPrefix.nonEmpty,
            "Impossible to create a node. LeafPrefix should be non empty."
          )
          emptyNode.updated(byteToInt(leafPrefix.head), Leaf(leafPrefix.tail, leafValue))
        case NodePtr(nodePtrPrefix, ptr) =>
          assert(
            nodePtrPrefix.nonEmpty,
            "Impossible to create a node. NodePtrPrefix should be non empty."
          )
          emptyNode
            .updated(byteToInt(nodePtrPrefix.head), NodePtr(nodePtrPrefix.tail, ptr))
      }

    /**
      * Create node from [[Item]].
      *
      * If item is NodePtr and prefix is empty - load child node
      */
    def constructNodeFromItem(item: Item): F[Node] =
      item match {
        case NodePtr(ByteVector.empty, ptr) => loadNode(ptr)
        case _                              => Sync[F].delay(createNodeFromItem(item))
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
    ): F[Option[Item]] = {

      def insertNewNodeToChild(
          childPtr: ByteVector,
          childPrefix: ByteVector,
          insPrefix: ByteVector
      ) =
        for {
          childNode                      <- loadNode(childPtr)
          (childItemIdx, childInsPrefix) = (byteToInt(insPrefix.head), insPrefix.tail)
          childItemOpt                   <- update(childNode(childItemIdx), childInsPrefix, insValue) // Deeper
          returnedItem = childItemOpt.map { childItem =>
            val updatedChildNode = childNode.updated(childItemIdx, childItem)
            saveNodeAndCreateItem(updatedChildNode, childPrefix, compaction = false)
          }
        } yield returnedItem

      Sync[F].defer {
        curItem match {
          case EmptyItem =>
            (Leaf(insPrefix, insValue): Item).some.pure // Update EmptyItem to Leaf.

          case Leaf(leafPrefix, leafValue) =>
            assert(
              leafPrefix.size == insPrefix.size,
              "The length of all prefixes in the subtree must be the same."
            )
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
            if (ptrPrefixRest.isEmpty)
              insertNewNodeToChild(ptr, commPrefix, insPrefixRest) // Add new node to existing child node.
            else {
              // Create child node, insert existing Ptr and new leaf in this node.
              val newNode = emptyNode
                .updated(byteToInt(ptrPrefixRest.head), NodePtr(ptrPrefixRest.tail, ptr))
                .updated(byteToInt(insPrefixRest.head), Leaf(insPrefixRest.tail, insValue))
              saveNodeAndCreateItem(newNode, commPrefix, compaction = false).some.pure
            }
        }
      }
    }

    /**
      * Delete leaf value from this part of tree (start from curItem).
      * Rehash and save all depend node to cacheW.
      *
      * If not found leaf with  delPrefix - return [[None]].
      * @return Updated current item.
      */
    def delete(curItem: Item, delPrefix: ByteVector): F[Option[Item]] = {
      def deleteFromChildNode(
          childPtr: ByteVector,
          childPrefix: ByteVector,
          delPrefix: ByteVector
      ): F[Option[Item]] =
        for {
          childNode                   <- loadNode(childPtr)
          (delItemIdx, delItemPrefix) = (byteToInt(delPrefix.head), delPrefix.tail)
          childItemOpt                <- delete(childNode(delItemIdx), delItemPrefix)
          newChildNode = childItemOpt.map { childItem =>
            saveNodeAndCreateItem(childNode.updated(delItemIdx, childItem), childPrefix)
          }
        } yield newChildNode

      Sync[F].defer {
        curItem match {
          case EmptyItem => none[Item].pure // Not found

          case Leaf(leafPrefix, _) =>
            if (leafPrefix == delPrefix) (EmptyItem: Item).some.pure // Happy end
            else none[Item].pure                                     // Not found

          case NodePtr(ptrPrefix, ptr) =>
            val (commPrefix, delPrefixRest, ptrPrefixRest) = commonPrefix(delPrefix, ptrPrefix)
            if (ptrPrefixRest.nonEmpty || delPrefixRest.isEmpty) none[Item].pure // Not found
            else deleteFromChildNode(ptr, commPrefix, delPrefixRest)             // Deeper
        }
      }
    }

    /**
      * Parallel processing of [[HistoryAction]]s in this part of tree (start from curNode).
      *
      * New data load to [[cacheW]].
      * @return Updated curNode. if no action was taken - return [[None]].
      */
    def makeActions(curNode: Node, actions: List[HistoryAction]): F[Option[Node]] = {

      // If we have 1 action in group.
      // We can't parallel next and we should use sequential traversing with help update() or delete().
      def processOneAction(action: HistoryAction, item: Item, itemIdx: Int) =
        for {
          newItem <- action match {
                      case InsertAction(key, hash) => update(item, ByteVector(key).tail, hash.bytes)
                      case DeleteAction(key)       => delete(item, ByteVector(key).tail)
                    }
        } yield (itemIdx, newItem)

      def clearingDeleteActions(actions: List[HistoryAction], item: Item) = {
        val notExistInsertAction = actions.collectFirst { case _: InsertAction => true }.isEmpty
        if (item == EmptyItem && notExistInsertAction) List() else actions
      }

      def trimKeys(actions: List[HistoryAction]): List[HistoryAction] =
        actions.map {
          case InsertAction(key, hash) => InsertAction(key.tail, hash)
          case DeleteAction(key)       => DeleteAction(key.tail)
        }

      def processNonEmptyActions(actions: List[HistoryAction], item: Item, itemIdx: Int) =
        for {
          createdNode <- constructNodeFromItem(item)
          newActions  = trimKeys(actions)
          newNodeOpt  <- makeActions(createdNode, newActions)
          newItem     = newNodeOpt.map(saveNodeAndCreateItem(_, ByteVector.empty))
        } yield (itemIdx, newItem)

      // If we have more than 1 action. We can create more parallel processes.
      def processSeveralActions(actions: List[HistoryAction], item: Item, itemIdx: Int) =
        for {
          clearedActions <- Sync[F].delay(clearingDeleteActions(actions, item))
          r <- if (clearedActions.isEmpty) (itemIdx, none[Item]).pure
              else processNonEmptyActions(clearedActions, item, itemIdx)
        } yield r

      // Process actions within each group.
      def processGroupedActions(groupedActions: List[(Byte, List[HistoryAction])], curNode: Node) =
        groupedActions.map {
          case (groupIdx, actionsInGroup) =>
            for {
              itemIdx <- Sync[F].delay(byteToInt(groupIdx))
              item    = curNode(itemIdx)
              r <- if (actionsInGroup.length == 1)
                    processOneAction(actionsInGroup.head, item, itemIdx)
                  else processSeveralActions(actionsInGroup, item, itemIdx)
            } yield r
        }

      // Group the actions by the first byte of the prefix.
      def grouping(actions: List[HistoryAction]) =
        actions.groupBy { action =>
          val firstByteOpt = action.key.headOption
          assert(
            firstByteOpt.nonEmpty,
            "The length of all prefixes in the subtree must be the same."
          )
          firstByteOpt.get
        }.toList

      for {
        // Group the actions by the first byte of the prefix.
        groupedActions <- Sync[F].delay(grouping(actions))
        // Process actions within each group.
        newGroupItems <- processGroupedActions(groupedActions, curNode).parSequence
        // Update all changed items in current node.
        newCurNode = newGroupItems.foldLeft(curNode) {
          case (tempNode, (index, newItemOpt)) =>
            newItemOpt.map(tempNode.updated(index, _)).getOrElse(tempNode)
        }
      } // If current node changing return new node, otherwise return none.
      yield if (newCurNode != curNode) newCurNode.some else none
    }

    /**
      * Changing the tree according to [[HistoryAction]]s.
      *
      * New data load to [[store]], after that clearing [[cacheW]].
      * @return new (rootNode, rootHash). if no action was taken - return [[None]].
      */
    def saveAndCommit(rootNode: Node, actions: List[HistoryAction]): F[Option[(Node, ByteVector)]] =
      for {
        newRootNodeOpt <- makeActions(rootNode, actions)
        r <- newRootNodeOpt.traverse { newRootNode =>
              val newRootHash = saveNode(newRootNode)
              commit.as(newRootNode, newRootHash)
            }
        _ = clearWriteCache()
      } yield r

    /**
      * Pretty printer for Radix tree
      */
    final def printTree(
        rootNode: Node,
        treeName: String,
        noPrintFlag: Boolean
    ): F[Vector[String]] = {

      def constructIdxStr(idx: Int): String = {
        val firstSymbol  = ((idx >> 4).toByte & 0xF).toHexString
        val secondSymbol = (idx.toByte & 0xF).toHexString
        (firstSymbol ++ secondSymbol).toUpperCase()
      }

      def constructPrefixStr(prefix: ByteVector): String =
        if (prefix.isEmpty) "empty" else prefix.toHex.toUpperCase

      def constructLeafValueStr(leafValue: ByteVector) =
        (leafValue.toHex.take(4) ++ "..." ++ leafValue.toHex.takeRight(4)).toUpperCase

      def constructLeafStr(
          indent: String,
          idx: Int,
          leafPrefix: ByteVector,
          leafValue: ByteVector
      ) = {
        val idxStr    = constructIdxStr(idx)
        val prefixStr = constructPrefixStr(leafPrefix)
        val valueStr  = constructLeafValueStr(leafValue)
        s"$indent[$idxStr]LEAF: prefix = $prefixStr, data = $valueStr"
      }

      def constructNodePtrStr(
          indent: String,
          idx: Int,
          ptrPrefix: ByteVector
      ) = {
        val idxStr    = constructIdxStr(idx)
        val prefixStr = constructPrefixStr(ptrPrefix)
        s"$indent[$idxStr]PTR: prefix = $prefixStr, ptr =>"
      }

      def constructIdent(indentLevel: Int) = Seq.fill(indentLevel * 3)(" ").mkString

      def print(node: Node, indentLevel: Int): F[Vector[String]] =
        for {
          indent <- Sync[F].delay(constructIdent(indentLevel))
          itemVectors <- node.zipWithIndex.traverse {
                          case (EmptyItem, _) => Vector[String]().pure
                          case (Leaf(leafPrefix, value), idx) =>
                            val leafStr = constructLeafStr(indent, idx, leafPrefix, value)
                            Vector(leafStr).pure
                          case (NodePtr(ptrPrefix, ptr), idx) =>
                            for {
                              childNode    <- loadNode(ptr)
                              childTreeStr <- print(childNode, indentLevel + 1)
                              ptrStr       = constructNodePtrStr(indent, idx, ptrPrefix)
                              r            = ptrStr +: childTreeStr
                            } yield r
                        }
          res = itemVectors.foldLeft(Vector[String]())(_ ++ _)
        } yield res

      def constructFirstStr(treeName: String) = treeName.toUpperCase() + ": root =>"

      def printStrings(strings: Vector[String]) = strings.map(println(_))

      for {
        strings     <- print(rootNode, 1)
        firstString = constructFirstStr(treeName)
        r           = firstString +: strings
        _           <- if (noPrintFlag) ().pure else Sync[F].delay(printStrings(r))
      } yield r
    }
  }
}
