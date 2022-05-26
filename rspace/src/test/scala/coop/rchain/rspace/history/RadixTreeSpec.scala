package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.shared.Base16
import coop.rchain.shared.syntax.{sharedSyntaxKeyValueStore, sharedSyntaxKeyValueTypedStore}
import coop.rchain.store.{InMemoryKeyValueStore, KeyValueTypedStore}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.concurrent.duration._

class RadixTreeSpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  "appending leaf in empty tree" should "create tree with one node" in withImplAndStore {
    (impl, _) =>
      val dataSet = radixKV("1122334455", "01")
      for {
        item1 <- impl.update(
                  RadixTree.EmptyItem,
                  dataSet.rKey,
                  dataSet.rValue
                )

        newRootNode    <- impl.constructNodeFromItem(item1.get)
        printedTreeStr <- impl.printTree(newRootNode, "TREE WITH ONE LEAF", noPrintFlag = true)

        referenceTree = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [11]LEAF: prefix = 22334455, data = 0000...0001"
        )

        _ = printedTreeStr shouldBe referenceTree
      } yield ()
  }

  "appending leaf to leaf (leafs contain common prefixes)" should "create 2 leafs with node ptr" in withImplAndStore {
    (impl, _) =>
      val dataSet = List(radixKV("1122334455", "01"), radixKV("112233AABB", "02"))
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     dataSet.head.rKey,
                     dataSet.head.rValue
                   )

        rootNode1    <- impl.constructNodeFromItem(item1Opt.get)
        printedTree1 <- impl.printTree(rootNode1, "TREE WITH ONE LEAF", noPrintFlag = true)

        referenceTree1 = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [11]LEAF: prefix = 22334455, data = 0000...0001"
        )

        item2Opt <- impl.update(
                     item1Opt.get,
                     dataSet(1).rKey,
                     dataSet(1).rValue
                   )

        rootNode2 <- impl.constructNodeFromItem(item2Opt.get)
        printedTree2 <- impl.printTree(
                         rootNode2,
                         "TREE WITH ONE NODE POINTER AND 2 LEAFS",
                         noPrintFlag = true
                       )

        referenceTree2 = Vector(
          "TREE WITH ONE NODE POINTER AND 2 LEAFS: root =>",
          "   [11]PTR: prefix = 2233, ptr =>",
          "      [44]LEAF: prefix = 55, data = 0000...0001",
          "      [AA]LEAF: prefix = BB, data = 0000...0002"
        )

        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2
      } yield ()
  }
  "appending leaf with other prefix to leaf" should "create node with two leafs" in withImplAndStore {
    (impl, _) =>
      val dataSet = List(radixKV("1122334455", "01"), radixKV("AABBCCDDEE", "02"))
      for {
        rootItem1Opt <- impl.update(
                         RadixTree.EmptyItem,
                         dataSet.head.rKey,
                         dataSet.head.rValue
                       )

        rootItem2Opt <- impl.update(
                         rootItem1Opt.get,
                         dataSet(1).rKey,
                         dataSet(1).rValue
                       )

        rootNode <- impl.constructNodeFromItem(rootItem2Opt.get)

        printedTreeStr <- impl.printTree(rootNode, "TREE: TWO LEAFS", noPrintFlag = true)

        referenceTree = Vector(
          "TREE: TWO LEAFS: root =>",
          "   [11]LEAF: prefix = 22334455, data = 0000...0001",
          "   [AA]LEAF: prefix = BBCCDDEE, data = 0000...0002"
        )

        _ = printedTreeStr shouldBe referenceTree
      } yield ()
  }

  "updating leaf" should "update data in this leaf" in withImplAndStore { (impl, _) =>
    val initialKVPair = radixKV("1122334455", "01")
    val newKVPair     = radixKV("1122334455", "FF")
    for {

      // Create tree with one leaf
      item1Opt <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)

      rootNode1 <- impl.constructNodeFromItem(item1Opt.get)
      printedTree1 <- impl.printTree(
                       rootNode1,
                       "TREE WITH ONE LEAF",
                       noPrintFlag = true
                     )

      item2Opt  <- impl.update(item1Opt.get, newKVPair.rKey, newKVPair.rValue)
      rootNode2 <- impl.constructNodeFromItem(item2Opt.get)

      printedTree2 <- impl.printTree(
                       rootNode2,
                       "TREE WITH ONE LEAF (AFTER CHANGING DATA)",
                       noPrintFlag = true
                     )

      referenceTree1 = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [11]LEAF: prefix = 22334455, data = 0000...0001"
      )

      referenceTree2 = Vector(
        "TREE WITH ONE LEAF (AFTER CHANGING DATA): root =>",
        "   [11]LEAF: prefix = 22334455, data = 0000...00FF"
      )

      _ = printedTree1 shouldBe referenceTree1
      _ = printedTree2 shouldBe referenceTree2
    } yield ()
  }

  "RadixTreeImpl" should "not allow to enter keys with different lengths in the subtree" in withImplAndStore {
    (impl, _) =>
      val initialKVPair = radixKV("1122334455", "01")
      val wrongKVPair   = radixKV("112233", "02")
      val referenceErrorMessage =
        s"assertion failed: The length of all prefixes in the subtree must be the same."
      for {
        leafItemOpt <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)
        err         <- impl.update(leafItemOpt.get, wrongKVPair.rKey, wrongKVPair.rValue).attempt

        ex = err.left.get
        _  = ex shouldBe a[AssertionError]
        _  = ex.getMessage shouldBe referenceErrorMessage
      } yield ()
  }

  "RadixTreeImpl" should "not allow to radix key smaller than NodePtr key" in withImplAndStore {
    (impl, _) =>
      val initialItem           = NodePtr(createBV("11223344"), createBlakeHash("01"))
      val wrongKVPair           = radixKV("11", "FF")
      val referenceErrorMessage = s"assertion failed: Radix key should be longer than NodePtr key."
      for {
        err <- impl
                .update(
                  initialItem,
                  wrongKVPair.rKey,
                  wrongKVPair.rValue
                )
                .attempt

        ex = err.left.get
        _  = ex shouldBe a[AssertionError]
        _  = ex.getMessage shouldBe referenceErrorMessage
      } yield ()
  }

  "deleting non - existent data" should "return none" in withImplAndStore { (impl, _) =>
    val initialKVPair  = radixKV("1122334455", "01")
    val nonExistentKey = createBV("FFFFFFFFFF")
    for {
      // Create tree with one node
      itemOpt <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)
      _       <- impl.constructNodeFromItem(itemOpt.get)

      // Trying to delete not existing leaf...
      del <- impl.delete(itemOpt.get, nonExistentKey)

      _ = del.map(item => item shouldBe None)
    } yield ()
  }

  "deleting leaf from tree with only one leaf" should "destroy tree" in withImplAndStore {
    (impl, _) =>
      val initialKVPair = radixKV("1122334455", "01")
      for {
        // Create tree with one node
        itemOpt <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)

        // Trying to delete not existing leaf...
        item3Opt  <- impl.delete(itemOpt.get, initialKVPair.rKey)
        rootNode2 <- impl.constructNodeFromItem(item3Opt.get)

        printedEmptyTreeStr <- impl.printTree(rootNode2, "EMPTY TREE", noPrintFlag = true)

        referenceEmptyTreeStr = Vector("EMPTY TREE: root =>")

        _ = item3Opt.map(item => item shouldBe RadixTree.EmptyItem)
        _ = referenceEmptyTreeStr shouldBe printedEmptyTreeStr
      } yield ()
  }

  "deleting leaf from node with two leafs" should "leave one leaf" in withImplAndStore {
    (impl, _) =>
      val dataSet = List(radixKV("1122334455", "01"), radixKV("AABBCCDDEE", "02"))
      for {
        rootItem1Opt <- impl.update(
                         RadixTree.EmptyItem,
                         dataSet.head.rKey,
                         dataSet.head.rValue
                       )

        rootItem2Opt <- impl.update(
                         rootItem1Opt.get,
                         dataSet(1).rKey,
                         dataSet(1).rValue
                       )

        rootNode1 <- impl.constructNodeFromItem(rootItem2Opt.get)
        printedTree1 <- impl.printTree(
                         rootNode1,
                         "TREE: TWO LEAFS (BEFORE DELETING)",
                         noPrintFlag = true
                       )

        itemIdx <- Sync[Task].delay(byteToInt(dataSet.head.rKey.head))

        itemToDelete = rootNode1(itemIdx)
        item3Opt     <- impl.delete(itemToDelete, dataSet.head.rKey.tail)
        rootNode2    = rootNode1.updated(itemIdx, item3Opt.get)

        printedTree2 <- impl.printTree(
                         rootNode2,
                         "TREE: TWO LEAFS (AFTER DELETING)",
                         noPrintFlag = true
                       )

        referenceTree1 = Vector(
          "TREE: TWO LEAFS (BEFORE DELETING): root =>",
          "   [11]LEAF: prefix = 22334455, data = 0000...0001",
          "   [AA]LEAF: prefix = BBCCDDEE, data = 0000...0002"
        )

        referenceTree2 = Vector(
          "TREE: TWO LEAFS (AFTER DELETING): root =>",
          "   [AA]LEAF: prefix = BBCCDDEE, data = 0000...0002"
        )
        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2
      } yield ()
  }

  "deleting one leaf from a child node containing two leafs" should "delete child node and reformat parent node" in withImplAndStore {
    (impl, _) =>
      val dataSet = List(radixKV("1122334455", "01"), radixKV("11223344FF", "02"))
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     dataSet.head.rKey,
                     dataSet.head.rValue
                   )
        item2Opt <- impl.update(
                     item1Opt.get,
                     dataSet(1).rKey,
                     dataSet(1).rValue
                   )

        rootNode1 <- impl.constructNodeFromItem(item2Opt.get)
        printedTree1 <- impl.printTree(
                         rootNode1,
                         "TREE WITH ONE NODE AND 2 LEAFS",
                         noPrintFlag = true
                       )

        referenceTree1 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [11]PTR: prefix = 223344, ptr =>",
          "      [55]LEAF: prefix = empty, data = 0000...0001",
          "      [FF]LEAF: prefix = empty, data = 0000...0002"
        )

        item3Opt  <- impl.delete(rootNode1(0x11), dataSet.head.rKey.tail)
        rootNode2 <- impl.constructNodeFromItem(item3Opt.get)

        printedTree2 <- impl.printTree(rootNode2, "TREE (AFTER DELETE)", noPrintFlag = true)

        referenceTree2 = Vector(
          "TREE (AFTER DELETE): root =>",
          "   [22]LEAF: prefix = 3344FF, data = 0000...0002"
        )

        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2

      } yield ()
  }

  "reading data from existing node" should "return data" in withImplAndStore { (impl, _) =>
    val initialKVPair = radixKV("1122334455", "01")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      readDataOpt <- impl.read(rootNode, initialKVPair.rKey)

      _ = readDataOpt.get shouldBe initialKVPair.rValue
    } yield ()
  }

  "reading non - existent data" should "return none" in withImplAndStore { (impl, _) =>
    val initialKVPair = radixKV("1122334455", "01")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, initialKVPair.rKey, initialKVPair.rValue)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      notExistingKey = createBV("0000")
      readDataOpt    <- impl.read(rootNode, notExistingKey)

      _ = readDataOpt shouldBe none

    } yield ()
  }

  "Call of functions saveNode() and then commit()" should "put node into store" in withImplAndStore {
    (impl, inMemoStore) =>
      for {
        nodesCount1 <- Sync[Task].delay(inMemoStore.numRecords())
        _           = impl.saveNode(emptyNode)
        _           <- impl.commit

        // After saving node numRecords must return 1
        nodesCount2 = inMemoStore.numRecords()
        _           = nodesCount1 shouldBe 0
        _           = nodesCount2 shouldBe 1
      } yield ()
  }

  "function loadNode" should "load node from store" in withImplAndStore { (impl, _) =>
    for {
      hash <- Sync[Task].delay(impl.saveNode(emptyNode))
      _    <- impl.commit

      _        = impl.clearReadCache()
      _        = impl.clearWriteCache()
      loadNode <- impl.loadNode(hash)
      _        = loadNode shouldBe emptyNode
    } yield ()
  }

  "Trying to load a non-existent node" should "throw error" in withImplAndStore { (impl, store) =>
    {
      for {
        hash <- Sync[Task].delay(impl.saveNode(emptyNode))
        _    <- impl.commit
        _    = store.clear() // Clearing database
        _    = impl.clearReadCache()
        _    = impl.clearWriteCache()

        err                   <- impl.loadNode(hash).attempt
        ex                    = err.left.get
        _                     = ex shouldBe a[AssertionError]
        referenceErrorMessage = s"assertion failed: Missing node in database. ptr=${hash.bytes.toHex}."
        _                     = ex.getMessage shouldBe referenceErrorMessage
      } yield ()
    }
  }

  // Data for test are given from RadixTree specification
  "encoding and then decoding a node" should "give this node" in {
    val leaf = Leaf(
      createBV("FFFF"),
      createBlakeHash("0000000000000000000000000000000000000000000000000000000000000001")
    )
    val nodePtr = NodePtr(
      createBV(""),
      createBlakeHash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    )
    val referenceNode = emptyNode
      .updated(1, leaf)
      .updated(2, nodePtr)

    val serializedNode   = RadixTree.Codecs.encode(referenceNode)
    val deserializedNode = RadixTree.Codecs.decode(serializedNode)
    val referenceSerialized = createBV(
      "0102FFFF00000000000000000000000000000000000000000000000000000000000000010280FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
    )
    deserializedNode shouldBe referenceNode
    serializedNode shouldBe referenceSerialized
  }

  "decode wrong serialized data" should "be stopped with assertion error" in {
    val wrongSerialized = createBV(
      "0102FFFF00000000000000000000000000000010280FFFFFFFFFFFFFFFFFFFFFFFFFF"
    )
    try {
      RadixTree.Codecs.decode(wrongSerialized)
    } catch {
      case ex: Exception =>
        ex shouldBe a[Exception]
        ex.getMessage shouldBe s"Error during deserialization: invalid data format"
    }
  }

  "collisions in KVDB" should "be detected" in withImplAndStore { (impl, inMemoStore) =>
    def copyBVToBuf(bv: ByteVector): ByteBuffer = {
      val arr    = bv.toArray
      val newBuf = ByteBuffer.allocateDirect(arr.length)
      newBuf.put(arr).rewind()
    }

    val collisionKVPair = (copyBVToBuf(RadixTree.emptyRootHash.bytes), ByteVector(0x00))
    val referenceErrorMessage = s"1 collisions in KVDB (first collision with key = " +
      s"${RadixTree.emptyRootHash.bytes.toHex})."
    for {
      _   <- inMemoStore.put[ByteVector](Seq(collisionKVPair), copyBVToBuf)
      _   = impl.saveNode(emptyNode)
      err <- impl.commit.attempt

      ex = err.left.get
      _  = ex shouldBe a[RuntimeException]
      _  = ex.getMessage shouldBe referenceErrorMessage
    } yield ()
  }

  "tree with saveAndCommit" should "be built correctly and not create artefacts in KV - store" in withImplAndStore {
    (impl, inMemoStore) =>
      def createDeleteActions(keys: List[ByteVector]): List[DeleteAction] =
        keys.map(key => DeleteAction(key.toSeq))

      /* treeDataSet:
            key      |   value
        111122334455 | 0000...0001
        11112233AABB | 0000...0002
        1111AABBCC   | 0000...0003
        33           | 0000...0004
        FF0011       | 0000...0005
        FF012222     | 0000...0006
       */
      val deleteActions = createDeleteActions(treeDataSet.map(_.rKey))

      val referenceTree1 = Vector(
        "TREE1: root =>",
        "   [11]PTR: prefix = 11, ptr =>",
        "      [22]PTR: prefix = 33, ptr =>",
        "         [44]LEAF: prefix = 55, data = 0000...0001",
        "         [AA]LEAF: prefix = BB, data = 0000...0002",
        "      [AA]LEAF: prefix = BBCC, data = 0000...0003",
        "   [33]LEAF: prefix = empty, data = 0000...0004",
        "   [FF]PTR: prefix = empty, ptr =>",
        "      [00]LEAF: prefix = 11, data = 0000...0005",
        "      [01]LEAF: prefix = 2222, data = 0000...0006"
      )
      val referenceTree2 = Vector("TREE2: root =>")
      for {
        // 1  Build a tree according to the example in specification
        rootNodeAndHash1Opt <- impl.saveAndCommit(RadixTree.emptyNode, referenceInsertActions)
        (rootNode1, _)      = rootNodeAndHash1Opt.get

        // Get the tree for compare with reference
        tree1 <- impl.printTree(rootNode1, treeName = "TREE1", noPrintFlag = true)

        // Number of nodes must be equal to 4 (with root)
        nodesCount1 = inMemoStore.numRecords()

        // 2   Delete all data from tree...
        rootNodeAndHash2Opt <- impl.saveAndCommit(rootNode1, deleteActions)

        (rootNode2, _) = rootNodeAndHash2Opt.get
        tree2          <- impl.printTree(rootNode2, treeName = "TREE2", noPrintFlag = true)

        // Number of nodes after deleting data must be equal to 5 (with root)
        nodesCount2 = inMemoStore.numRecords()

        _ = rootNode2 shouldBe RadixTree.emptyNode
        _ = tree1 shouldBe referenceTree1
        _ = nodesCount1 shouldBe 4
        _ = tree2 shouldBe referenceTree2
        _ = nodesCount2 shouldBe 5
      } yield ()
  }

  "sequentialExport" should "export all data from tree" in withImplAndStore { (impl, store) =>
    for {
      // Create tree with 6 leafs
      rootNodeAndHashOpt <- impl.saveAndCommit(RadixTree.emptyNode, referenceInsertActions)
      (_, rootHash)      = rootNodeAndHashOpt.get

      // First data export
      typedStore = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
      exported1 <- sequentialExport(
                    rootHash,
                    None,
                    skipSize = 0,
                    takeSize = 100,
                    typedStore.get1,
                    exportSettings
                  )
      (exportData1, _) = exported1

      // Create new storage
      nodeKVDBKeys   = exportData1.nodeKeys
      nodeKVDBValues = exportData1.nodeValues

      localStorage = (nodeKVDBKeys zip nodeKVDBValues).toMap

      // Export data from new storage
      exported2 <- {
        sequentialExport(
          rootHash,
          None,
          skipSize = 0,
          takeSize = 100,
          x => Sync[Task].delay(localStorage.get(x)),
          exportSettings
        )
      }

      (exportData2, _) = exported2

      _ = exportData1.nodeKeys.size shouldBe 4
      _ = exported1 shouldBe exported2
      _ = exportData1.leafPrefixes shouldBe referenceLeafPrefixes
      _ = exportData1.leafValues shouldBe referenceLeafValues
      _ = exportData2.leafPrefixes shouldBe referenceLeafPrefixes
      _ = exportData2.leafValues shouldBe referenceLeafValues
    } yield ()
  }

  "invalid initial conditions in sequentialExport" should "raise exception" in withImplAndStore {
    (impl, store) =>
      val typedStore = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
      for {
        // Create tree with 6 leafs
        rootAndHashOpt <- impl.saveAndCommit(RadixTree.emptyNode, referenceInsertActions)
        (_, rootHash)  = rootAndHashOpt.get

        // Validate exception when skipSize = 0 and takeSize == 0
        err <- sequentialExport(
                rootHash,
                None,
                skipSize = 0,
                takeSize = 0,
                typedStore.get1,
                exportSettings
              ).attempt

        ex = err.left.get
        _  = ex shouldBe a[RuntimeException]
        _  = ex.getMessage shouldBe "Export error: invalid initial conditions (skipSize, takeSize)==(0,0)."
      } yield ()
  }

  "multipage export with last prefix" should "work correctly" in withImplAndStore { (impl, store) =>
    val typedStore = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
    for {
      // Create tree with 6 leafs
      rootNodeAndHashOpt <- impl.saveAndCommit(RadixTree.emptyNode, referenceInsertActions)
      (_, rootHash)      = rootNodeAndHashOpt.get

      validateData <- validateMultipageExport(rootHash, typedStore, withSkip = false)
      (firstExportData, reconstructExportData) = (
        validateData.firstExport._1,
        validateData.reconstructExport._1
      )
      (firstExport, reconstructExport) = (validateData.firstExport, validateData.reconstructExport)

      _ = firstExportData.nodeKeys.size shouldBe 4
      _ = firstExport shouldBe reconstructExport
      _ = firstExportData.leafPrefixes shouldBe referenceLeafPrefixes
      _ = firstExportData.leafValues shouldBe referenceLeafValues
      _ = reconstructExportData.leafPrefixes shouldBe referenceLeafPrefixes
      _ = reconstructExportData.leafValues shouldBe referenceLeafValues
    } yield ()
  }

  "multipage export with skip" should "work correctly" in withImplAndStore { (impl, store) =>
    val typedStore = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
    for {
      // Create tree with 6 leafs
      rootNodeAndHashOpt <- impl.saveAndCommit(RadixTree.emptyNode, referenceInsertActions)
      (_, rootHash)      = rootNodeAndHashOpt.get

      validateData <- validateMultipageExport(rootHash, typedStore, withSkip = true)
      (firstExportData1, reconstructExportData2) = (
        validateData.firstExport._1,
        validateData.reconstructExport._1
      )
      (firstExport, reconstructExport) = (validateData.firstExport, validateData.reconstructExport)

      _ = firstExportData1.nodeKeys.size shouldBe 4
      _ = firstExport shouldBe reconstructExport
      _ = firstExportData1.leafPrefixes shouldBe referenceLeafPrefixes
      _ = firstExportData1.leafValues shouldBe referenceLeafValues
      _ = reconstructExportData2.leafPrefixes shouldBe referenceLeafPrefixes
      _ = reconstructExportData2.leafValues shouldBe referenceLeafValues
    } yield ()
  }

  "sequentialExport with non-existing tree" should "return empty data" in withImplAndStore {
    (impl, store) =>
      val typedStore    = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
      val emptyRootHash = impl.saveNode(emptyNode)
      for {
        exported1 <- sequentialExport(
                      emptyRootHash,
                      None,
                      skipSize = 0,
                      takeSize = 100,
                      typedStore.get1,
                      exportSettings
                    )
        referenceEmptyData = (ExportData(Vector(), Vector(), Vector(), Vector(), Vector()), none)
        _                  = exported1 shouldBe referenceEmptyData
      } yield ()
  }

  "function commonPrefix" should "return correct prefixes" in {
    val v12345 = ByteVector(1, 2, 3, 4, 5)
    val v1245  = ByteVector(1, 2, 4, 5)
    val v123   = ByteVector(1, 2, 3)
    val v12367 = ByteVector(1, 2, 3, 6, 7)
    val v22345 = ByteVector(2, 2, 3, 4, 5)
    val res1   = RadixTree.commonPrefix(v12345, v1245)
    val res2   = RadixTree.commonPrefix(v12345, v123)
    val res3   = RadixTree.commonPrefix(v12345, ByteVector.empty)
    val res4   = RadixTree.commonPrefix(v12345, v12367)
    val res5   = RadixTree.commonPrefix(v22345, v12345)
    val res6   = RadixTree.commonPrefix(ByteVector.empty, ByteVector.empty)

    res1 shouldBe (ByteVector(1, 2), ByteVector(3, 4, 5), ByteVector(4, 5))
    res2 shouldBe (ByteVector(1, 2, 3), ByteVector(4, 5), ByteVector.empty)
    res3 shouldBe (ByteVector.empty, ByteVector(1, 2, 3, 4, 5), ByteVector.empty)
    res4 shouldBe (ByteVector(1, 2, 3), ByteVector(4, 5), ByteVector(6, 7))
    res5 shouldBe (ByteVector.empty, ByteVector(2, 2, 3, 4, 5), ByteVector(1, 2, 3, 4, 5))
    res6 shouldBe (ByteVector.empty, ByteVector.empty, ByteVector.empty)
  }

  def createBlakeHash(s: String): Blake2b256Hash = {
    val notEmptyPart = createBV(s)
    val emptyPart    = List.fill(32 - notEmptyPart.size.toInt)(0x00.toByte)
    Blake2b256Hash.fromByteVector(ByteVector(emptyPart) ++ notEmptyPart)
  }

  def createBV(s: String): ByteVector = ByteVector(Base16.unsafeDecode(s))
  def createInsertActions(dataSet: List[radixKV]): List[InsertAction] =
    dataSet.map { ds =>
      InsertAction(ds.rKey.toSeq, ds.rValue)
    }

  case class radixKV(rKey: ByteVector, rValue: Blake2b256Hash)

  object radixKV {
    def apply(strKey: String, strValue: String): radixKV =
      new radixKV(createBV(strKey), createBlakeHash(strValue))
  }

  /*
        key       |   value
     111122334455 | 0000...0001
     11112233AABB | 0000...0002
     1111AABBCC   | 0000...0003
     33           | 0000...0004
     FF0011       | 0000...0005
     FF012222     | 0000...0006
   */
  private val treeDataSet = List(
    radixKV("111122334455", "01"),
    radixKV("11112233AABB", "02"),
    radixKV("1111AABBCC", "03"),
    radixKV("33", "04"),
    radixKV("FF0011", "05"),
    radixKV("FF012222", "06")
  )

  private val referenceLeafPrefixes = treeDataSet.map(_.rKey)
  private val referenceLeafValues   = treeDataSet.map(_.rValue)

  private val referenceInsertActions = createInsertActions(treeDataSet)

  private val exportSettings = ExportDataSettings(
    flagNodePrefixes = true,
    flagNodeKeys = true,
    flagNodeValues = true,
    flagLeafPrefixes = true,
    flagLeafValues = true
  )
  case class ExportParameters(
      rootHash: Blake2b256Hash, // hash
      typedStore: KeyValueTypedStore[Task, Blake2b256Hash, ByteVector],
      takeSize: Int,     // take size
      skipSize: Int,     // skip size
      withSkip: Boolean, // start with skip is true
      exportData: ExportData,
      lastPrefix: Option[ByteVector] // last prefix
  )

  case class MultipageExportResults(
      firstExport: (ExportData, Option[ByteVector]),
      reconstructExport: (ExportData, Option[ByteVector])
  )

  def validateMultipageExport(
      rootHash: Blake2b256Hash,
      store: KeyValueTypedStore[Task, Blake2b256Hash, ByteVector],
      withSkip: Boolean
  ): Task[MultipageExportResults] = {

    def multipageExport(p: ExportParameters): Task[Either[ExportParameters, ExportParameters]] = {
      def collectExportData(prevData: ExportData, pageData: ExportData): ExportData =
        ExportData(
          prevData.nodePrefixes ++ pageData.nodePrefixes,
          prevData.nodeKeys ++ pageData.nodeKeys,
          prevData.nodeValues ++ pageData.nodeValues,
          prevData.leafPrefixes ++ pageData.leafPrefixes,
          prevData.leafValues ++ pageData.leafValues
        )
      for {
        exported <- sequentialExport(
                     rootHash,
                     if (withSkip || p.lastPrefix.get.isEmpty) None else p.lastPrefix,
                     if (withSkip) p.skipSize else 0,
                     p.takeSize,
                     store.get1,
                     exportSettings
                   )
        (pageExportData, _) = exported
        pageKVDBKeys        = pageExportData.nodeKeys
        pageKVDBValues      = pageExportData.nodeValues
        result = ExportParameters(
          rootHash,
          store,
          p.takeSize,
          if (withSkip) p.skipSize + p.takeSize else p.skipSize,
          withSkip,
          collectExportData(p.exportData, pageExportData),
          exported._2
        )
      } yield
        if (pageKVDBKeys.isEmpty && pageKVDBValues.isEmpty) result.asRight
        else result.asLeft
    }

    // Initialize structure for export
    val initSeq        = Seq[ByteVector]()
    val initExportData = ExportData(initSeq, Seq.empty, initSeq, initSeq, Seq.empty)
    val initParameters = ExportParameters(
      rootHash,
      store,
      takeSize = 2,
      skipSize = 0,
      withSkip,
      initExportData,
      Option(ByteVector.empty)
    )
    for {
      allExport        <- initParameters.tailRecM(multipageExport)
      firstExportData  = (allExport.exportData, allExport.lastPrefix)
      (exportData1, _) = firstExportData
      nodeKVDBKeys     = exportData1.nodeKeys
      nodeKVDBValues   = exportData1.nodeValues
      localStorage     = (nodeKVDBKeys zip nodeKVDBValues).toMap

      // Export data from new storage
      reconstructExportData <- sequentialExport(
                                rootHash,
                                None,
                                skipSize = 0,
                                takeSize = 100,
                                x =>
                                  Sync[Task]
                                    .delay(localStorage.get(x)),
                                exportSettings
                              )

      result = MultipageExportResults(firstExportData, reconstructExportData)
    } yield result
  }

  private def withImplAndStore(
      f: (
          RadixTreeImpl[Task],
          InMemoryKeyValueStore[Task]
      ) => Task[Unit]
  ): Unit = {
    val store         = InMemoryKeyValueStore[Task]
    val typedStore    = store.toTypedStore(RadixHistory.codecBlakeHash, scodec.codecs.bytes)
    val radixTreeImpl = new RadixTreeImpl[Task](typedStore)
    f(radixTreeImpl, store).runSyncUnsafe(20.seconds)
  }
};
