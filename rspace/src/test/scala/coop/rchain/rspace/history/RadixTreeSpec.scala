package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.history.TestData._
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.shared.Base16
import coop.rchain.shared.syntax.{sharedSyntaxKeyValueStore, sharedSyntaxKeyValueTypedStore}
import coop.rchain.store.{InMemoryKeyValueStore, KeyValueStore, KeyValueStoreOps}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.concurrent.duration._
import scala.util.Random

class RadixTreeSpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  "appending leaf in empty tree" should "create tree with one node" in withImplAndStore {
    (impl, _) =>
      for {
        item1 <- impl.update(
                  RadixTree.EmptyItem,
                  createBV("FFFFFFF1"),
                  createBV32("A")
                )

        newRootNode    <- impl.constructNodeFromItem(item1.get)
        printedTreeStr <- impl.printTree(newRootNode, "TREE WITH ONE LEAF", noPrintFlag = true)

        referenceTree = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [FF]LEAF: prefix = FFFFF1, data = 0000...000A"
        )

        _ = printedTreeStr shouldBe referenceTree
      } yield ()
  }

  "appending leaf to leaf (leafs contain common prefixes)" should "create 2 leafs with node ptr" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("0001122013"),
        createBV("0001122225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     createBV32("11")
                   )

        rootNode1    <- impl.constructNodeFromItem(item1Opt.get)
        printedTree1 <- impl.printTree(rootNode1, "TREE WITH ONE LEAF", noPrintFlag = true)

        referenceTree1 = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [00]LEAF: prefix = 01122013, data = 0000...0011"
        )

        item2Opt <- impl.update(
                     item1Opt.get,
                     keys(1),
                     createBV32("55")
                   )

        rootNode2 <- impl.constructNodeFromItem(item2Opt.get)
        printedTree2 <- impl.printTree(
                         rootNode2,
                         "TREE WITH ONE NODE AND 2 LEAFS",
                         noPrintFlag = true
                       )

        referenceTree2 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [00]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2
      } yield ()
  }
  "appending leaf with other prefix to leaf" should "create node with two leafs" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("00000000"),
        createBV("34564544")
      )
      for {
        rootItem1Opt <- impl.update(
                         RadixTree.EmptyItem,
                         keys(0),
                         createBV32("0x11")
                       )

        rootItem2Opt <- impl.update(
                         rootItem1Opt.get,
                         keys(1),
                         createBV32("55")
                       )

        rootNode <- impl.constructNodeFromItem(rootItem2Opt.get)

        printedTreeStr <- impl.printTree(rootNode, "TREE: TWO LEAFS", noPrintFlag = true)

        referenceTree = Vector(
          "TREE: TWO LEAFS: root =>",
          "   [00]LEAF: prefix = 000000, data = 0000...0011",
          "   [34]LEAF: prefix = 564544, data = 0000...0055"
        )

        _ = printedTreeStr shouldBe referenceTree
      } yield ()
  }

  "updating leaf" should "update data in this leaf" in withImplAndStore { (impl, _) =>
    val firstLeafData = createBV32("0xCB")
    val newLeafData   = createBV32("0xFF")
    val leafKey       = createBV("00123456F1")
    for {

      //  Create tree with one leaf
      item1Opt <- impl.update(RadixTree.EmptyItem, leafKey, firstLeafData)

      rootNode1 <- impl.constructNodeFromItem(item1Opt.get)
      printedTree1 <- impl.printTree(
                       rootNode1,
                       "TREE WITH ONE LEAF",
                       noPrintFlag = true
                     )

      item2Opt  <- impl.update(item1Opt.get, leafKey, newLeafData)
      _         <- Sync[Task].delay(byteToInt(leafKey.head))
      rootNode2 <- impl.constructNodeFromItem(item2Opt.get)

      printedTree2 <- impl.printTree(
                       rootNode2,
                       "TREE WITH ONE LEAF (AFTER CHANGING DATA)",
                       noPrintFlag = true
                     )

      referenceTree1 = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00CB"
      )

      referenceTree2 = Vector(
        "TREE WITH ONE LEAF (AFTER CHANGING DATA): root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00FF"
      )

      _ = printedTree1 shouldBe referenceTree1
      _ = printedTree2 shouldBe referenceTree2
    } yield ()
  }

  "RadixTreeImpl" should "not allow to enter keys with different lengths in the subtree" in withImplAndStore {
    (impl, _) =>
      val leafData    = createBV32("CB")
      val leafKey     = createBV("00123456F1")
      val testLeafKey = createBV("0112")
      for {
        leafItemOpt <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
        err         <- impl.update(leafItemOpt.get, testLeafKey, leafData).attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: The length of all prefixes in the subtree must be the same."
      }
  }

  "RadixTreeImpl" should "not allow to radix key smaller than NodePtr key" in withImplAndStore {
    (impl, _) =>
      for {
        err <- impl
                .update(
                  NodePtr(ByteVector(0x00, 0x11), createBV32("00")),
                  createBV("00"),
                  createBV32("33")
                )
                .attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: Radix key should be longer than NodePtr key."
      }
  }

  "deleting non - existent data" should "return none" in withImplAndStore { (impl, _) =>
    val leafData = createBV32("CC")
    val leafKey  = createBV("00123456F1")
    for {
      //  Create tree with one node
      itemOpt <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
      _       <- impl.constructNodeFromItem(itemOpt.get)

      //  Trying to delete not existing leaf...
      del <- impl.delete(itemOpt.get, createBV("0000").tail)

      _ = del.map(item => item shouldBe None)
    } yield ()
  }

  "deleting leaf from tree with only one leaf" should "destroy tree" in withImplAndStore {
    (impl, _) =>
      val leafData = createBV32("CC")
      val leafKey  = createBV("00123456F1")
      for {
        //  Create tree with one node
        itemOpt   <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode1 <- impl.constructNodeFromItem(itemOpt.get)

        //  Trying to delete not existing leaf...
        deletedItem         <- impl.delete(itemOpt.get, leafKey)
        rootNode2           = rootNode1.updated(leafKey.head.toInt, deletedItem.get)
        printedEmptyTreeStr <- impl.printTree(rootNode2, "EMPTY TREE", noPrintFlag = true)

        referenceEmptyTreeStr = Vector("EMPTY TREE: root =>")

        _ = deletedItem.map(item => item shouldBe RadixTree.EmptyItem)
        _ = referenceEmptyTreeStr shouldBe printedEmptyTreeStr
      } yield ()
  }

  "deleting leaf from node with two leafs" should "leave one leaf" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("00000000"),
        createBV("34564544")
      )

      val rootItem1Hash = createBV32("11")
      val rootItem2Hash = createBV32("AF")
      for {
        rootItem1Opt <- impl.update(
                         RadixTree.EmptyItem,
                         keys(0),
                         rootItem1Hash
                       )

        rootItem2Opt <- impl.update(
                         rootItem1Opt.get,
                         keys(1),
                         rootItem2Hash
                       )

        rootNode1 <- impl.constructNodeFromItem(rootItem2Opt.get)
        printedTree1 <- impl.printTree(
                         rootNode1,
                         "TREE: TWO LEAFS (BEFORE DELETING)",
                         noPrintFlag = true
                       )

        itemIdx <- Sync[Task].delay(byteToInt(keys(0).head))

        itemToDelete = rootNode1(itemIdx)
        deletedItem  <- impl.delete(itemToDelete, keys(0).tail)
        rootNode2    = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 <- impl.printTree(
                         rootNode2,
                         "TREE: TWO LEAFS (AFTER DELETING)",
                         noPrintFlag = true
                       )

        referenceTree1 = Vector(
          "TREE: TWO LEAFS (BEFORE DELETING): root =>",
          "   [00]LEAF: prefix = 000000, data = 0000...0011",
          "   [34]LEAF: prefix = 564544, data = 0000...00AF"
        )

        referenceTree2 = Vector(
          "TREE: TWO LEAFS (AFTER DELETING): root =>",
          "   [34]LEAF: prefix = 564544, data = 0000...00AF"
        )
        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2
      } yield ()
  }

  "deleting one leaf from a child node containing two leafs" should "child node and reformat parent node" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("FA01122013"),
        createBV("FA01122225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     createBV32("11")
                   )
        item2Opt <- impl.update(
                     item1Opt.get,
                     keys(1),
                     createBV32("55")
                   )

        rootNode1 <- impl.constructNodeFromItem(item2Opt.get)
        printedTree1 <- impl.printTree(
                         rootNode1,
                         "TREE WITH ONE NODE AND 2 LEAFS",
                         noPrintFlag = true
                       )

        referenceTree1 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [FA]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        itemIdx     <- Sync[Task].delay(byteToInt(keys(0).head))
        deletedItem <- impl.delete(rootNode1(0xFA), keys(0).tail)
        rootNode2   = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 <- impl.printTree(rootNode2, "TREE (AFTER DELETE)", noPrintFlag = true)

        referenceTree2 = Vector(
          "TREE (AFTER DELETE): root =>",
          "   [FA]LEAF: prefix = 01122225, data = 0000...0055"
        )

        _ = printedTree1 shouldBe referenceTree1
        _ = printedTree2 shouldBe referenceTree2

      } yield ()
  }

  "reading data from existing node" should "return data" in withImplAndStore { (impl, _) =>
    val itemData = createBV32("CB")
    val key      = createBV("0123456F1")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      readDataOpt <- impl.read(rootNode, key)

      _ = readDataOpt.get shouldBe itemData
    } yield ()
  }

  "reading non - existent data" should "return none" in withImplAndStore { (impl, _) =>
    val itemData = createBV32("CB")
    val key      = createBV("0123456F1")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      notExistingKey = createBV("0000")
      readDataOpt    <- impl.read(rootNode, notExistingKey)

      _ = readDataOpt shouldBe none

    } yield ()
  }

  "function saveNode" should "put node into store" in withImplAndStore { (impl, inMemoStore) =>
    for {
      nodesCount1 <- Sync[Task].delay(inMemoStore.numRecords())
      _           = impl.saveNode(emptyNode)
      _           <- impl.commit

      //  After saving node numRecords must return 1
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

  // Data for test are given from RadixTree specification
  "encode and decode" should "give initial node" in {
    val leaf = Leaf(
      createBV("FFFF"),
      createBV32("0000000000000000000000000000000000000000000000000000000000000001")
    )
    val nodePtr = NodePtr(
      createBV(""),
      createBV("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
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

  "collisions in KVDB" should "be detected" in withImplAndStore { (impl, inMemoStore) =>
    def copyBVToBuf(bv: ByteVector): ByteBuffer = {
      val arr    = bv.toArray
      val newBuf = ByteBuffer.allocateDirect(arr.length)
      newBuf.put(arr).rewind()
    }

    val collisionKVPair = (copyBVToBuf(History.emptyRootHash.bytes), ByteVector(0x00))
    for {
      _   <- inMemoStore.put[ByteVector](Seq(collisionKVPair), copyBVToBuf)
      _   = impl.saveNode(emptyNode)
      err <- impl.commit.attempt

    } yield {
      err.isLeft shouldBe true
      val ex = err.left.get
      ex shouldBe a[RuntimeException]
      ex.getMessage shouldBe
        s"1 collisions in KVDB (first collision with key = " +
          s"${History.emptyRootHash.bytes.toHex})."
    }
  }

  "tree with makeActions" should "be built correctly and not create artefacts in KV - store" in withImplAndStore {
    (impl, inMemoStore) =>
      val keysAndData = List(
        ("111122334455", "01"),
        ("11112233AABB", "02"),
        ("1111AABBCC", "03"),
        ("33", "04"),
        ("FF0011", "05"),
        ("FF012222", "06")
      )

      val insertActions = createInsertActions(keysAndData)
      val deleteActions = createDeleteActions(keysAndData.map {
        case (key, _) => key
      })
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
        //  1  Build a tree according to the example in specification
        rootNode1Opt <- impl.makeActions(RadixTree.emptyNode, insertActions)

        //    Get the tree for compare with reference
        tree1 <- impl.printTree(rootNode1Opt.get, "TREE1", noPrintFlag = true)

        _ = impl.saveNode(rootNode1Opt.get)
        _ <- impl.commit

        //  Number of nodes must be equal to 4 (with root)
        nodesCount1 = inMemoStore.numRecords()

        //  2   Delete all data from tree...
        rootNode2Opt <- impl.makeActions(rootNode1Opt.get, deleteActions)

        tree2 <- impl.printTree(rootNode2Opt.get, "TREE2", noPrintFlag = true)
        _     = impl.saveNode(rootNode2Opt.get)
        _     <- impl.commit

        //  Number of nodes after deleting data must be equal to 5 (with root)
        nodesCount2 = inMemoStore.numRecords()

        _ = tree1 shouldBe referenceTree1
        _ = nodesCount1 shouldBe 4
        _ = tree2 shouldBe referenceTree2
        _ = nodesCount2 shouldBe 5
      } yield ()
  }

  "sequentialExport" should "export all data from tree" in withImplAndStore { (impl, store) =>
    val leafKeysAndValues = List(
      ("111122334455", "01"),
      ("11112233AABB", "02"),
      ("1111AABBCC", "03"),
      ("33", "04"),
      ("FF0011", "05"),
      ("FF012222", "06")
    )
    val insertActions = createInsertActions(leafKeysAndValues)
    val referenceLeafPrefixes =
      leafKeysAndValues.map { case (prefix, _) => createBV(prefix) }
    val referenceLeafValues = leafKeysAndValues.map { case (_, value) => createBV32(value) }

    val exportSettings = ExportDataSettings(
      flagNodePrefixes = true,
      flagNodeKeys = true,
      flagNodeValues = true,
      flagLeafPrefixes = true,
      flagLeafValues = true
    )
    for {
      //  Create tree with 6 leafs
      rootNode2Opt <- impl.makeActions(emptyNode, insertActions)
      hash         = rootNode2Opt.map(node => impl.saveNode(node))
      _            <- impl.commit

      //  First data export
      typedStore       = store.toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
      exported1        <- sequentialExport(hash.get, None, 0, 100, typedStore.get1, exportSettings)
      (exportData1, _) = exported1

      //  Create new storage
      (
        ExportData(_, nodeKVDBKeys, nodeKVDBValues, _, _),
        _
      ) = exported1

      localStorage = nodeKVDBKeys zip nodeKVDBValues
      newLocalStore = InMemoryKeyValueStore[Task]
        .toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
      _ <- newLocalStore.put(localStorage)

      //  Export data from new storage
      exported2 <- {
        sequentialExport(
          hash.get,
          None,
          0,
          100,
          newLocalStore.get1,
          exportSettings
        )
      }

      //  Data exported from created storage must me equal to data from source store
      _ = {
        assert(exported2 == exported1, "Error of validation")
        ()
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

  def createBV32(s: String): ByteVector = {
    val notEmptyPart = createBV(s)
    val emptyPart    = List.fill(32 - notEmptyPart.size.toInt)(0).map(_.toByte)
    ByteVector(emptyPart) ++ notEmptyPart
  }

  def createBV(s: String): ByteVector = ByteVector(Base16.unsafeDecode(s))
  def createInsertActions(
      tuplesKeyAndHash: List[(String, String)]
  ): List[InsertAction] =
    tuplesKeyAndHash.map {
      case (key, data) =>
        InsertAction(createBV(key).toSeq, Blake2b256Hash.fromByteVector(createBV32(data)))
    }

  def createDeleteActions(keys: List[String]): List[DeleteAction] =
    keys.map(key => DeleteAction(createBV(key).toSeq))

  protected def withImplAndStore(
      f: (
          RadixTreeImpl[Task],
          InMemoryKeyValueStore[Task]
      ) => Task[Unit]
  ): Unit = {
    val store         = InMemoryKeyValueStore[Task]
    val typedStore    = store.toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
    val radixTreeImpl = new RadixTreeImpl[Task](typedStore)
    f(radixTreeImpl, store).runSyncUnsafe(20.seconds)
  }
};
