package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree.{
  byteToInt,
  sequentialExport,
  ExportDataSettings,
  NodePtr,
  RadixTreeImpl
}
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
                  createBV32(0xA.toByte)
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

  "appending leaf to tree with one leaf " should "create 2 leafs with node ptr" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("0001122013"),
        createBV("0001122225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     createBV32(0x11.toByte)
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
                     createBV32(0x55.toByte)
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
  "appending leaf to leaf" should "create node with two leafs" in withImplAndStore { (impl, _) =>
    val keys = Vector[ByteVector](
      createBV("00000000"),
      createBV("34564544")
    )
    for {
      rootItem1Opt <- impl.update(
                       RadixTree.EmptyItem,
                       keys(0),
                       createBV32(0x11.toByte)
                     )

      rootItem2Opt <- impl.update(
                       rootItem1Opt.get,
                       keys(1),
                       createBV32(0x55.toByte)
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
    val firstLeafData = createBV32(0xCB.toByte)
    val newLeafData   = createBV32(0xFF.toByte)
    val leafKey       = createBV("0123456F1")
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
      val leafData    = createBV32(0xCB.toByte)
      val leafKey     = createBV("0123456F1")
      val testLeafKey = createBV("112")
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
                  NodePtr(ByteVector(0x00, 0x11), createBV32(0x00.toByte)),
                  TestData.hexKey("0"),
                  createBV32(0x33.toByte)
                )
                .attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: Radix key should be longer than NodePtr key."
      }
  }

  "deleting not existent data" should "return none" in withImplAndStore { (impl, _) =>
    val leafData = createBV32(0xCC.toByte)
    val leafKey  = createBV("0123456F1")
    for {
      //  Create tree with one node
      itemOpt  <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      //  Trying to delete not existing leaf...
      del <- impl.delete(itemOpt.get, createBV("000").tail)

      _ = del.map(item => item shouldBe None)
    } yield ()
  }

  "deleting leaf from tree with only one leaf" should "destroy tree" in withImplAndStore {
    (impl, _) =>
      val leafData = createBV32(0xCC.toByte)
      val leafKey  = createBV("0123456F1")
      for {
        //  Create tree with one node
        itemOpt   <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode1 <- impl.constructNodeFromItem(itemOpt.get)

        //  Trying to delete not existing leaf...
        deletedItem         <- impl.delete(itemOpt.get, leafKey)
        rootNode2           = rootNode1.updated(leafKey.head.toInt, deletedItem.get)
        printedEmptyTreeStr <- impl.printTree(rootNode2, "EMPTY TREE", noPrintFlag = true)

        emptyTreeStr = Vector("EMPTY TREE: root =>")

        _ = deletedItem.map(item => item shouldBe RadixTree.EmptyItem)
        _ = emptyTreeStr shouldBe printedEmptyTreeStr
      } yield ()
  }

  "deleting leaf from node with two leafs" should "leave one leaf" in withImplAndStore {
    (impl, _) =>
      val keys = Vector[ByteVector](
        createBV("00000000"),
        createBV("34564544")
      )

      val rootItem1Hash = createBV32(0x11.toByte)
      val rootItem2Hash = createBV32(0xAF.toByte)
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

  "deleting data from leaf" should "destroy this leaf" in withImplAndStore { (impl, _) =>
    val ptrPrefix                = "FA"
    val commonKeyPartForTwoLeafs = ptrPrefix + "01122"
    val keys = Vector[ByteVector](
      createBV(commonKeyPartForTwoLeafs + "013"),
      createBV(commonKeyPartForTwoLeafs + "225")
    )
    for {
      item1Opt <- impl.update(
                   RadixTree.EmptyItem,
                   keys(0),
                   createBV32(0x11.toByte)
                 )
      item2Opt <- impl.update(
                   item1Opt.get,
                   keys(1),
                   createBV32(0x55.toByte)
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
    val itemData = createBV32(0xCB.toByte)
    val key      = createBV("0123456F1")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      readDataOpt <- impl.read(rootNode, key)

      _ = readDataOpt.map(_.toArray shouldBe itemData.toArray)

    } yield ()
  }

  "reading non - existent data" should "return none" in withImplAndStore { (impl, _) =>
    val itemData = createBV32(0xCB.toByte)
    val key      = createBV("0123456F1")
    for {
      itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      notExistingKey = createBV("000")
      readDataOpt    <- impl.read(rootNode, notExistingKey)

      _ = readDataOpt.map(_ shouldBe none)

    } yield ()
  }

  "function saveNode" should "put node into store" in withImplAndStore { (impl, inMemoStore) =>
    val itemData = createBV32(0xCB.toByte)
    val key      = createBV("0123456F1")
    for {
      itemOpt     <- impl.update(RadixTree.EmptyItem, key, itemData)
      nodesCount1 = inMemoStore.numRecords()

      node <- impl.constructNodeFromItem(itemOpt.get)
      _    = impl.saveNode(node)
      _    <- impl.commit

      //  After saving node numRecords must return 1
      nodesCount2 = inMemoStore.numRecords()
      _           = nodesCount1 shouldBe 0
      _           = nodesCount2 shouldBe 1
    } yield ()
  }

  "encode and decode" should "give initial node" in withImplAndStore { (impl, _) =>
    for {
      item1 <- impl.update(
                RadixTree.EmptyItem,
                createBV("FFF8AFF1"),
                createBV32(0xAD.toByte)
              )

      node <- impl.constructNodeFromItem(item1.get)

      serializedNode = RadixTree.Codecs.encode(node)

      deserializedNode = RadixTree.Codecs.decode(serializedNode)

      referenceString = "ByteVector(37 bytes, 0xff03f8aff100000000000000000000000000000000000000000000000000000000000000ad)"

      _ = deserializedNode shouldBe node
      _ = serializedNode.toString() shouldBe referenceString
    } yield ()
  }

  "collisions in KVDB" should "be detected" in withImplAndStore { (impl, inMemoStore) =>
    def copyBVToBuf(bv: ByteVector): ByteBuffer = {
      val arr    = bv.toArray
      val newBuf = ByteBuffer.allocateDirect(arr.length)
      newBuf.put(arr).rewind()
    }

    val insertRecord    = createInsertActions(List(("FF00FFF01", 0xAA.toByte)))
    val deleteRecord    = createDeleteActions(List("FF00FFF01"))
    val collisionKVPair = (copyBVToBuf(History.emptyRootHash.bytes), insertRecord.head.hash.bytes)
    for {
      rootNode <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

      //  process
      newRootNodeOpt1 <- impl.makeActions(rootNode, insertRecord)

      _ <- newRootNodeOpt1.traverse { newRootNode =>
            val hash      = impl.saveNode(newRootNode)
            val blakeHash = Blake2b256Hash.fromByteVector(hash)
            impl.commit.as(blakeHash)
          }
      _ = impl.clearWriteCache()

      _ <- inMemoStore.put[ByteVector](Seq(collisionKVPair), copyBVToBuf)

      newRootNodeOpt2 <- impl.makeActions(newRootNodeOpt1.get, deleteRecord)
      _               = newRootNodeOpt2.map(node => impl.saveNode(node))
      err             <- impl.commit.attempt

    } yield {
      err.isLeft shouldBe true
      val ex = err.left.get
      ex shouldBe a[RuntimeException]
      ex.getMessage shouldBe
        s"1 collisions in KVDB (first collision with key = " +
          s"${History.emptyRootHash.bytes.toHex})."

      println(ex.getMessage)
    }
  }

  "tree with makeActions" should "be built correctly" in withImplAndStore { (impl, _) =>
    for {
      rootNode <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

      keysAndData = List(
        ("FF00FFF01", 0xA.toByte),
        ("FF0000201", 0xB.toByte),
        ("FF002111", 0x1.toByte),
        ("FF002112", 0x2.toByte)
      )

      insertActions = createInsertActions(keysAndData)

      newRootNodeOpt <- impl.makeActions(rootNode, insertActions)
      treeInfo       <- impl.printTree(newRootNodeOpt.get, "TREE1", noPrintFlag = true)

      referenceTree = Vector(
        "TREE1: root =>",
        "   [0F]PTR: prefix = F0, ptr =>",
        "      [00]LEAF: prefix = 0201, data = 0000...000B",
        "      [0F]LEAF: prefix = FF01, data = 0000...000A",
        "   [FF]PTR: prefix = 0021, ptr =>",
        "      [11]LEAF: prefix = empty, data = 0000...0001",
        "      [12]LEAF: prefix = empty, data = 0000...0002"
      )

      _ = treeInfo shouldBe referenceTree
    } yield ()
  }

  "function makeActions" should "not create artefacts" in withImplAndStore { (impl, inMemoStore) =>
    val insertActions = createInsertActions(
      List(("FF00FFF01", 0xAA.toByte), ("FF0012345", 0x11.toByte), ("FF00F5676", 0x16.toByte))
    )
    for {
      rootNode1 <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

      rootNode2Opt <- impl.makeActions(rootNode1, insertActions)

      //  Root node is also saved in store
      _ = rootNode2Opt.map(rootNode => impl.saveNode(rootNode))
      _ <- impl.commit

      _ = inMemoStore.numRecords() shouldBe 3

    } yield ()
  }

  "function makeActions in work with non-empty tree" should "not create artefacts" in withImplAndStore {
    (impl, inMemoStore) =>
      val insertFirstNodesActions = createInsertActions(
        List(
          ("111122334455", 0xAA.toByte),
          ("11112233AABB", 0x11.toByte),
          ("1111AABBCC", 0x16.toByte)
        )
      )

      val insertLastNodesActions = createInsertActions(
        List(
          ("33", 0x11.toByte),
          ("FF0011", 0x22.toByte),
          ("FF012222", 0x33.toByte)
        )
      )
      for {
        rootNode1 <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

        //  Create tree with 3 leafs
        rootNode2Opt <- impl.makeActions(rootNode1, insertFirstNodesActions)
        _            <- impl.commit

        nodesCount1 = inMemoStore.numRecords()
        _ = println(
          s"Nodes count after creating tree of 3 leafs (without root node) : ${nodesCount1.toString}"
        )

        //  Append in existing tree 3 leafs...
        rootNode3Opt <- impl.makeActions(rootNode2Opt.get, insertLastNodesActions)
        _            <- impl.commit
        nodesCount2  = inMemoStore.numRecords()
        _ = println(
          s"Nodes count after appending ${insertLastNodesActions.size.toString} leafs (without root node): ${nodesCount2.toString}"
        )

        _ = nodesCount1 shouldBe 2
        _ = nodesCount2 shouldBe 3
      } yield ()
  }

  "sequentialExport" should "export all data from tree" in withImplAndStore { (impl, store) =>
    val insertFirstNodesActions = createInsertActions(
      List(
        ("111122334455", 0xAA.toByte),
        ("11112233AABB", 0x11.toByte),
        ("1111AABBCC", 0x16.toByte),
        ("33", 0x11.toByte),
        ("FF0011", 0x22.toByte),
        ("FF012222", 0x33.toByte)
      )
    )

    val exportSettings = ExportDataSettings(
      flagNodePrefixes = false,
      flagNodeKeys = true,
      flagNodeValues = true,
      flagLeafPrefixes = false,
      flagLeafValues = true
    )

    for {
      rootNode1 <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

      //  Create tree with 3 leafs
      rootNode2Opt <- impl.makeActions(rootNode1, insertFirstNodesActions)
      hash         = rootNode2Opt.map(node => impl.saveNode(node))
      _            <- impl.commit

      store1          = store.toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
      exported        <- sequentialExport(hash.get, None, 0, 100, store1.get1, exportSettings)
      (exportData, _) = exported

      (nodeKeys, nodeValues, leafValues) = (
        exportData.nodeKeys.map(_.toString()),
        exportData.nodeValues.map(_.toString()),
        exportData.leafValues.map(_.toString())
      )

      referenceNodeKeys = Seq[String](
        "ByteVector(32 bytes, 0x1f7deb90cf44a7576aae69f02b9c65224354d78502d3c4300b94b8e93078d6b0)",
        "ByteVector(32 bytes, 0x8d00dee837d7af0e85cd675b0ed5b132717308a50b5d8a05b13e3e67004f7c0d)",
        "ByteVector(32 bytes, 0xeff9110a5f977dec456f165c9d8ff334d59be461b469ca689914a14d35107d39)",
        "ByteVector(32 bytes, 0x4bb180aee719cd3b55eee21071ed535c3769035ad5a124704713815bc097dac9)"
      )

      referenceNodeValues = Seq[String](
        "ByteVector(103 bytes, 0x1181118d00dee837d7af0e85cd675b0ed5b132717308a50b5d8a05b13e3e67004f7c0d33000000000000000000000000000000000000000000000000000000000000000011ff804bb180aee719cd3b55eee21071ed535c3769035ad5a124704713815bc097dac9)",
        "ByteVector(71 bytes, 0x228133eff9110a5f977dec456f165c9d8ff334d59be461b469ca689914a14d35107d39aa02bbcc0000000000000000000000000000000000000000000000000000000000000016)",
        "ByteVector(70 bytes, 0x44015500000000000000000000000000000000000000000000000000000000000000aaaa01bb0000000000000000000000000000000000000000000000000000000000000011)",
        "ByteVector(71 bytes, 0x0001110000000000000000000000000000000000000000000000000000000000000022010222220000000000000000000000000000000000000000000000000000000000000033)"
      )

      referenceLeafValues = Seq[String](
        "ByteVector(32 bytes, 0x00000000000000000000000000000000000000000000000000000000000000aa)",
        "ByteVector(32 bytes, 0x0000000000000000000000000000000000000000000000000000000000000011)",
        "ByteVector(32 bytes, 0x0000000000000000000000000000000000000000000000000000000000000016)",
        "ByteVector(32 bytes, 0x0000000000000000000000000000000000000000000000000000000000000011)",
        "ByteVector(32 bytes, 0x0000000000000000000000000000000000000000000000000000000000000022)",
        "ByteVector(32 bytes, 0x0000000000000000000000000000000000000000000000000000000000000033)"
      )

      _ = exportData.nodeKeys.size shouldBe 4
      _ = nodeKeys shouldBe referenceNodeKeys
      _ = nodeValues shouldBe referenceNodeValues
      _ = leafValues shouldBe referenceLeafValues

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

  def createBV32(lastByte: Byte): ByteVector =
    (List.fill(31)(0) ++ List.fill(1)(lastByte.toInt)).map(_.toByte).toVector

  def createBV(s: String): ByteVector = ByteVector(Base16.unsafeDecode(s))
  def createInsertActions(
      tuplesKeyAndHash: List[(String, Byte)]
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
