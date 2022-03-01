package coop.rchain.rspace.history

import cats.{Functor, Parallel}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree.{
  byteToInt,
  commonPrefix,
  ExportDataSettings,
  Item,
  NodePtr,
  RadixTreeImpl
}
import coop.rchain.rspace.history.TestData._
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.shared.Base16
import coop.rchain.shared.syntax.sharedSyntaxKeyValueStore
import coop.rchain.store.{
  InMemoryKeyValueStore,
  KeyValueStore,
  KeyValueStoreOps,
  KeyValueTypedStore
}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.concurrent.duration._
import scala.util.Random

class RadixTreeTests extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  def generateDataWithLastNonZeroByte(lastByte: Byte): Array[Byte] =
    (List.fill(31)(0) ++ List.fill(1)(lastByte.toInt)).map(_.toByte).toArray

  "tree with makeActions" should "be built correctly!!!" in withRadixTreeImplAndInMemoStore {
    (impl, typedStore) =>
      for {
        rootNode <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

        keys = List(
          TestData.hexKey("FF00FFF01"),
          TestData.hexKey("FF0000201"),
          TestData.hexKey("FF002111"),
          TestData.hexKey("FF002112")
        )

        lastByteForHashes = List[Byte](0xA, 0xB, 0x1, 0x2)

        //  List
        dataForHashes = lastByteForHashes.map(byte => generateDataWithLastNonZeroByte(byte))
        hashesBlake   = dataForHashes.map(hash => Blake2b256Hash.fromByteArray(hash))

        insertActions2 = InsertAction(keys(0), hashesBlake(0)) ::
          InsertAction(keys(1), hashesBlake(1)) ::
          InsertAction(keys(2), hashesBlake(2)) ::
          InsertAction(keys(3), hashesBlake(3)) :: Nil

        newRootNodeOpt <- impl.makeActions(rootNode, insertActions2)
        treeInfo       <- impl.printTree(newRootNodeOpt.get, "TREE1", false)

        etalonTree = Vector(
          "TREE1: root =>",
          "   [0F]PTR: prefix = F0, ptr =>",
          "      [00]LEAF: prefix = 0201, data = 0000...000B",
          "      [0F]LEAF: prefix = FF01, data = 0000...000A",
          "   [FF]PTR: prefix = 0021, ptr =>",
          "      [11]LEAF: prefix = empty, data = 0000...0001",
          "      [12]LEAF: prefix = empty, data = 0000...0002"
        )

        _ = treeInfo shouldBe etalonTree
      } yield ()
  }

  "appending leaf in empty tree" should "work" in withRadixTreeImplAndInMemoStore { (impl, _) =>
    for {
      item1 <- impl.update(
                RadixTree.EmptyItem,
                TestData.hexKey("FFFFFFF1").toVector,
                generateDataWithLastNonZeroByte(0xA.toByte).toVector
              )

      newRootNode    <- impl.constructNodeFromItem(item1.get)
      printedTreeStr <- impl.printTree(newRootNode, "TREE WITH ONE LEAF", false)

      etalonTree = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [FF]LEAF: prefix = FFFFF1, data = 0000...000A"
      )

      _ = printedTreeStr shouldBe etalonTree
    } yield ()
  }

  "appending leaf to tree with one leaf " should "create 2 leafs with node ptr" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val commonKeyPartForTwoLeafs = "001122"
      val keys = Vector[Seq[Byte]](
        TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
        TestData.hexKey(commonKeyPartForTwoLeafs + "225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     generateDataWithLastNonZeroByte(0x11.toByte).toVector
                   )

        rootNode1    <- impl.constructNodeFromItem(item1Opt.get)
        printedTree1 <- impl.printTree(rootNode1, "TREE WITH ONE LEAF", false)

        etalonTree1 = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [00]LEAF: prefix = 01122013, data = 0000...0011"
        )

        item2Opt <- impl.update(
                     item1Opt.get,
                     keys(1),
                     generateDataWithLastNonZeroByte(0x55.toByte).toVector
                   )

        rootNode2    <- impl.constructNodeFromItem(item2Opt.get)
        printedTree2 <- impl.printTree(rootNode2, "TREE WITH ONE NODE AND 2 LEAFS", false)

        etalonTree2 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [00]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        _ = printedTree1 shouldBe etalonTree1
        _ = printedTree2 shouldBe etalonTree2
      } yield ()
  }
  "appending leaf to leaf" should "create node with two leafs" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val keys = Vector[Seq[Byte]](
        TestData.hexKey("00000000"),
        TestData.hexKey("34564544")
      )
      for {
        rootItem1Opt <- impl.update(
                         RadixTree.EmptyItem,
                         keys(0),
                         generateDataWithLastNonZeroByte(0x11.toByte).toVector
                       )

        rootItem2Opt <- impl.update(
                         rootItem1Opt.get,
                         keys(1),
                         generateDataWithLastNonZeroByte(0x55.toByte).toVector
                       )

        rootNode <- impl.constructNodeFromItem(rootItem2Opt.get)

        printedTreeStr <- impl.printTree(rootNode, "TREE: TWO LEAFS", false)

        etalonTree = Vector(
          "TREE: TWO LEAFS: root =>",
          "   [00]LEAF: prefix = 000000, data = 0000...0011",
          "   [34]LEAF: prefix = 564544, data = 0000...0055"
        )

        _ = printedTreeStr shouldBe etalonTree
      } yield ()
  }

  "updating leaf" should "work correctly" in withRadixTreeImplAndInMemoStore { (impl, _) =>
    val firstLeafData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
    val newLeafData   = generateDataWithLastNonZeroByte(0xFF.toByte).toVector
    val leafKey       = TestData.hexKey("0123456F1").toVector
    for {

      //  Create tree with one leaf
      item1Opt <- impl.update(RadixTree.EmptyItem, leafKey, firstLeafData)

      rootNode1 <- impl.constructNodeFromItem(item1Opt.get)
      printedTree1 <- impl.printTree(
                       rootNode1,
                       "TREE WITH ONE LEAF",
                       false
                     )

      item2Opt  <- impl.update(item1Opt.get, leafKey, newLeafData)
      itemIdx   <- Sync[Task].delay(byteToInt(leafKey.head))
      rootNode2 <- impl.constructNodeFromItem(item2Opt.get)

      printedTree2 <- impl.printTree(
                       rootNode2,
                       "TREE WITH ONE LEAF (AFTER CHANGING DATA)",
                       false
                     )

      etalonTree1 = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00CB"
      )

      etalonTree2 = Vector(
        "TREE WITH ONE LEAF (AFTER CHANGING DATA): root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00FF"
      )

      _ = printedTree1 shouldBe etalonTree1
      _ = printedTree2 shouldBe etalonTree2
    } yield ()
  }

  "RadixTreeImpl" should "not allow to enter keys with different lengths" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val leafData    = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
      val leafKey     = TestData.hexKey("0123456F1").toVector
      val testLeafKey = TestData.hexKey("112").toVector
      for {
        leafItemOpt <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
        err         <- impl.update(leafItemOpt.get, testLeafKey, leafData).attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: All Radix keys should be same length."
      }
  }

  "RadixTreeImpl" should "not allow to radix key is smaller than NodePtr key" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val commonKeyPartForTwoLeafs = "121122"
      val keys = Vector[Seq[Byte]](
        TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
        TestData.hexKey(commonKeyPartForTwoLeafs + "225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     generateDataWithLastNonZeroByte(0x11.toByte).toVector
                   )
        item2Opt <- impl.update(
                     item1Opt.get,
                     keys(1),
                     generateDataWithLastNonZeroByte(0x55.toByte).toVector
                   )

        newRootNode    <- impl.constructNodeFromItem(item2Opt.get)
        printedTreeStr <- impl.printTree(newRootNode, "TREE WITH ONE NODE AND 2 LEAFS", false)

        rootNodeItem = newRootNode(1)
        err <- impl
                .update(
                  rootNodeItem,
                  TestData.hexKey("0"),
                  generateDataWithLastNonZeroByte(0x33.toByte).toVector
                )
                .attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: Radix key should be longer than NodePtr key."
      }
  }

  "deleting not exising data" should "return none" in withRadixTreeImplAndInMemoStore { (impl, _) =>
    val leafData = generateDataWithLastNonZeroByte(0xCC.toByte).toVector
    val leafKey  = TestData.hexKey("0123456F1").toVector
    for {
      //  Create tree with one node
      itemOpt  <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
      rootNode <- impl.constructNodeFromItem(itemOpt.get)

      printedTreeStr <- impl.printTree(rootNode, "TREE (TEST DELETE NOT EXISTING LEAF)", false)

      //  Trying to delete not existing leaf...
      del <- impl.delete(itemOpt.get, TestData.hexKey("000").toVector.tail)

      _ = del.map(item => item shouldBe None)
    } yield ()
  }

  "deleting leaf from tree with only one leaf" should "destroy tree" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val leafData = generateDataWithLastNonZeroByte(0xCC.toByte).toVector
      val leafKey  = TestData.hexKey("0123456F1").toVector
      for {
        //  Create tree with one node
        itemOpt   <- impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode1 <- impl.constructNodeFromItem(itemOpt.get)

        printedTreeStr <- impl.printTree(rootNode1, "TREE (TEST DELETING ONE LEAF)", false)

        //  Trying to delete not existing leaf...
        deletedItem         <- impl.delete(itemOpt.get, leafKey)
        rootNode2           = rootNode1.updated((leafKey.head).toInt, deletedItem.get)
        printedEmptyTreeStr <- impl.printTree(rootNode2, "EMPTY TREE", false)

        emptyTreeStr = Vector("EMPTY TREE: root =>")

        _ = deletedItem.map(item => item shouldBe RadixTree.EmptyItem)
        _ = emptyTreeStr shouldBe printedEmptyTreeStr
      } yield ()
  }

  "deleting leaf from node with two leafs" should "leave one leaf" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val keys = Vector[Seq[Byte]](
        TestData.hexKey("00000000"),
        TestData.hexKey("34564544")
      )

      val rootItem1Hash = generateDataWithLastNonZeroByte(0x11.toByte).toVector
      val rootItem2Hash = generateDataWithLastNonZeroByte(0xAF.toByte).toVector
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

        rootNode1    <- impl.constructNodeFromItem(rootItem2Opt.get)
        printedTree1 <- impl.printTree(rootNode1, "TREE: TWO LEAFS (BEFORE DELETING)", false)

        itemIdx <- Sync[Task].delay(byteToInt(keys(0).head))

        itemToDelete = rootNode1(itemIdx)
        deletedItem  <- impl.delete(itemToDelete, keys(0).tail)
        rootNode2    = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 <- impl.printTree(
                         rootNode2,
                         "TREE: TWO LEAFS (AFTER DELETING)",
                         false
                       )

        etalonTree1 = Vector(
          "TREE: TWO LEAFS (BEFORE DELETING): root =>",
          "   [00]LEAF: prefix = 000000, data = 0000...0011",
          "   [34]LEAF: prefix = 564544, data = 0000...00AF"
        )

        etalonTree2 = Vector(
          "TREE: TWO LEAFS (AFTER DELETING): root =>",
          "   [34]LEAF: prefix = 564544, data = 0000...00AF"
        )
        _ = printedTree1 shouldBe etalonTree1
        _ = printedTree2 shouldBe etalonTree2
      } yield ()
  }

  "deleting data from leaf" should "destroy this leaf" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val ptrPrefix                = "FA"
      val commonKeyPartForTwoLeafs = ptrPrefix + "01122"
      val keys = Vector[Seq[Byte]](
        TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
        TestData.hexKey(commonKeyPartForTwoLeafs + "225")
      )
      for {
        item1Opt <- impl.update(
                     RadixTree.EmptyItem,
                     keys(0),
                     generateDataWithLastNonZeroByte(0x11.toByte).toVector
                   )
        item2Opt <- impl.update(
                     item1Opt.get,
                     keys(1),
                     generateDataWithLastNonZeroByte(0x55.toByte).toVector
                   )

        rootNode1    <- impl.constructNodeFromItem(item2Opt.get)
        printedTree1 <- impl.printTree(rootNode1, "TREE WITH ONE NODE AND 2 LEAFS", false)

        etalonTree1 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [FA]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        itemIdx     <- Sync[Task].delay(byteToInt(keys(0).head))
        deletedItem <- impl.delete(rootNode1(0xFA), keys(0).tail)
        rootNode2   = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 <- impl.printTree(rootNode2, "TREE (AFTER DELETE)", false)

        etalonTree2 = Vector(
          "TREE (AFTER DELETE): root =>",
          "   [FA]LEAF: prefix = 01122225, data = 0000...0055"
        )

        _ = printedTree1 shouldBe etalonTree1
        _ = printedTree2 shouldBe etalonTree2

      } yield ()
  }

  "reading data from existing node" should "return data" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val itemData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
      val key      = TestData.hexKey("0123456F1").toVector
      for {
        itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
        rootNode <- impl.constructNodeFromItem(itemOpt.get)

        readedDataOpt <- impl.read(rootNode, key)

        _ = readedDataOpt.map(readedData => readedData.toArray shouldBe itemData.toArray)

      } yield ()
  }

  "reading non - existent data" should "return none" in withRadixTreeImplAndInMemoStore {
    (impl, _) =>
      val itemData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
      val key      = TestData.hexKey("0123456F1").toVector
      for {
        itemOpt  <- impl.update(RadixTree.EmptyItem, key, itemData)
        rootNode <- impl.constructNodeFromItem(itemOpt.get)

        notExistingKey = TestData.hexKey("000").toVector
        readedDataOpt  <- impl.read(rootNode, notExistingKey)

        _ = readedDataOpt.map(readedData => readedData shouldBe none)

      } yield ()
  }

  "collision detecting in KVDB" should "works" in withRadixTreeImplAndInMemoStore {
    (impl, inMemoStore) =>
      def copyBVToBuf(bv: ByteVector): ByteBuffer = {
        val arr    = bv.toArray
        val newBuf = ByteBuffer.allocateDirect(arr.length)
        newBuf.put(arr).rewind()
      }

      val insertRecord    = createInsertActions(List(("FF00FFF01", 0xAA.toByte)))
      val deleteRecord    = createDeleteActions(List("FF00FFF01"))
      val collisionKVPair = (copyBVToBuf(History.emptyRootHash.bytes), insertRecord(0).hash.bytes)
      for {
        rootNode <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

        //  process
        newRootNodeOpt1 <- impl.makeActions(rootNode, insertRecord)

        printedTree1 <- impl.printTree(newRootNodeOpt1.get, "TREE1", false)
        hashOpt1 <- newRootNodeOpt1.traverse { newRootNode =>
                     val hash      = impl.saveNode(newRootNode)
                     val blakeHash = Blake2b256Hash.fromByteVector(hash)
                     impl.commit.as(blakeHash)
                   }
        clrWriteCache = impl.clearWriteCache()
        printedTree2  <- impl.printTree(newRootNodeOpt1.get, "TREE2", false)

        _ <- inMemoStore.put[ByteVector](Seq(collisionKVPair), copyBVToBuf)

        newRootNodeOpt2 <- impl.makeActions(newRootNodeOpt1.get, deleteRecord)
        hashOpt         = newRootNodeOpt2.map(node => impl.saveNode(node))
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

  "encoding and decoding node" should "work" in withRadixTreeImplAndInMemoStore { (impl, _) =>
    for {
      item1 <- impl.update(
                RadixTree.EmptyItem,
                TestData.hexKey("FFF8AFF1").toVector,
                generateDataWithLastNonZeroByte(0xAD.toByte).toVector
              )

      node           <- impl.constructNodeFromItem(item1.get)
      printedTreeStr <- impl.printTree(node, "NODE BEFORE DECODING", false)

      serializedNode = RadixTree.Codecs.encode(node)

      deserializedNode = RadixTree.Codecs.decode(serializedNode)

      printedTreeStr <- impl.printTree(node, "NODE AFTER SERIALIZE", false)

      etalonString = "ByteVector(37 bytes, 0xff03f8aff100000000000000000000000000000000000000000000000000000000000000ad)"
      printed      = println(s"Serialized vector : ${serializedNode.toString()}")

      _ = deserializedNode shouldBe node
      _ = serializedNode.toString() shouldBe etalonString
    } yield ()

  }

  "function makeActions" should "not create artefacts" in withRadixTreeImplAndInMemoStore {
    (impl, inMemoStore) =>
      val insertActions = createInsertActions(
        List(("FF00FFF01", 0xAA.toByte), ("FF0012345", 0x11.toByte), ("FF00F5676", 0x16.toByte))
      )
      for {
        rootNode1 <- impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

        rootNode2Opt <- impl.makeActions(rootNode1, insertActions)

        //  Root node is also saved in store
        hashOpt   = rootNode2Opt.map(rootNode => impl.saveNode(rootNode))
        committed <- impl.commit

        printedTree1 <- impl.printTree(rootNode2Opt.get, "TREE1111", false)

        _ = inMemoStore.numRecords() shouldBe 3

      } yield ()
  }

  "function makeActions in work with non-empty tree" should "not create artefacts" in withRadixTreeImplAndInMemoStore {
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
        committed1   <- impl.commit

        nodesCount1 = inMemoStore.numRecords()
        printed = println(
          s"Nodes count after creating tree of 3 leafs (without root node) : ${nodesCount1.toString}"
        )

        printedTree1 <- impl.printTree(rootNode2Opt.get, "TREE1", false)

        //  Append in existing tree 3 leafs...
        rootNode3Opt <- impl.makeActions(rootNode2Opt.get, insertLastNodesActions)
        committed2   <- impl.commit
        nodesCount2  = inMemoStore.numRecords()
        printed2 = println(
          s"Nodes count after appending ${insertLastNodesActions.size.toString} leafs (without root node): ${nodesCount2.toString}"
        )
        printedTree2 <- impl.printTree(rootNode3Opt.get, "TREE2", false)

        _ = nodesCount1 shouldBe 2
        _ = nodesCount2 shouldBe 3
      } yield ()
  }

  "function saveNode" should "put node into store" in withRadixTreeImplAndInMemoStore {
    (impl, inMemoStore) =>
      val itemData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
      val key      = TestData.hexKey("0123456F1").toVector
      for {
        itemOpt     <- impl.update(RadixTree.EmptyItem, key, itemData)
        nodesCount1 = inMemoStore.numRecords()

        node     <- impl.constructNodeFromItem(itemOpt.get)
        saved    = impl.saveNode(node)
        commited <- impl.commit

        //  After saving node numRecords must return 1
        nodesCount2 = inMemoStore.numRecords()
        _           = nodesCount1 shouldBe 0
        _           = nodesCount2 shouldBe 1
      } yield ()
  }

  "function commonPrefix" should "work" in {
    val v12345   = Seq[Byte](1, 2, 3, 4, 5)
    val v1245    = Seq[Byte](1, 2, 4, 5)
    val v123     = Seq[Byte](1, 2, 3)
    val v12367   = Seq[Byte](1, 2, 3, 6, 7)
    val v22345   = Seq[Byte](2, 2, 3, 4, 5)
    val emptyVal = Seq[Byte]()
    val res1     = HistoryMergingInstances.commonPrefix(v12345, v1245)
    val res2     = HistoryMergingInstances.commonPrefix(v12345, v123)
    val res3     = HistoryMergingInstances.commonPrefix(v12345, emptyVal)
    val res4     = HistoryMergingInstances.commonPrefix(v12345, v12367)
    val res5     = HistoryMergingInstances.commonPrefix(v22345, v12345)
    val res6     = HistoryMergingInstances.commonPrefix(emptyVal, emptyVal)
    println(s"PREFIX (1, 2, 3, 4, 5)(1, 2, 4, 5) should be: $res1")
    println(s"PREFIX (1, 2, 3, 4, 5)(1, 2, 3) should be: $res2")
    println(s"PREFIX (1, 2, 3, 4, 5)(empty) should be: $res3")
    println(s"PREFIX (1, 2, 3, 4, 5)(1, 2, 3, 6, 7) should be: $res4")
    println(s"PREFIX (2, 2, 3, 4, 5)(1, 2, 3, 4, 5) should be: $res5")
    println(s"PREFIX (empty)(empty) should be: $res6")
    res1 shouldBe List(1, 2)
    res2 shouldBe List(1, 2, 3)
    res3 shouldBe List()
    res4 shouldBe List(1, 2, 3)
    res5 shouldBe List()
    res6 shouldBe List()
  }

  def createInsertActions(
      tuplesKeyAndHash: List[(String, Byte)]
  ): List[InsertAction] = {
    val convertedKeysAndDatas = tuplesKeyAndHash.map(
      keyAndData =>
        (
          TestData.hexKey(keyAndData._1),
          Blake2b256Hash.fromByteArray(generateDataWithLastNonZeroByte(keyAndData._2))
        )
    )

    convertedKeysAndDatas.map(insData => InsertAction(insData._1, insData._2)) ++ Nil
  }

  def createDeleteActions(keys: List[String]): List[DeleteAction] =
    keys.map(key => DeleteAction(TestData.hexKey(key))) ++ Nil

  protected def withRadixTreeImplAndInMemoStore(
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
