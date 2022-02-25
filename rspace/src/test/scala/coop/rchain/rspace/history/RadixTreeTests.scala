package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree.{byteToInt, commonPrefix, Item, NodePtr, RadixTreeImpl}
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

trait RadixTreeObject[F[_]] {}
object RadixTreeObject {
  def apply[F[_]: Sync: Parallel](
      store: KeyValueTypedStore[F, ByteVector, ByteVector]
  ): F[RadixTreeImpl[F]] =
    for {
      impl ← Sync[F].delay(new RadixTreeImpl[F](store))
    } yield impl //RadixTreeObject(store)
  def create[F[_]: Concurrent: Sync: Parallel](
      store: KeyValueTypedStore[F, ByteVector, ByteVector]
  ): F[RadixTreeImpl[F]] = RadixTreeObject(store)
}

class RadixTreeTests extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  def generateDataWithLastNonZeroByte(lastByte: Byte): Array[Byte] =
    (List.fill(31)(0) ++ List.fill(1)(lastByte.toInt)).map(_.toByte).toArray

  "Tree with makeActions" should "be built correctly!!!" in withRadixTreeImplAndStore {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl       ← radixTreeImplF
        emptyRHash = RadixHistory.emptyRootHash
        rootNode   ← impl.loadNode(emptyRHash.bytes, noAssert = true)

        keys = List(
          TestData.hexKey("FF00FFF01"),
          TestData.hexKey("FF0000201"),
          TestData.hexKey("FF002111"),
          TestData.hexKey("FF002112")
        )

        lastByteForHashes = List[Byte](0xA, 0xB, 0x1, 0x2)

        //  List
        dataForHashes = lastByteForHashes.map(byte ⇒ generateDataWithLastNonZeroByte(byte))
        hashesBlake   = dataForHashes.map(hash ⇒ Blake2b256Hash.fromByteArray(hash))

        insertActions2 = InsertAction(keys(0), hashesBlake(0)) ::
          InsertAction(keys(1), hashesBlake(1)) ::
          InsertAction(keys(2), hashesBlake(2)) ::
          InsertAction(keys(3), hashesBlake(3)) :: Nil

        newRootNodeOpt ← impl.makeActions(rootNode, insertActions2)
        treeInfo       ← impl.printTree(newRootNodeOpt.get, "TREE1", false)

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

  "Appending leaf in empty tree" should "work" in withRadixTreeImplAndStore { (radixTreeImplF, _) ⇒
    for {
      impl ← radixTreeImplF
      item1 ← impl.update(
               RadixTree.EmptyItem,
               TestData.hexKey("FFFFFFF1").toVector,
               generateDataWithLastNonZeroByte(0xA.toByte).toVector
             )

      newRootNode    ← impl.constructNodeFromItem(item1.get)
      printedTreeStr ← impl.printTree(newRootNode, "TREE WITH ONE LEAF", false)

      etalonTree = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [FF]LEAF: prefix = FFFFF1, data = 0000...000A"
      )

      _ = printedTreeStr shouldBe etalonTree
    } yield ()
  }

  "Appending leaf to tree with one leaf " should "create 2 leafs with node ptr" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl                     ← radixTreeImplF
        commonKeyPartForTwoLeafs = "001122"
        keys = Vector[Seq[Byte]](
          TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
          TestData.hexKey(commonKeyPartForTwoLeafs + "225")
        )
        item1Opt ← impl.update(
                    RadixTree.EmptyItem,
                    keys(0),
                    generateDataWithLastNonZeroByte(0x11.toByte).toVector
                  )

        rootNode1    ← impl.constructNodeFromItem(item1Opt.get)
        printedTree1 ← impl.printTree(rootNode1, "TREE WITH ONE LEAF", false)

        etalonTree1 = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [00]LEAF: prefix = 01122013, data = 0000...0011"
        )

        item2Opt ← impl.update(
                    item1Opt.get,
                    keys(1),
                    generateDataWithLastNonZeroByte(0x55.toByte).toVector
                  )

        rootNode2    ← impl.constructNodeFromItem(item2Opt.get)
        printedTree2 ← impl.printTree(rootNode2, "TREE WITH ONE NODE AND 2 LEAFS", false)

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
  "Appending leaf to leaf" should "create node with two leafs" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl ← radixTreeImplF

        keys = Vector[Seq[Byte]](
          TestData.hexKey("00000000"),
          TestData.hexKey("34564544")
        )
        rootItem1Opt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        generateDataWithLastNonZeroByte(0x11.toByte).toVector
                      )

        rootItem2Opt ← impl.update(
                        rootItem1Opt.get,
                        keys(1),
                        generateDataWithLastNonZeroByte(0x55.toByte).toVector
                      )

        rootNode ← impl.constructNodeFromItem(rootItem2Opt.get)

        printedTreeStr ← impl.printTree(rootNode, "TREE: TWO LEAFS", false)

        etalonTree = Vector(
          "TREE: TWO LEAFS: root =>",
          "   [00]LEAF: prefix = 000000, data = 0000...0011",
          "   [34]LEAF: prefix = 564544, data = 0000...0055"
        )

        _ = printedTreeStr shouldBe etalonTree
      } yield ()
  }

  "Updating leaf" should "work correctly" in withRadixTreeImplAndStore { (radixTreeImplF, _) ⇒
    for {
      impl          ← radixTreeImplF
      firstLeafData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
      newLeafData   = generateDataWithLastNonZeroByte(0xFF.toByte).toVector
      leafKey       = TestData.hexKey("0123456F1").toVector

      //  Create tree with one leaf
      item1Opt ← impl.update(RadixTree.EmptyItem, leafKey, firstLeafData)

      rootNode1 <- impl.constructNodeFromItem(item1Opt.get)
      printedTree1 ← impl.printTree(
                      rootNode1,
                      "TREE WITH ONE LEAF",
                      false
                    )

      item2Opt  ← impl.update(item1Opt.get, leafKey, newLeafData)
      itemIdx   <- Sync[Task].delay(byteToInt(leafKey.head))
      rootNode2 ← impl.constructNodeFromItem(item2Opt.get)

      printedTree2 ← impl.printTree(
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

  "RadixTreeImpl" should "not allow to enter keys with different lengths" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl        ← radixTreeImplF
        leafData    = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
        leafKey     = TestData.hexKey("0123456F1").toVector
        testLeafKey = TestData.hexKey("112").toVector

        leafItemOpt ← impl.update(RadixTree.EmptyItem, leafKey, leafData)
        err         ← impl.update(leafItemOpt.get, testLeafKey, leafData).attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: All Radix keys should be same length."
      }
  }

  "RadixTreeImpl" should "not allow to radix key is smaller than NodePtr key" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl                     ← radixTreeImplF
        commonKeyPartForTwoLeafs = "121122"
        keys = Vector[Seq[Byte]](
          TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
          TestData.hexKey(commonKeyPartForTwoLeafs + "225")
        )
        item1Opt ← impl.update(
                    RadixTree.EmptyItem,
                    keys(0),
                    generateDataWithLastNonZeroByte(0x11.toByte).toVector
                  )
        item2Opt ← impl.update(
                    item1Opt.get,
                    keys(1),
                    generateDataWithLastNonZeroByte(0x55.toByte).toVector
                  )

        newRootNode    ← impl.constructNodeFromItem(item2Opt.get)
        printedTreeStr ← impl.printTree(newRootNode, "TREE WITH ONE NODE AND 2 LEAFS", false)

        rootNodeItem = newRootNode(1)
        err ← impl
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

  "Deleting not exising node" should "return none" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl ← radixTreeImplF

        //  Create tree with one node
        leafData = generateDataWithLastNonZeroByte(0xCC.toByte).toVector
        leafKey  = TestData.hexKey("0123456F1").toVector

        itemOpt  ← impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode ← impl.constructNodeFromItem(itemOpt.get)

        printedTreeStr ← impl.printTree(rootNode, "TREE (TEST DELETE NOT EXISTING LEAF)", false)

        //  Trying to delete not existing leaf...
        del ← impl.delete(itemOpt.get, TestData.hexKey("000").toVector.tail)

        _ = del.map(item ⇒ item shouldBe None)
      } yield ()
  }

  "Deleting leaf from tree with only one leaf" should "destroy tree" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl ← radixTreeImplF

        //  Create tree with one node
        leafData = generateDataWithLastNonZeroByte(0xCC.toByte).toVector
        leafKey  = TestData.hexKey("0123456F1").toVector

        itemOpt   ← impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode1 ← impl.constructNodeFromItem(itemOpt.get)

        printedTreeStr ← impl.printTree(rootNode1, "TREE (TEST DELETING ONE LEAF)", false)

        //  Trying to delete not existing leaf...
        deletedItem         ← impl.delete(itemOpt.get, leafKey)
        rootNode2           = rootNode1.updated((leafKey.head).toInt, deletedItem.get)
        printedEmptyTreeStr ← impl.printTree(rootNode2, "EMPTY TREE", false)

        emptyTreeStr = Vector("EMPTY TREE: root =>")

        _ = deletedItem.map(item ⇒ item shouldBe RadixTree.EmptyItem)
        _ = emptyTreeStr shouldBe printedEmptyTreeStr
      } yield ()
  }

  "Deleting leaf from node with two leafs" should "work correctly" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl ← radixTreeImplF

        keys = Vector[Seq[Byte]](
          TestData.hexKey("00000000"),
          TestData.hexKey("34564544")
        )

        rootItem1Hash = generateDataWithLastNonZeroByte(0x11.toByte).toVector
        rootItem2Hash = generateDataWithLastNonZeroByte(0xAF.toByte).toVector
        rootItem1Opt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        rootItem1Hash
                      )

        rootItem2Opt ← impl.update(
                        rootItem1Opt.get,
                        keys(1),
                        rootItem2Hash
                      )

        rootNode1    ← impl.constructNodeFromItem(rootItem2Opt.get)
        printedTree1 ← impl.printTree(rootNode1, "TREE: TWO LEAFS (BEFORE DELETING)", false)

        itemIdx <- Sync[Task].delay(byteToInt(keys(0).head))

        itemToDelete = rootNode1(itemIdx)
        deletedItem  ← impl.delete(itemToDelete, keys(0).tail)
        rootNode2    = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 ← impl.printTree(
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

  "Deleting data from leaf" should "destroy this leaf" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl                     ← radixTreeImplF
        ptrPrefix                = "FA"
        commonKeyPartForTwoLeafs = ptrPrefix + "01122"
        keys = Vector[Seq[Byte]](
          TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
          TestData.hexKey(commonKeyPartForTwoLeafs + "225")
        )
        item1Opt ← impl.update(
                    RadixTree.EmptyItem,
                    keys(0),
                    generateDataWithLastNonZeroByte(0x11.toByte).toVector
                  )
        item2Opt ← impl.update(
                    item1Opt.get,
                    keys(1),
                    generateDataWithLastNonZeroByte(0x55.toByte).toVector
                  )

        rootNode1    ← impl.constructNodeFromItem(item2Opt.get)
        printedTree1 ← impl.printTree(rootNode1, "TREE WITH ONE NODE AND 2 LEAFS", false)

        etalonTree1 = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [FA]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        itemIdx     <- Sync[Task].delay(byteToInt(keys(0).head))
        deletedItem ← impl.delete(rootNode1(0xFA), keys(0).tail)
        rootNode2   = rootNode1.updated(itemIdx, deletedItem.get)

        printedTree2 ← impl.printTree(rootNode2, "TREE (AFTER DELETE)", false)

        etalonTree2 = Vector(
          "TREE (AFTER DELETE): root =>",
          "   [FA]LEAF: prefix = 01122225, data = 0000...0055"
        )

        _ = printedTree1 shouldBe etalonTree1
        _ = printedTree2 shouldBe etalonTree2

      } yield ()
  }

  "reading data from existing node" should "return data" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl     ← radixTreeImplF
        itemData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
        key      = TestData.hexKey("0123456F1").toVector
        itemOpt  ← impl.update(RadixTree.EmptyItem, key, itemData)
        rootNode ← impl.constructNodeFromItem(itemOpt.get)

        readedDataOpt ← impl.read(rootNode, key)

        _ = readedDataOpt.map(readedData ⇒ readedData.toArray shouldBe itemData.toArray)

      } yield ()
  }

  "reading non - existent data" should "return none" in withRadixTreeImplAndStore {
    (radixTreeImplF, _) ⇒
      for {
        impl     ← radixTreeImplF
        itemData = generateDataWithLastNonZeroByte(0xCB.toByte).toVector
        key      = TestData.hexKey("0123456F1").toVector
        itemOpt  ← impl.update(RadixTree.EmptyItem, key, itemData)
        rootNode ← impl.constructNodeFromItem(itemOpt.get)

        notExistingKey = TestData.hexKey("000").toVector
        readedDataOpt  ← impl.read(rootNode, notExistingKey)

        _ = readedDataOpt.map(readedData ⇒ readedData shouldBe none)

      } yield ()
  }

  "collision detecting in KVDB" should "works" in withRadixTreeImplAndStore {
    (radixTreeImplF, inMemoStore) ⇒
      def copyBVToBuf(bv: ByteVector): ByteBuffer = {
        val arr    = bv.toArray
        val newBuf = ByteBuffer.allocateDirect(arr.length)
        newBuf.put(arr).rewind()
      }

      val insertRecord    = createInsertActions(List(("FF00FFF01", 0xAA.toByte)))
      val deleteRecord    = createDeleteActions(List("FF00FFF01"))
      val collisionKVPair = (copyBVToBuf(History.emptyRootHash.bytes), insertRecord(0).hash.bytes)
      for {
        impl       ← radixTreeImplF
        emptyRHash = RadixHistory.emptyRootHash
        rootNode   ← impl.loadNode(emptyRHash.bytes, noAssert = true)

        //  process
        newRootNodeOpt1 <- impl.makeActions(rootNode, insertRecord)

        printedTree1 ← impl.printTree(newRootNodeOpt1.get, "TREE1", false)
        hashOpt1 <- newRootNodeOpt1.traverse { newRootNode =>
                     val hash      = impl.saveNode(newRootNode)
                     val blakeHash = Blake2b256Hash.fromByteVector(hash)
                     impl.commit.as(blakeHash)
                   }
        clrWriteCache = impl.clearWriteCache()
        printedTree2  ← impl.printTree(newRootNodeOpt1.get, "TREE2", false)

        _ ← inMemoStore.put[ByteVector](Seq(collisionKVPair), copyBVToBuf)

        newRootNodeOpt2 ← impl.makeActions(newRootNodeOpt1.get, deleteRecord)
        hashOpt         = newRootNodeOpt2.map(node ⇒ impl.saveNode(node))
        err             ← impl.commit.attempt

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

  "encoding and decoding node" should "work" in withRadixTreeImplAndStore { (radixTreeImplF, _) ⇒
    for {
      impl ← radixTreeImplF
      item1 ← impl.update(
               RadixTree.EmptyItem,
               TestData.hexKey("FFF8AFF1").toVector,
               generateDataWithLastNonZeroByte(0xAD.toByte).toVector
             )

      node           ← impl.constructNodeFromItem(item1.get)
      printedTreeStr ← impl.printTree(node, "NODE BEFORE DECODING", false)

      serializedNode = RadixTree.Codecs.encode(node)

      deserializedNode = RadixTree.Codecs.decode(serializedNode)

      printedTreeStr ← impl.printTree(node, "NODE AFTER SERIALIZE", false)

      etalonString = "ByteVector(37 bytes, 0xff03f8aff100000000000000000000000000000000000000000000000000000000000000ad)"
      printed      = println(s"Serialized vector : ${serializedNode.toString()}")

      _ = deserializedNode shouldBe node
      _ = serializedNode.toString() shouldBe etalonString
    } yield ()

  }

  "function makeActions" should "not create artefacts" in withRadixTreeImplAndStore {
    (radixTreeImplF, inMemoStore) ⇒
      val insertActions = createInsertActions(
        List(("FF00FFF01", 0xAA.toByte), ("FF0012345", 0x11.toByte), ("FF00F5676", 0x16.toByte))
      )

      for {
        impl       ← radixTreeImplF
        emptyRHash = RadixHistory.emptyRootHash
        rootNode1  ← impl.loadNode(emptyRHash.bytes, noAssert = true)

        rootNode2Opt ← impl.makeActions(rootNode1, insertActions)

        //  Root node is also saved in store
        hashOpt   = rootNode2Opt.map(rootNode ⇒ impl.saveNode(rootNode))
        committed ← impl.commit

        printedTree1 ← impl.printTree(rootNode2Opt.get, "TREE1111", false)

        _ = inMemoStore.numRecords() shouldBe insertActions.size

      } yield ()
  }

  "function makeActions in work with non-empty tree" should "not create artefacts" in withRadixTreeImplAndStore {
    (radixTreeImplF, inMemoStore) ⇒
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
        impl      ← radixTreeImplF
        rootNode1 ← impl.loadNode(RadixHistory.emptyRootHash.bytes, noAssert = true)

        //  Create tree with 3 leafs
        rootNode2Opt ← impl.makeActions(rootNode1, insertFirstNodesActions)
        committed1   ← impl.commit

        nodesCount1 = inMemoStore.numRecords()
        printed = println(
          s"Nodes count after creating tree of 3 leafs (without root) : ${nodesCount1.toString}"
        )

        printedTree1 ← impl.printTree(rootNode2Opt.get, "TREE1", false)

        //  Append in existing tree 3 leafs...
        rootNode3Opt ← impl.makeActions(rootNode2Opt.get, insertLastNodesActions)
        committed2   ← impl.commit
        nodesCount2  = inMemoStore.numRecords()
        printed2 = println(
          s"Nodes count after appending ${insertLastNodesActions.size.toString} leafs : ${nodesCount2.toString}"
        )
        printedTree2 ← impl.printTree(rootNode3Opt.get, "TREE2", false)

        _ = nodesCount1 shouldBe 2
        _ = nodesCount2 shouldBe 3
      } yield ()
  }

  def createInsertActions(
      tuplesKeyAndHash: List[(String, Byte)]
  ): List[InsertAction] = {
    val convertedKeysAndDatas = tuplesKeyAndHash.map(
      keyAndData ⇒
        (
          TestData.hexKey(keyAndData._1),
          Blake2b256Hash.fromByteArray(generateDataWithLastNonZeroByte(keyAndData._2))
        )
    )

    convertedKeysAndDatas.map(insData ⇒ InsertAction(insData._1, insData._2)) ++ Nil
  }

  def createDeleteActions(keys: List[String]): List[DeleteAction] =
    keys.map(key ⇒ DeleteAction(TestData.hexKey(key))) ++ Nil

  protected def withRadixTreeImplAndStore(
      f: (
          Task[RadixTreeImpl[Task]],
          InMemoryKeyValueStore[Task]
      ) => Task[Unit]
  ): Unit = {
    // val typedStore: KeyValueTypedStore[Task, ByteVector, ByteVector] =
    //   (InMemoryKeyValueStore[Task]).toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
    val store      = InMemoryKeyValueStore[Task]
    val typedStore = store.toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
    val radixTreeImpl =
      RadixTreeObject.create(typedStore)
    f(radixTreeImpl, store).runSyncUnsafe(20.seconds)
  }
};
