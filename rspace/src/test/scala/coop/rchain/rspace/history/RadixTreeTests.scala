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
  def generateDataForHash(lastByte: Byte): Array[Byte] =
    (List.fill(31)(0) ++ List.fill(1)(lastByte.toInt)).map(_.toByte).toArray

  "Tree with makeActions" should "be built correctly!!!" in createRadixTreeImpl {
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
        dataForHashes = lastByteForHashes.map(byte ⇒ generateDataForHash(byte))
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

  "Appending leaf in empty tree" should "work" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF
        item1 ← impl.update(
                 RadixTree.EmptyItem,
                 TestData.hexKey("FFFFFFF1").toVector,
                 generateDataForHash(0xA.toByte).toVector
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

  "Appending leaf to tree with one leaf " should "create 2 leafs with node ptr" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
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
                    generateDataForHash(0x11.toByte).toVector
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
                    generateDataForHash(0x55.toByte).toVector
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
  "Appending leaf to leaf" should "create node with two leafs" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF

        keys = Vector[Seq[Byte]](
          TestData.hexKey("00000000"),
          TestData.hexKey("34564544")
        )
        rootItem1Opt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        generateDataForHash(0x11.toByte).toVector
                      )

        rootItem2Opt ← impl.update(
                        rootItem1Opt.get,
                        keys(1),
                        generateDataForHash(0x55.toByte).toVector
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

  "Updating leaf" should "work correctly" in createRadixTreeImpl { (radixTreeImplF, typedStore) ⇒
    for {
      impl          ← radixTreeImplF
      firstLeafData = generateDataForHash(0xCB.toByte).toVector
      newLeafData   = generateDataForHash(0xFF.toByte).toVector
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

  "RadixTreeImpl" should "not allow to enter keys with different lengths" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl        ← radixTreeImplF
        leafData    = generateDataForHash(0xCB.toByte).toVector
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

  "RadixTreeImpl" should "not allow to radix key is smaller than NodePtr key" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
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
                    generateDataForHash(0x11.toByte).toVector
                  )
        item2Opt ← impl.update(
                    item1Opt.get,
                    keys(1),
                    generateDataForHash(0x55.toByte).toVector
                  )

        newRootNode    ← impl.constructNodeFromItem(item2Opt.get)
        printedTreeStr ← impl.printTree(newRootNode, "TREE WITH ONE NODE AND 2 LEAFS", false)

        rootNodeItem = newRootNode(1)
        err ← impl
               .update(
                 rootNodeItem,
                 TestData.hexKey("0"),
                 generateDataForHash(0x33.toByte).toVector
               )
               .attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[AssertionError]
        ex.getMessage shouldBe s"assertion failed: Radix key should be longer than NodePtr key."
      }
  }

  "Deleting not exising node" should "return none" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF

        //  Create tree with one node
        leafData = generateDataForHash(0xCC.toByte).toVector
        leafKey  = TestData.hexKey("0123456F1").toVector

        itemOpt  ← impl.update(RadixTree.EmptyItem, leafKey, leafData)
        rootNode ← impl.constructNodeFromItem(itemOpt.get)

        printedTreeStr ← impl.printTree(rootNode, "TREE (TEST DELETE NOT EXISTING LEAF)", false)

        //  Trying to delete not existing leaf...
        del ← impl.delete(itemOpt.get, TestData.hexKey("000").toVector.tail)

        _ = del.map(item ⇒ item shouldBe None)
      } yield ()
  }

  "Deleting leaf from tree with only one leaf" should "destroy tree" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF

        //  Create tree with one node
        leafData = generateDataForHash(0xCC.toByte).toVector
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

  "Deleting leaf from node with two leafs" should "work correctly" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF

        keys = Vector[Seq[Byte]](
          TestData.hexKey("00000000"),
          TestData.hexKey("34564544")
        )

        rootItem1Hash = generateDataForHash(0x11.toByte).toVector
        rootItem2Hash = generateDataForHash(0xAF.toByte).toVector
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

  "Deleting data from leaf" should "destroy this leaf" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
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
                    generateDataForHash(0x11.toByte).toVector
                  )
        item2Opt ← impl.update(
                    item1Opt.get,
                    keys(1),
                    generateDataForHash(0x55.toByte).toVector
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

  protected def createRadixTreeImpl(
      f: (
          Task[RadixTreeImpl[Task]],
          KeyValueTypedStore[Task, ByteVector, ByteVector]
      ) => Task[Unit]
  ): Unit = {
    val typedStore: KeyValueTypedStore[Task, ByteVector, ByteVector] =
      (InMemoryKeyValueStore[Task]).toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
    val radixTreeImpl = RadixTreeObject.create(typedStore)
    f(radixTreeImpl, typedStore).runSyncUnsafe(20.seconds)
  }
};
