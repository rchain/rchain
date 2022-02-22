package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree.{Item, RadixTreeImpl}
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

        etalonVectorStr = Vector(
          "TREE1: root =>",
          "   [0F]PTR: prefix = F0, ptr =>",
          "      [00]LEAF: prefix = 0201, data = 0000...000B",
          "      [0F]LEAF: prefix = FF01, data = 0000...000A",
          "   [FF]PTR: prefix = 0021, ptr =>",
          "      [11]LEAF: prefix = empty, data = 0000...0001",
          "      [12]LEAF: prefix = empty, data = 0000...0002"
        )

        _ = treeInfo shouldBe etalonVectorStr
      } yield ()
  }

  "Appending leaf in empty tree" should "work" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl ← radixTreeImplF
        newItemOpt ← impl.update(
                      RadixTree.EmptyItem,
                      TestData.hexKey("FFFFFFF1").toVector,
                      generateDataForHash(0xA.toByte).toVector
                    )

        newRootNodeOpt = newItemOpt.map(item ⇒ impl.createNodeFromItem(item))
        printedTreeStr ← impl.printTree(newRootNodeOpt.get, "TREE WITH ONE LEAF", false)

        etalonVectorStr = Vector(
          "TREE WITH ONE LEAF: root =>",
          "   [FF]LEAF: prefix = FFFFF1, data = 0000...000A"
        )

        _ = printedTreeStr shouldBe etalonVectorStr
      } yield ()
  }

  "Creating tree with two leafs and one nodePtr" should "work" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl                     ← radixTreeImplF
        commonKeyPartForTwoLeafs = "001122"
        keys = Vector[Seq[Byte]](
          TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
          TestData.hexKey(commonKeyPartForTwoLeafs + "225")
        )
        firstLeafOpt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        generateDataForHash(0x11.toByte).toVector
                      )
        secondLeafOpt ← impl.update(
                         firstLeafOpt.get,
                         keys(1),
                         generateDataForHash(0x55.toByte).toVector
                       )

        newRootNodeOpt = secondLeafOpt.map(item ⇒ impl.createNodeFromItem(item))
        printedTreeStr ← impl.printTree(newRootNodeOpt.get, "TREE WITH ONE NODE AND 2 LEAFS", false)

        etalonVectorStr = Vector(
          "TREE WITH ONE NODE AND 2 LEAFS: root =>",
          "   [00]PTR: prefix = 0112, ptr =>",
          "      [20]LEAF: prefix = 13, data = 0000...0011",
          "      [22]LEAF: prefix = 25, data = 0000...0055"
        )

        _ = printedTreeStr shouldBe etalonVectorStr
      } yield ()
  }
  // TODO: Ask Denis!!!!!
  "Appending leaf to leaf" should "create node with two leafs" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl          ← radixTreeImplF
        emptyRHash    = RadixHistory.emptyRootHash
        emptyRootNode ← impl.loadNode(emptyRHash.bytes, noAssert = true)

        keys = Vector[Seq[Byte]](
          TestData.hexKey("00000000"),
          TestData.hexKey("34564544")
        )
        firstLeafOpt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        generateDataForHash(0x11.toByte).toVector
                      )

        secondLeafOpt ← impl.update(
                         firstLeafOpt.get,
                         keys(1),
                         generateDataForHash(0x55.toByte).toVector
                       )

        //  TODO: look in makeActions!!
        // newRoot1Opt = secondLeafOpt.map(item ⇒ impl.createNodeFromItem(item))
        newRoot1 ← impl.createOrLoadNode(secondLeafOpt.get)

        printed ← impl.printTree(newRoot1, "TREE", false)
      } yield ()
  }

  //  TODO: Ask Deins!!!!
  "Updating leaf" should "work correctly" in createRadixTreeImpl { (radixTreeImplF, typedStore) ⇒
    for {
      impl          ← radixTreeImplF
      firstLeafData = generateDataForHash(0xCB.toByte).toVector
      newLeafData   = generateDataForHash(0xFF.toByte).toVector
      leafKey       = TestData.hexKey("0123456F1").toVector

      //  Create tree with one leaf
      leafItemOpt ← impl.update(RadixTree.EmptyItem, leafKey, firstLeafData)

      newRootNodeOpt = leafItemOpt.map(item ⇒ impl.createNodeFromItem(item))
      printedTreeBeforeChangingLeafData ← impl.printTree(
                                           newRootNodeOpt.get,
                                           "TREE WITH ONE LEAF",
                                           false
                                         )

      itemAfterChangeDataOpt ← impl.update(leafItemOpt.get, leafKey, newLeafData)

      newRootNodeAfterChangeLeafOpt = itemAfterChangeDataOpt.map(
        item ⇒ impl.createNodeFromItem(item)
      )
      printedTreeWithNewLeafData ← impl.printTree(
                                    newRootNodeAfterChangeLeafOpt.get,
                                    "TREE WITH ONE LEAF (AFTER CHANGING DATA)",
                                    false
                                  )

      treeBeforeChangeLeaf = Vector(
        "TREE WITH ONE LEAF: root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00CB"
      )

      treeAfterChangeLeaf = Vector(
        "TREE WITH ONE LEAF (AFTER CHANGING DATA): root =>",
        "   [00]LEAF: prefix = 123456F1, data = 0000...00FF"
      )

      _ = printedTreeBeforeChangingLeafData shouldBe treeBeforeChangeLeaf
      _ = printedTreeWithNewLeafData shouldBe treeAfterChangeLeaf
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

  "RadixTreeImpl" should "not allow to radix key is smaller than nodePtr key" in createRadixTreeImpl {
    (radixTreeImplF, typedStore) ⇒
      for {
        impl                     ← radixTreeImplF
        commonKeyPartForTwoLeafs = "121122"
        keys = Vector[Seq[Byte]](
          TestData.hexKey(commonKeyPartForTwoLeafs + "013"),
          TestData.hexKey(commonKeyPartForTwoLeafs + "225")
        )
        firstLeafOpt ← impl.update(
                        RadixTree.EmptyItem,
                        keys(0),
                        generateDataForHash(0x11.toByte).toVector
                      )
        secondLeafOpt ← impl.update(
                         firstLeafOpt.get,
                         keys(1),
                         generateDataForHash(0x55.toByte).toVector
                       )

        newRootNodeOpt = secondLeafOpt.map(item ⇒ impl.createNodeFromItem(item))
        printedTreeStr ← impl.printTree(newRootNodeOpt.get, "TREE WITH ONE NODE AND 2 LEAFS", false)

        rootNodeItem = newRootNodeOpt.get(1)
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
