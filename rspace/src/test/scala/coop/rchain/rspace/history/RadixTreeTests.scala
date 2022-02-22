package coop.rchain.rspace.history

import cats.Parallel
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree.RadixTreeImpl
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
