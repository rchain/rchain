package coop.rchain.rspace.history

import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.test.TestKey4
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers, OptionValues, Outcome}
import scodec.Codec
import scodec.bits.ByteVector

trait HistoryTestsBase[T, K, V]
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with Configuration
    with WithTestStore[T, K, V] {

  def getRoot(store: ITrieStore[T, K, V], branch: Branch): Option[Blake2b256Hash] =
    store.withTxn(store.createTxnRead())(txn => store.getRoot(txn, branch))

  def setRoot(store: ITrieStore[T, K, V], branch: Branch, hash: Blake2b256Hash): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.get(txn, hash) match {
        case Some(Node(_)) => store.putRoot(txn, branch, hash)
        case _             => throw new Exception(s"no node at $hash")
      }
    }

  def getLeaves(store: ITrieStore[T, K, V], hash: Blake2b256Hash): Seq[Leaf[K, V]] =
    store.withTxn(store.createTxnRead())(txn => store.getLeaves(txn, hash))

  object TestData {
    val key1 = TestKey4.create(Seq(1, 0, 0, 0))
    val val1 = ByteVector("value1".getBytes)
    val key2 = TestKey4.create(Seq(1, 0, 0, 1))
    val val2 = ByteVector("value2".getBytes)
    val key3 = TestKey4.create(Seq(1, 0, 1, 0))
    val val3 = ByteVector("value3".getBytes)
    val key4 = TestKey4.create(Seq(1, 0, 1, 1))
    val val4 = ByteVector("value4".getBytes)
    val key5 = TestKey4.create(Seq(1, 0, 2, 1))
    val val5 = ByteVector("value5".getBytes)
    val key6 = TestKey4.create(Seq(1, 0, 0, 2))
    val val6 = ByteVector("value6".getBytes)
  }

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }
}

trait WithTestStore[T, K, V] {

  implicit def codecK: Codec[K]
  implicit def codecV: Codec[V]

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestTrieStore[R](f: (ITrieStore[T, K, V], Branch) => R): R
}
