package coop.rchain.rspace.history

import com.typesafe.scalalogging.Logger
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers, OptionValues, Outcome}

trait HistoryTestsBase[T, K, V]
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with Configuration {

  def getRoot(store: ITrieStore[T, K, V]): Option[Blake2b256Hash] =
    store.withTxn(store.createTxnRead())(txn => store.getRoot(txn))

  def setRoot(store: ITrieStore[T, K, V], hash: Blake2b256Hash): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.get(txn, hash) match {
        case Some(Node(_)) => store.putRoot(txn, hash)
        case _             => throw new Exception(s"no node at $hash")
      }
    }

  def getLeaves(store: ITrieStore[T, K, V], hash: Blake2b256Hash): Seq[Leaf[K, V]] =
    store.withTxn(store.createTxnRead())(txn => store.getLeaves(txn, hash))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestTrieStore[R](f: ITrieStore[T, K, V] => R): R
}
