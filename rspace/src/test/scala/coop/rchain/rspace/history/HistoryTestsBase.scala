package coop.rchain.rspace.history

import com.typesafe.scalalogging.Logger
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers, OptionValues, Outcome}
import scodec.bits.ByteVector

trait HistoryTestsBase[T, K, V]
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with Configuration {

  object TestData {
    val key1 = TestKey.create(Seq(1, 0, 0, 0))
    val val1 = ByteVector("value1".getBytes)
    val key2 = TestKey.create(Seq(1, 0, 0, 1))
    val val2 = ByteVector("value2".getBytes)
    val key3 = TestKey.create(Seq(1, 0, 1, 0))
    val val3 = ByteVector("value3".getBytes)
    val key4 = TestKey.create(Seq(1, 0, 1, 1))
    val val4 = ByteVector("value4".getBytes)
    val key5 = TestKey.create(Seq(1, 0, 2, 1))
    val val5 = ByteVector("value5".getBytes)
    val key6 = TestKey.create(Seq(1, 0, 0, 2))
    val val6 = ByteVector("value6".getBytes)
  }

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
