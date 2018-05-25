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
