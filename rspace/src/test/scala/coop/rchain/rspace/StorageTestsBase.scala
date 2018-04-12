package coop.rchain.rspace

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers, OptionValues, Outcome}

trait StorageTestsBase[C, P, A, K] extends FlatSpec with Matchers with OptionValues {

  type T = IStore[C, P, A, K] with ITestableStore[C, P]

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestStore(f: T => Unit): Unit
}
