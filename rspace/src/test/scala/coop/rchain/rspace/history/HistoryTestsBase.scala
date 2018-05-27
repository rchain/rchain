package coop.rchain.rspace.history

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers, OptionValues, Outcome}

trait HistoryTestsBase[T, K, V] extends FlatSpec with Matchers with OptionValues {

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestTrieStore(f: ITrieStore[T, K, V] => Unit): Unit
}
