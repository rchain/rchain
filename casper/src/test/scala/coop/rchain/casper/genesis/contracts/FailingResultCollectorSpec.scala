import coop.rchain.casper.helper.{RhoAssertEquals, RhoAssertTrue, RhoSpec, RhoTestAssertion}
import coop.rchain.rholang.FailingResultCollectorTest
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class FailingResultCollectorSpec extends FlatSpec with Matchers {
  def mkTest(assertion: RhoTestAssertion): Unit =
    it should assertion.testName in {
      assertion match {
        case RhoAssertEquals(testName, expected, actual) => actual should not be (expected)
        case RhoAssertTrue(testName, value)              => value should not be (true)
      }
    }

  RhoSpec
    .mkAssertions(FailingResultCollectorTest)
    .runSyncUnsafe(3.seconds)
    .foreach(mkTest)
}
