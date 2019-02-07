import coop.rchain.casper.helper.{RhoAssertEquals, RhoAssertTrue, RhoSpec, RhoTestAssertion}
import coop.rchain.rholang.FailingResultCollectorTest
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class FailingResultCollectorSpec extends FlatSpec with AppendedClues with Matchers {
  def mkTest(test: (String, List[RhoTestAssertion])): Unit =
    test match {
      case (testName, assertions) =>
        it should testName in {
          assertions.foreach {
            case RhoAssertEquals(_, expected, actual, clue) => {
              actual should not be (expected)
            } withClue (clue)
            case RhoAssertTrue(_, value, clue) => { value should not be (true) } withClue (clue)
          }
        }
    }

  val result =
    RhoSpec
      .getResults(FailingResultCollectorTest, Seq.empty)
      .runSyncUnsafe(Duration.Inf)

  result.assertions
    .foreach(mkTest(_))

  it should "complete within timeout" in {
    result.hasFinished should be(true)
  }
}
