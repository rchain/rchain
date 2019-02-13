import coop.rchain.casper.helper.{RhoAssertEquals, RhoAssertTrue, RhoSpec, RhoTestAssertion}
import coop.rchain.rholang.FailingResultCollectorTest
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class FailingResultCollectorSpec extends FlatSpec with AppendedClues with Matchers {
  def clue(clueMsg: String, attempt: Long) = s"$clueMsg (attempt $attempt)"
  def mkTest(test: (String, Map[Long, List[RhoTestAssertion]])): Unit =
    test match {
      case (testName, attempts) =>
        it should testName in {
          attempts.foreach {
            case (attempt, assertions) =>
              assertions.foreach {
                case RhoAssertEquals(_, expected, actual, clueMsg) =>
                  actual should not be (expected) withClue (clue(clueMsg, attempt))
                case RhoAssertTrue(_, value, clueMsg) =>
                  value should not be (true) withClue (clue(clueMsg, attempt))
              }
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
