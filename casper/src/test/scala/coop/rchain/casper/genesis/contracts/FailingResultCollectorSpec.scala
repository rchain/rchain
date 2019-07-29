import coop.rchain.casper.helper.{RhoAssertEquals, RhoAssertTrue, RhoSpec, RhoTestAssertion}
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv
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
                case _ => throw new RuntimeException()
              }
          }
        }
    }

  val result =
    RhoSpec
      .getResults(
        CompiledRholangSource("FailingResultCollectorTest.rho", NormalizerEnv.empty),
        Seq.empty,
        10.seconds
      )
      .runSyncUnsafe(Duration.Inf)

  result.assertions
    .foreach(mkTest(_))

  it should "complete within timeout" in {
    result.hasFinished should be(true)
  }
}
