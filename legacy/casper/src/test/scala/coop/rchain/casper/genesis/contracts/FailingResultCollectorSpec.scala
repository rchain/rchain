import coop.rchain.casper.helper.{
  RhoAssertEquals,
  RhoAssertTrue,
  RhoSpec,
  RhoTestAssertion,
  TestResult
}
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FailingResultCollectorSpec extends AnyFlatSpec with AppendedClues with Matchers {
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

  val result: TestResult = new RhoSpec(
    CompiledRholangSource("FailingResultCollectorTest.rho", NormalizerEnv.Empty),
    Seq.empty,
    10.seconds
  ).result

  result.assertions
    .foreach(mkTest)

  it should "complete within timeout" in {
    result.hasFinished should be(true)
  }
}
