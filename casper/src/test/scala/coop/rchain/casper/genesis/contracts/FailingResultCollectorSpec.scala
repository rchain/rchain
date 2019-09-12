package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper._
import coop.rchain.casper.util.ConstructDeploy.defaultSec
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.io.Source

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

  val result: TestResult = new RhoSpec(
    Seq((Source.fromResource("FailingResultCollectorTest.rho").mkString, defaultSec)),
    10.seconds
  ).result

  result.assertions
    .foreach(mkTest)

  it should "complete within timeout" in {
    result.hasFinished should be(true)
  }
}
