package coop.rchain.casper.batch1

import cats.implicits._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.Signed
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.casper.EstimatorHelper
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class ConflictDetectSpec extends FlatSpec with Matchers with Inspectors with MergeabilityRules {
  import coop.rchain.casper.util.GenesisBuilder._
  val genesis          = buildGenesis()
  implicit val timeEff = new LogicalTime[Effect]
  it should "respect mergeability rules when checking two deployLogs" in {
    baseMergeabilityCases.map(_._2.runSyncUnsafe())
  }

  it should "respect mergeability rules when checking two deployLogs with peek" in {
    peekMergeabilityCases.map(_._2.runSyncUnsafe())
  }

  def conflicts(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    estimatorCheckConflict(base, b1, b2, true)

  def merges(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    estimatorCheckConflict(base, b1, b2, false)

  private[this] def estimatorCheckConflict(
      base: Rho,
      b1: Rho,
      b2: Rho,
      result: Boolean
  )(implicit file: sourcecode.File, line: sourcecode.Line): Effect[Unit] =
    Vector(
      ConstructDeploy.sourceDeployNowF[Effect](base.value),
      ConstructDeploy.sourceDeployNowF[Effect](b1.value),
      ConstructDeploy.sourceDeployNowF[Effect](b2.value, sec = ConstructDeploy.defaultSec2)
    ).sequence[Effect, Signed[DeployData]]
      .flatMap { deploys =>
        // TODO, construct these two states in one node(event not with node setup) instead of creating a network with 3 nodes which is better in performance
        TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
          for {
            _          <- nodes(0).propagateBlock(deploys(0))(nodes(1))
            b1         <- nodes(0).addBlock(deploys(1))
            b2         <- nodes(1).addBlock(deploys(2))
            isConflict = EstimatorHelper.isConflict(b1.body.deploys(0), b2.body.deploys(0))

            _ = nodes(0).logEff.warns.isEmpty shouldBe true
            _ = isConflict shouldBe result
          } yield ()
        }
      }
      .adaptError {
        case e: Throwable =>
          new TestFailedException(s"""Expected
                                     | base = ${base.value}
                                     | b1   = ${b1.value}
                                     | b2   = ${b2.value}
                                     | The conflict result should be ${result}
                                     | but it isn't
                                     |
                                     | go see it at ${file.value}:${line.value}
                                     | """.stripMargin, e, 5).severedAtStackDepth
      }
}
