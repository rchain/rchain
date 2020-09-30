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

class MergeDiscussSpec extends FlatSpec with Matchers with Inspectors {
  import coop.rchain.casper.util.GenesisBuilder._
  val genesis          = buildGenesis()
  implicit val timeEff = new LogicalTime[Effect]
  it should "respect mergeability rules when checking two deployLogs" in {

    TestNode
      .networkEff(genesis, networkSize = 2)
      .use { nodes =>
        for {
          baseDeployB0 <- ConstructDeploy.sourceDeployNowF("""@1!(42)""")
          deployB1     <- ConstructDeploy.sourceDeployNowF("""@2!(0)""")
          deployB2     <- ConstructDeploy.sourceDeployNowF("""for (@y <- @1) {
                                                             |  new x in {
                                                             |    x!([1, 2]) |
                                                             |    for (@[a, b] <- x) { @1!(a + y) }
                                                             |  }
                                                             |}""".stripMargin)
          deployB3     <- ConstructDeploy.sourceDeployNowF("""Nil""")
          b0           <- nodes(0).propagateBlock(baseDeployB0)(nodes(1))
          b1           <- nodes(0).addBlock(deployB1)
          b2           <- nodes(1).addBlock(deployB2)
          _ = println(s"$b0, $b1, $b2")
          _            = nodes(0).logEff.warns.isEmpty shouldBe true
        } yield ()
      }
      .runSyncUnsafe()
  }
}
