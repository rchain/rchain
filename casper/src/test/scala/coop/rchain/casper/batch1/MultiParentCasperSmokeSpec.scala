package coop.rchain.casper.batch1

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

class MultiParentCasperSmokeSpec extends AnyFlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  private val genesis = buildGenesis()

  it should "perform the most basic deploy successfully" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      ConstructDeploy
        .sourceDeployNowF("new x in { x!(0) }", shardId = genesis.genesisBlock.shardId) >>= (node
        .addBlock(_))
    }
  }

}
