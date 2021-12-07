package coop.rchain.casper.batch1

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperSmokeSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  private val genesis = buildGenesis()

  it should "perform the most basic deploy successfully" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      ConstructDeploy.sourceDeployNowF("new x in { x!(0) }") >>= (node.addBlock(_))
    }
  }

}
