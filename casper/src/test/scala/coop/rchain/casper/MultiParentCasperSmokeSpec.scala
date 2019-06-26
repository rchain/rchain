package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperSmokeSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val genesis = buildGenesis(buildGenesisParameters())

  it should "perform the most basic deploy successfully" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      ConstructDeploy.sourceDeployNowF("new x in { x!(0) }") >>= (node.addBlock(_))
    }
  }

}
