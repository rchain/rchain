package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperFinalizationSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis(
    buildGenesisParameters(bondsFunction = _.map(pk => pk -> 10L).toMap)
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "increment last finalized block as appropriate in round robin" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployDatas <- (0 to 7).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))

        block1 <- nodes(0).publishBlock(deployDatas(0))(nodes: _*)
        block2 <- nodes(1).publishBlock(deployDatas(1))(nodes: _*)
        block3 <- nodes(2).publishBlock(deployDatas(2))(nodes: _*)
        block4 <- nodes(0).publishBlock(deployDatas(3))(nodes: _*)
        block5 <- nodes(1).publishBlock(deployDatas(4))(nodes: _*)

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block1
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)

        block6 <- nodes(2).publishBlock(deployDatas(5))(nodes: _*)

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block2
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)

        block7 <- nodes(0).publishBlock(deployDatas(6))(nodes: _*)

        _ <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block3
        _ = state.deployHistory.size should be(1)

        block8 <- nodes(1).publishBlock(deployDatas(7))(nodes: _*)

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block4
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)
      } yield ()
    }
  }
}
