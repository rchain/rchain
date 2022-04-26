package coop.rchain.casper.batch1

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

// TODO consider adjusting or removing when new finalizer test is implemented
class MultiParentCasperFinalizationSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis(
    buildGenesisParameters(bondsFunction = _.map(pk => pk -> 10L).toMap)
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "increment last finalized block as appropriate in round robin" ignore effectTest {
    def assertFinalizedBlock(
        node: TestNode[Effect]
    )(expected: BlockMessage)(implicit file: sourcecode.File, line: sourcecode.Line) =
      for {
        lastFinalizedBlock <- node.lastFinalizedBlock
        //state              = node.casperBufferStorage
      } yield {
        withClue(s"Assertion failed at ${file.value}:${line.value}:\n\n") {
          lastFinalizedBlock shouldBe expected
          // state.deployHistory.size should be(1)
          // TODO except where it isn't - namely line 63. It was masked in the previous test
          // by testing the **old** state (twice)
        }
      }
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployDatas <- (0 to 7).toList.traverse(
                        i =>
                          ConstructDeploy
                            .basicDeployData[Effect](i, shardId = genesis.genesisBlock.shardId)
                      )

        block1 <- nodes(0).propagateBlock(deployDatas(0))(nodes: _*)
        block2 <- nodes(1).propagateBlock(deployDatas(1))(nodes: _*)
        block3 <- nodes(2).propagateBlock(deployDatas(2))(nodes: _*)
        block4 <- nodes(0).propagateBlock(deployDatas(3))(nodes: _*)
        block5 <- nodes(1).propagateBlock(deployDatas(4))(nodes: _*)

        _ <- assertFinalizedBlock(nodes(0))(block1)

        block6 <- nodes(2).propagateBlock(deployDatas(5))(nodes: _*)

        _ <- assertFinalizedBlock(nodes(0))(block2)

        block7 <- nodes(0).propagateBlock(deployDatas(6))(nodes: _*)

        _ <- assertFinalizedBlock(nodes(0))(block3)

        block8 <- nodes(1).propagateBlock(deployDatas(7))(nodes: _*)

        _ <- assertFinalizedBlock(nodes(0))(block4)
      } yield ()
    }
  }
}
