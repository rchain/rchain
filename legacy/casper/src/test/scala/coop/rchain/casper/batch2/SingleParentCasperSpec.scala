package coop.rchain.casper.batch2

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.crypto.signatures.Signed
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib.effectTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

// TODO Reenable after new finalizer is implemented.
class SingleParentCasperSpec extends AnyFlatSpec with Matchers with Inspectors {
  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "SingleParentCasper" should "create blocks with a single parent" ignore effectTest {
    TestNode.networkEff(genesis, networkSize = 2, maxNumberOfParents = 1).use {
      case n1 +: n2 +: _ =>
        for {
          deployDatas <- (0 to 2).toList
                          .traverse[Effect, Signed[DeployData]](
                            i =>
                              ConstructDeploy
                                .basicDeployData[Effect](i, shardId = genesis.genesisBlock.shardId)
                          )
          b1 <- n1.addBlock(deployDatas(0))
          b2 <- n2.addBlock(deployDatas(1))

          _ <- n1.syncWith(n2)
          _ <- n2.syncWith(n1)

          b3 <- n1.addBlock(deployDatas(2))
        } yield b3.justifications.size shouldBe 1
    }
  }
}
