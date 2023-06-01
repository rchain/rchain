package coop.rchain.casper.batch2

import cats.effect.IO
import cats.instances.list._
import cats.syntax.traverse._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LimitedParentDepthSpec extends AnyFlatSpec with Matchers {
  implicit val timeEff = new LogicalTime[IO]

  val genesisContext = buildGenesis()

  it should "obey absent parent depth limitation" in {
    TestNode.networkEff(genesisContext, networkSize = 2, maxParentDepth = None).use {
      case nodes @ n1 +: n2 +: Seq() =>
        for {
          produceDeploys <- (0 until 6).toList.traverse(i => basicDeployData[IO](i))

          b1 <- n1.propagateBlock(produceDeploys(0))()
          b2 <- n2.propagateBlock(produceDeploys(1))(nodes: _*)
          b4 <- n2.propagateBlock(produceDeploys(2))(nodes: _*)
          b4 <- n2.propagateBlock(produceDeploys(3))(nodes: _*)
          b5 <- n2.propagateBlock(produceDeploys(4))(nodes: _*)
          b6 <- n1.propagateBlock(produceDeploys(5))(nodes: _*)
        } yield b6.justifications shouldBe List(b1.blockHash, b5.blockHash)
    }
  }
}
