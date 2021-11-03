//package coop.rchain.casper.batch2
//
//import cats.instances.list._
//import cats.syntax.either._
//import cats.syntax.flatMap._
//import cats.syntax.traverse._
//import coop.rchain.casper.helper.TestNode
//import coop.rchain.casper.helper.TestNode.Effect
//import coop.rchain.casper.protocol.DeployData
//import coop.rchain.casper.util.ConstructDeploy
//import coop.rchain.casper.util.GenesisBuilder.buildGenesis
//import coop.rchain.casper.{BlockStatus, ValidBlock, Validate}
//import coop.rchain.crypto.signatures.Signed
//import coop.rchain.p2p.EffectsTestInstances.LogicalTime
//import coop.rchain.shared.scalatestcontrib.effectTest
//import monix.execution.Scheduler.Implicits.global
//import org.scalatest.{FlatSpec, Inspectors, Matchers}
//
//class SingleParentCasperSpec extends FlatSpec with Matchers with Inspectors {
//  implicit val timeEff = new LogicalTime[Effect]
//
//  val genesis = buildGenesis()
//
//  "SingleParentCasper" should "create blocks with a single parent" in effectTest {
//    TestNode.networkEff(genesis, networkSize = 2, maxNumberOfParents = 1).use {
//      case n1 +: n2 +: _ =>
//        for {
//          deployDatas <- (0 to 2).toList
//                          .traverse[Effect, Signed[DeployData]](
//                            i => ConstructDeploy.basicDeployData[Effect](i)
//                          )
//          b1 <- n1.addBlock(deployDatas(0))
//          b2 <- n2.addBlock(deployDatas(1))
//
//          _ <- n1.syncWith(n2)
//          _ <- n2.syncWith(n1)
//
//          b3 <- n1.addBlock(deployDatas(2))
//        } yield b3.header.parentsHashList.size shouldBe 1
//    }
//  }
//
//  it should "reject multi-parent blocks" in effectTest {
//    TestNode.networkEff(genesis, networkSize = 2, maxNumberOfParents = 1).use {
//      case n1 +: n2 +: _ =>
//        for {
//          deployDatas <- (0 to 2).toList
//                          .traverse[Effect, Signed[DeployData]](
//                            i => ConstructDeploy.basicDeployData[Effect](i)
//                          )
//          b1 <- n1.addBlock(deployDatas(0))
//          b2 <- n2.addBlock(deployDatas(1))
//
//          _ <- n1.syncWith(n2)
//          _ <- n2.syncWith(n1)
//
//          b3           <- n2.addBlock(deployDatas(2))
//          twoParents   = List(b2.blockHash, b1.blockHash)
//          dualParentB3 = b3.copy(header = b3.header.copy(parentsHashList = twoParents))
//
//          validateResult <- {
//            import n1._
//            n1.casperEff.getSnapshot(Some(dualParentB3)) >>=
//              (snap => Validate.parents(dualParentB3, n1.genesis, snap))
//          }
//        } yield validateResult shouldBe BlockStatus.invalidParents.asLeft[ValidBlock]
//    }
//  }
//}
