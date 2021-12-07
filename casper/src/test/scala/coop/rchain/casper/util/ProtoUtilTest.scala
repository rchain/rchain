//package coop.rchain.casper.util
//
//import cats.implicits._
//import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
//import coop.rchain.casper._
//import coop.rchain.casper.helper.TestNode
//import coop.rchain.casper.helper.TestNode.{Effect, _}
//import coop.rchain.shared.scalatestcontrib._
//import coop.rchain.models.BlockHash.BlockHash
//import coop.rchain.models.blockImplicits._
//import coop.rchain.p2p.EffectsTestInstances.LogicalTime
//import monix.execution.Scheduler.Implicits.global
//import org.scalatest._
//import org.scalatest.prop.GeneratorDrivenPropertyChecks
//
//class ProtoUtilTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
//
//  "dependenciesHashesOf" should "return hashes of all justifications and parents of a block" in {
//    forAll(blockElementGen()) { blockElement =>
//      val result = ProtoUtil.dependenciesHashesOf(blockElement)
//      val justificationsHashes = blockElement.justifications.map(
//        _.latestBlockHash
//      )
//      val parentsHashes = blockElement.header.parentsHashList
//      result should contain allElementsOf (justificationsHashes)
//      result should contain allElementsOf (parentsHashes)
//      result should contain theSameElementsAs ((justificationsHashes ++ parentsHashes).toSet)
//    }
//  }
//
//  import GenesisBuilder._
//
//  implicit val timeEff = new LogicalTime[Effect]
//
//  val genesis = buildGenesis()
//
//  "unseenBlockHashes" should "return empty for a single block dag" in effectTest {
//    TestNode.standaloneEff(genesis).use { node =>
//      import node.blockStore
//      for {
//        signedBlock       <- ConstructDeploy.basicDeployData[Effect](0) >>= (node.addBlock(_))
//        dag               <- node.casperEff.blockDag
//        unseenBlockHashes <- ProtoUtil.unseenBlockHashes[Effect](dag, signedBlock)
//        _                 = unseenBlockHashes should be(Set.empty[BlockHash])
//      } yield ()
//    }
//  }
//
//  it should "return all but the first block when passed the first block in a chain" in effectTest {
//    TestNode.standaloneEff(genesis).use { node =>
//      import node._
//      implicit val timeEff = new LogicalTime[Effect]
//
//      for {
//        block0            <- ConstructDeploy.basicDeployData[Effect](0) >>= (node.addBlock(_))
//        block1            <- ConstructDeploy.basicDeployData[Effect](1) >>= (node.addBlock(_))
//        dag               <- node.casperEff.blockDag
//        unseenBlockHashes <- ProtoUtil.unseenBlockHashes[Effect](dag, block0)
//        _                 = unseenBlockHashes should be(Set(block1.blockHash))
//      } yield ()
//    }
//  }
//}
