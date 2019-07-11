package coop.rchain.casper.util

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ProtoUtilTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "dependenciesHashesOf" should "return hashes of all justifications and parents of a block" in {
    forAll(blockElementGen) { blockElement =>
      val result = ProtoUtil.dependenciesHashesOf(blockElement)
      val justificationsHashes = blockElement.justifications.map(
        _.latestBlockHash
      )
      val parentsHashes = blockElement.header.toSeq.flatMap(_.parentsHashList)
      result should contain allElementsOf (justificationsHashes)
      result should contain allElementsOf (parentsHashes)
      result should contain theSameElementsAs ((justificationsHashes ++ parentsHashes).toSet)
    }
  }

  import GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "unseenBlockHashes" should "return empty for a single block dag" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy               <- ConstructDeploy.basicDeployData[Effect](0)
        casper               = node.casperEff
        _                    <- casper.deploy(deploy)
        createBlockResult    <- casper.createBlock
        Created(signedBlock) = createBlockResult
        _                    <- casper.addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        dag                  <- casper.blockDag
        unseenBlockHashes    <- ProtoUtil.unseenBlockHashes[Effect](dag, signedBlock)
        _                    = unseenBlockHashes should be(Set.empty[BlockHash])
      } yield ()
    }
  }

  it should "return all but the first block when passed the first block in a chain" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy            <- ConstructDeploy.basicDeployData[Effect](0)
        casper            = node.casperEff
        _                 <- casper.deploy(deploy)
        createBlockResult <- casper.createBlock
        Created(block0)   = createBlockResult
        _                 <- casper.addBlock(block0, ignoreDoppelgangerCheck[Effect])
        deploy            <- ConstructDeploy.basicDeployData[Effect](1)
        createBlockResult <- casper.deploy(deploy) >> casper.createBlock
        Created(block1)   = createBlockResult
        _                 <- casper.addBlock(block1, ignoreDoppelgangerCheck[Effect])
        dag               <- casper.blockDag
        unseenBlockHashes <- ProtoUtil.unseenBlockHashes[Effect](dag, block0)
        _                 = unseenBlockHashes should be(Set(block1.blockHash))
      } yield ()
    }
  }
}
