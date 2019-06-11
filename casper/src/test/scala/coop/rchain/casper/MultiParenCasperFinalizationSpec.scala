package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperFinalizationSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, validatorPks.map(pk => pk -> 10L).toMap)
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "increment last finalized block as appropriate in round robin" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis).use { nodes =>
      for {
        deployDatas <- (0 to 7).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))

        block1 <- nodes(0).createBlock(deployDatas(0))
        _      <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        block2 <- nodes(1).createBlock(deployDatas(1))
        _      <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(0).receive()
        _      <- nodes(2).receive()

        block3 <- nodes(2).createBlock(deployDatas(2))
        _      <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        block4 <- nodes(0).createBlock(deployDatas(3))
        _      <- nodes(0).casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        block5 <- nodes(1).createBlock(deployDatas(4))
        _      <- nodes(1).casperEff.addBlock(block5, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(0).receive()
        _      <- nodes(2).receive()

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block1
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)

        block6 <- nodes(2).createBlock(deployDatas(5))
        _      <- nodes(2).casperEff.addBlock(block6, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block2
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)

        block7 <- nodes(0).createBlock(deployDatas(6))
        _      <- nodes(0).casperEff.addBlock(block7, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        _ <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block3
        _ = state.deployHistory.size should be(1)

        block8 <- nodes(1).createBlock(deployDatas(7))
        _      <- nodes(1).casperEff.addBlock(block8, ignoreDoppelgangerCheck[Effect])
        _      <- nodes(0).receive()
        _      <- nodes(2).receive()

        _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block4
        state <- nodes(0).casperState.read
        _     = state.deployHistory.size should be(1)
      } yield ()
    }
  }
}
