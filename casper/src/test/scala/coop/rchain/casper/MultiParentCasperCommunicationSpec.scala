package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol._
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}

class MultiParentCasperCommunicationSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "ask peers for blocks it is missing" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deploy1 <- ConstructDeploy.sourceDeployNowF("for(_ <- @1){ Nil } | @1!(1)")

        signedBlock1 <- nodes(0).addBlock(deploy1)
        _            <- nodes(1).receive()
        _            <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

        deploy2      <- ConstructDeploy.sourceDeployNowF("@2!(2)")
        signedBlock2 <- nodes(0).addBlock(deploy2)
        _            <- nodes(1).receive() //receives block2
        _            <- nodes(2).receive() //receives block2; asks if who has block1
        _            <- nodes(1).receive() //receives request for has block1; sends i have block1
        _            <- nodes(2).receive() //receives I have block1; asks for block1
        _            <- nodes(1).receive() //receives request block1; sends block1
        _            <- nodes(2).receive() //receives block2; asks for block1
        _            <- nodes(2).receive() //receives block1; adds both block1 and block2

        _ <- nodes(2).casperEff.contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(2).casperEff.contains(signedBlock2.blockHash) shouldBeF true

        _ = nodes(2).logEff.infos
          .count(_ startsWith "Requested missing block") should be(1)
        result = nodes(1).logEff.infos.count(
          s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
        ) should be(1)
        _ <- nodes.toList.traverse_ { node =>
              for {
                _      <- node.blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
                result <- node.blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              } yield result
            }
      } yield result
    }
  }

  /*
   *  DAG Looks like this:
   *
   *             h1
   *            /  \
   *           g1   g2
   *           |  X |
   *           f1   f2
   *            \  /
   *             e1
   *             |
   *             d1
   *            /  \
   *           c1   c2
   *           |  X |
   *           b1   b2
   *           |  X |
   *           a1   a2
   *            \  /
   *          genesis
   *
   * f2 has in its justifications list c2. This should be handled properly.
   *
   */
  it should "ask peers for blocks it is missing and add them" in effectTest {
    def makeDeploy(i: Int): Effect[DeployData] =
      ConstructDeploy.sourceDeployNowF(Vector("@2!(2)", "@1!(1)")(i))

    def stepSplit(nodes: Seq[TestNode[Effect]]) =
      for {
        _ <- makeDeploy(0) >>= (nodes(0).addBlock(_))
        _ <- makeDeploy(1) >>= (nodes(1).addBlock(_))

        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block
      } yield ()

    def stepSingle(nodes: Seq[TestNode[Effect]]) =
      for {
        _ <- makeDeploy(0) >>= (nodes(0).addBlock(_))

        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block
      } yield ()

    def propagate(nodes: Seq[TestNode[Effect]]) =
      for {
        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).receive()
      } yield ()

    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        _ <- stepSplit(nodes) // blocks a1 a2
        _ <- stepSplit(nodes) // blocks b1 b2
        _ <- stepSplit(nodes) // blocks c1 c2

        _ <- stepSingle(nodes) // block d1
        _ <- stepSingle(nodes) // block e1

        _ <- stepSplit(nodes) // blocks f1 f2
        _ <- stepSplit(nodes) // blocks g1 g2

        // this block will be propagated to all nodes and force nodes(2) to ask for missing blocks.
        br <- makeDeploy(0) >>= (nodes(0).addBlock(_)) // block h1

        _ <- List.fill(22)(propagate(nodes)).sequence // force the network to communicate

        _ <- nodes(2).casperEff.contains(br.blockHash) shouldBeF true

        nr <- makeDeploy(0) >>= (nodes(2).addBlock(_))
        _  = nr.header.parentsHashList shouldBe List(br.blockHash)
      } yield ()
    }
  }

  it should "handle a long chain of block requests appropriately" in effectTest {
    TestNode
      .networkEff(genesis, networkSize = 2)
      .use { nodes =>
        for {
          _ <- (0 to 9).toList.traverse_[Effect, Unit] { i =>
                for {
                  deploy <- ConstructDeploy.basicDeployData[Effect](i)
                  block  <- nodes(0).addBlock(deploy)
                  _      <- nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block
                } yield ()
              }
          deployData10 <- ConstructDeploy.basicDeployData[Effect](10)
          block11      <- nodes(0).addBlock(deployData10)

          // Cycle of requesting and passing blocks until block #3 from nodes(0) to nodes(1)
          _ <- (0 to 8).toList.traverse_[Effect, Unit] { i =>
                nodes(1).receive() >> nodes(0).receive() >> nodes(1).receive() >> nodes(0)
                  .receive() >> nodes(1).receive() >> nodes(0).receive()
              }

          // We simulate a network failure here by not allowing block #2 to get passed to nodes(1)

          // And then we assume fetchDependencies eventually gets called
          _ <- nodes(1).casperEff.fetchDependencies
          _ <- nodes(0).receive()
          _ <- nodes(0).receive()

          _ = nodes(1).logEff.infos.count(_ startsWith "Requested missing block") should be(10)
          result = nodes(0).logEff.infos.count(
            s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
          ) should be(10)
        } yield result
      }
  }

}
