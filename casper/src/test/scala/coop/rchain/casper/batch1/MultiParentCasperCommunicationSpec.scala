package coop.rchain.casper.batch1

import cats.implicits._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.Signed
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

// casper and communication has little to do with each other, so commented out
class MultiParentCasperCommunicationSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "MultiParentCasper" should "ask peers for blocks it is missing" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deploy1 <- ConstructDeploy.sourceDeployNowF("for(_ <- @1){ Nil } | @1!(1)")

        signedBlock1 <- nodes(0).addBlock(deploy1)
        _            <- nodes(1).handleReceive()
        _            <- nodes(2).shutoff() //nodes(2) misses this block

        deploy2      <- ConstructDeploy.sourceDeployNowF("@2!(2)")
        signedBlock2 <- nodes(0).addBlock(deploy2)
        _            <- nodes(2).syncWith(nodes)
        //1 receives block2
        //2 receives block2; asks if who has block1
        //1 receives request for has block1; sends i have block1
        //2 receives I have block1; asks for block1
        //1 receives request block1; sends block1
        //2 receives block2; asks for block1
        //2 receives block1; adds both block1 and block2

        // due to asynchronous processing of blocks syncWith ends before blocks are added to DAG
        // as this test is supposed to check that node is asking for block, just checking blockstore
        // for presence of block is enough
        _ <- nodes(2).blockStore.contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(2).blockStore.contains(signedBlock2.blockHash) shouldBeF true

        /*       this test is too restrictive in the presence of block hashes (see RCHAIN-3819).
                 Node #1 at this point only "knows" about block2 - which is nonetheless sufficient for recovering missing blocks.
                 Leaving for reference:
 _ <- nodes.toList.traverse_ { node =>
              for {
                maybeBlock1 <- node.blockStore.get(signedBlock1.blockHash)
                maybeBlock2 <- node.blockStore.get(signedBlock2.blockHash)
              } yield {
                withClue(s"Assertion failed for node ${node.local} --") {
                  maybeBlock1 shouldBe Some(signedBlock1)
                  maybeBlock2 shouldBe Some(signedBlock2)
                }
              }
            }*/
      } yield ()
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
   * TODO: investigate why this test is so brittle - in presence of hashes it starts to pass
   * only when hashes are synchroznied precisely as in the test - otherwise it will see 2 parents of h1
   *
   */
  // TODO this test fails because `TestNode.propagate(nodes)` returns prematurely.
  it should "ask peers for blocks it is missing and add them" ignore effectTest {
    def makeDeploy(i: Int): Effect[Signed[DeployData]] =
      ConstructDeploy.sourceDeployNowF(
        Vector("@2!(2)", "@1!(1)")(i),
        sec = if (i == 0) ConstructDeploy.defaultSec else ConstructDeploy.defaultSec2
      )

    def stepSplit(nodes: Seq[TestNode[Effect]]) =
      for {
        _ <- makeDeploy(0) >>= (nodes(0).addBlock(_))
        _ <- makeDeploy(1) >>= (nodes(1).addBlock(_))

        _ <- nodes(0).syncWith(nodes(1))
        _ <- nodes(2).shutoff() //nodes(2) misses this block
      } yield ()

    def stepSingle(nodes: Seq[TestNode[Effect]]) =
      for {
        _ <- makeDeploy(0) >>= (nodes(0).addBlock(_))

        _ <- nodes(1).syncWith(nodes(0))
        _ <- nodes(2).shutoff() //nodes(2) misses this block
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

        _ <- TestNode.propagate(nodes) // force the network to communicate

        _ <- nodes(2).contains(br.blockHash) shouldBeF true

        nr <- makeDeploy(0) >>= (nodes(2).addBlock(_))
      } yield {
        PrettyPrinter.buildString(nr.header.parentsHashList) shouldBe PrettyPrinter
          .buildString(List(br.blockHash))
      }
    }
  }

  //TODO: investigate this test - it doesnt make much sense in the presence of hashes (see RCHAIN-3819)
  // and why on earth does it test logs?
  it should "handle a long chain of block requests appropriately" ignore effectTest {
    TestNode
      .networkEff(genesis, networkSize = 2)
      .use { nodes =>
        for {
          _ <- (0 to 9).toList.traverse_[Effect, Unit] { i =>
                for {
                  deploy <- ConstructDeploy.basicDeployData[Effect](i)
                  block  <- nodes(0).addBlock(deploy)
                  _      <- nodes(1).shutoff() //nodes(1) misses this block
                } yield ()
              }
          deployData10 <- ConstructDeploy.basicDeployData[Effect](10)
          block11      <- nodes(0).addBlock(deployData10)

          // Cycle of requesting and passing blocks until block #3 from nodes(0) to nodes(1)
          _ <- (0 to 8).toList.traverse_[Effect, Unit] { i =>
                nodes(1).handleReceive() >> nodes(0).handleReceive() >> nodes(1)
                  .handleReceive() >> nodes(0)
                  .handleReceive() >> nodes(1).handleReceive() >> nodes(0).handleReceive()
              }

          // We simulate a network failure here by not allowing block #2 to get passed to nodes(1)

          // And then we assume fetchDependencies eventually gets called
          _ <- nodes(1).casperEff.fetchDependencies
          _ <- nodes(0).handleReceive()
          _ <- nodes(0).handleReceive()

          _ = nodes(1).logEff.infos.count(_ startsWith "Requested missing block") should be(10)
          result = nodes(0).logEff.infos.count(
            s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
          ) should be(10)
          // Oh yes - 10!
        } yield result
      }
  }

}
