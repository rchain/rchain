package coop.rchain.casper

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import coop.rchain.blockstorage.{BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, RSpaceUtil}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.{Log, Time}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperMergeSpec extends FlatSpec with Matchers with Inspectors {

  import RSpaceUtil._
  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "HashSetCasper" should "handle multi-parent blocks correctly" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      implicit val rm = nodes(1).runtimeManager
      for {
        deployData0 <- ConstructDeploy.basicDeployData[Effect](0)
        deployData1 <- ConstructDeploy.sourceDeployNowF("@1!(1) | for(@x <- @1){ @1!(x) }")
        deployData2 <- ConstructDeploy.basicDeployData[Effect](2)
        deploys = Vector(
          deployData0,
          deployData1,
          deployData2
        )
        block0 <- nodes(0).addBlock(deploys(0))
        block1 <- nodes(1).addBlock(deploys(1))
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        //multiparent block joining block0 and block1 since they do not conflict
        multiparentBlock <- nodes(0).addBlock(deploys(2))
        _                <- nodes(1).receive()

        _ = nodes(0).logEff.warns.isEmpty shouldBe true
        _ = nodes(1).logEff.warns.isEmpty shouldBe true
        _ = multiparentBlock.header.get.parentsHashList.size shouldBe 2
        _ = nodes(0).casperEff.contains(multiparentBlock.blockHash) shouldBeF true
        _ = nodes(1).casperEff.contains(multiparentBlock.blockHash) shouldBeF true
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 0).map(_ shouldBe Seq("0"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 1).map(_ shouldBe Seq("1"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 2).map(_ shouldBe Seq("2"))
      } yield ()
    }
  }

  it should "handle multi-parent blocks correctly when they operate on stdout" ignore effectTest {
    def echoContract(no: Int) = s"""new stdout(`rho:io:stdout`) in { stdout!("Contract $no") }"""
    checkMerge("Nil", echoContract(1), echoContract(2))
  }

  // TODO: Peek rows/column
  // Note this skips pairs that lead to infinite loops
  it should "handle multi-parent blocks correctly when they operate on volatile produce/consume pairs" in effectTest {
    // 4! 4!
    checkConflicts("@0!(0)", "for (_ <- @0) { 0 }", "for (_ <- @0) { 0 }") >>
      // 4X 4X (TODO: Eventually should merge)
      checkConflicts("0", "for (_ <- @0) { 0 }", "for (_ <- @0) { 0 }") >>
      //
      // !X !X (TODO: Eventually should merge)
      checkConflicts("0", "@0!(0)", "@0!(0)") >>
      // !X !4
      checkMerge("for (_ <- @0) { 0 }", "@1!(0)", "@0!(0)") >>
      // !X !C
      checkMerge("contract @0(id) = { 0 }", "@1!(0)", "@0!(0)") >>
      // !X 4X
      checkConflicts("0", "@0!(0)", "for (_ <- @0) { 0 }") >>
      // !X 4!
      checkMerge("@0!(0)", "@1!(0)", "for (_ <- @0) { 0 }") >>
      // !X 4!!
      checkMerge("@0!!(0)", "@1!(0)", "for (_ <- @0) { 0 }") >>
      // !X !!X (TODO: Eventually should merge)
      checkConflicts("0", "@0!(0)", "@0!!(0)") >>
      // !X !!4
      checkMerge("for (_ <- @0) { 0 }", "@1!(0)", "@0!!(0)") >>
      // !X CX
      checkConflicts("0", "@0!(0)", "contract @0(id) = { 0 }") >>
      // !X C!
      checkMerge("@0!(0)", "@1!(0)", "contract @0(id) = { 0 }") >>
      // (!4) (!4)
      checkMerge("0", "for (_ <- @0) { 0 } | @0!(1)", "for (_ <- @0) { 0 } | @0!(2)") >>
      // !4 !4
      checkConflicts("for (_ <- @0) { 0 }", "@0!(1)", "@0!(2)") >>
      // !4 !4
      checkMerge("for (@1 <- @0) { 0 }", "@0!(1)", "for (@2 <- @0) { 0 } | @0!(2)") >>
      // !4 !C
      checkMerge("for (@1 <- @0) { 0 } | contract @1(id) = { 0 }", "@0!(1)", "@1!(1)") >>
      // !4 4X
      checkMerge("for (_ <- @0) { 0 }", "@0!(0)", "for (_ <- @1) { 0 }") >>
      // Skip !4 4! as handled above
      // !4 4!!
      checkMerge("for (_ <- @0) { 0 } | @1!!(0)", "@0!(0)", "for (_ <- @1) { 0 }") >>
      // !4 !!X
      checkMerge("for (_ <- @0) { 0 }", "@0!(0)", "@1!!(0)") >>
      // !4 !!4
      checkConflicts("for (_ <- @0) { 0 } | for (_ <- @0) { 0 }", "@0!(0)", "@0!!(0)") >>
      // !4 CX (TODO: Check if this could conflict)
      checkMerge("for (_ <- @0) { 0 }", "@0!(1)", "contract @1(id) = { 0 }") >>
      // !4 C!
      checkMerge("for (_ <- @0) { 0 } | for (_ <- @1) { 0 }", "@0!(0)", "contract @1(id) = { 0 }")
  }

  private def checkConflicts(base: String, b1: String, b2: String): Effect[Unit] =
    diamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 1) >> diamondConflictCheck(
      base,
      b2,
      b1,
      numberOfParentsForDiamondTip = 1
    )

  private def checkMerge(base: String, b1: String, b2: String): Effect[Unit] =
    diamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 2) >> diamondConflictCheck(
      base,
      b2,
      b1,
      numberOfParentsForDiamondTip = 2
    )

  private def diamondConflictCheck(
      base: String,
      b1: String,
      b2: String,
      numberOfParentsForDiamondTip: Int
  ): Effect[Unit] =
    Vector(
      ConstructDeploy.sourceDeployNowF[Effect](base),
      ConstructDeploy.sourceDeployNowF[Effect](b1),
      ConstructDeploy.sourceDeployNowF[Effect](b2),
      ConstructDeploy.sourceDeployNowF[Effect]("Nil")
    ).sequence[Effect, DeployData].flatMap { deploys =>
      HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
        for {
          _ <- nodes(0).addBlock(deploys(0))
          _ <- nodes(0).receive()
          _ <- nodes(1).receive()
          _ <- nodes(0).addBlock(deploys(1))
          _ <- nodes(1).addBlock(deploys(2))
          _ <- nodes(0).receive()
          _ <- nodes(1).receive()
          _ <- nodes(0).receive()
          _ <- nodes(1).receive()

          multiParentBlock <- nodes(0).addBlock(deploys(3))
          _                <- nodes(1).receive()

          _ = nodes(0).logEff.warns.isEmpty shouldBe true
          _ = nodes(1).logEff.warns.isEmpty shouldBe true
          _ = multiParentBlock.header.get.parentsHashList.size shouldBe numberOfParentsForDiamondTip
          _ = nodes(0).casperEff.contains(multiParentBlock.blockHash) shouldBeF true
          _ = nodes(1).casperEff.contains(multiParentBlock.blockHash) shouldBeF true
        } yield ()
      }
    }

  it should "not merge blocks that touch the same channel" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        current0 <- timeEff.currentMillis
        deploy0 = ConstructDeploy.sourceDeploy(
          "@1!(47)",
          current0
        )
        current1 <- timeEff.currentMillis
        deploy1 = ConstructDeploy.sourceDeploy(
          "for(@x <- @1){ @1!(x) }",
          current1
        )
        deploy2 <- ConstructDeploy.basicDeployData[Effect](2)
        deploys = Vector(
          deploy0,
          deploy1,
          deploy2
        )
        block0 <- nodes(0).addBlock(deploys(0))
        block1 <- nodes(1).addBlock(deploys(1))
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        singleParentBlock <- nodes(0).addBlock(deploys(2))
        _                 <- nodes(1).receive()

        _      = nodes(0).logEff.warns.isEmpty shouldBe true
        _      = nodes(1).logEff.warns.isEmpty shouldBe true
        _      = singleParentBlock.header.get.parentsHashList.size shouldBe 1
        _      <- nodes(0).casperEff.contains(singleParentBlock.blockHash) shouldBeF true
        result <- nodes(1).casperEff.contains(singleParentBlock.blockHash) shouldBeF true
      } yield result
    }
  }

  it should "not produce UnusedCommEvent while merging non conflicting blocks in the presence of conflicting ones" in effectTest {

    val registryRho =
      """
        |// Expected output
        |//
        |// "REGISTRY_SIMPLE_INSERT_TEST: create arbitrary process X to store in the registry"
        |// Unforgeable(0xd3f4cbdcc634e7d6f8edb05689395fef7e190f68fe3a2712e2a9bbe21eb6dd10)
        |// "REGISTRY_SIMPLE_INSERT_TEST: adding X to the registry and getting back a new identifier"
        |// `rho:id:pnrunpy1yntnsi63hm9pmbg8m1h1h9spyn7zrbh1mcf6pcsdunxcci`
        |// "REGISTRY_SIMPLE_INSERT_TEST: got an identifier for X from the registry"
        |// "REGISTRY_SIMPLE_LOOKUP_TEST: looking up X in the registry using identifier"
        |// "REGISTRY_SIMPLE_LOOKUP_TEST: got X from the registry using identifier"
        |// Unforgeable(0xd3f4cbdcc634e7d6f8edb05689395fef7e190f68fe3a2712e2a9bbe21eb6dd10)
        |
        |new simpleInsertTest, simpleInsertTestReturnID, simpleLookupTest,
        |    signedInsertTest, signedInsertTestReturnID, signedLookupTest,
        |    ri(`rho:registry:insertArbitrary`),
        |    rl(`rho:registry:lookup`),
        |    stdout(`rho:io:stdout`),
        |    stdoutAck(`rho:io:stdoutAck`), ack in {
        |        simpleInsertTest!(*simpleInsertTestReturnID) |
        |        for(@idFromTest1 <- simpleInsertTestReturnID) {
        |            simpleLookupTest!(idFromTest1, *ack)
        |        } |
        |
        |        contract simpleInsertTest(registryIdentifier) = {
        |            stdout!("REGISTRY_SIMPLE_INSERT_TEST: create arbitrary process X to store in the registry") |
        |            new X, Y, innerAck in {
        |                stdoutAck!(*X, *innerAck) |
        |                for(_ <- innerAck){
        |                    stdout!("REGISTRY_SIMPLE_INSERT_TEST: adding X to the registry and getting back a new identifier") |
        |                    ri!(*X, *Y) |
        |                    for(@uri <- Y) {
        |                        stdout!("REGISTRY_SIMPLE_INSERT_TEST: got an identifier for X from the registry") |
        |                        stdout!(uri) |
        |                        registryIdentifier!(uri)
        |                    }
        |                }
        |            }
        |        } |
        |
        |        contract simpleLookupTest(@uri, result) = {
        |            stdout!("REGISTRY_SIMPLE_LOOKUP_TEST: looking up X in the registry using identifier") |
        |            new lookupResponse in {
        |                rl!(uri, *lookupResponse) |
        |                for(@val <- lookupResponse) {
        |                    stdout!("REGISTRY_SIMPLE_LOOKUP_TEST: got X from the registry using identifier") |
        |                    stdoutAck!(val, *result)
        |                }
        |            }
        |        }
        |    }
      """.stripMargin

    val tuplesRho =
      """
        |// tuples only support random access
        |new stdout(`rho:io:stdout`) in {
        |
        |  // prints 2 because tuples are 0-indexed
        |  stdout!((1,2,3).nth(1))
        |}
      """.stripMargin
    val timeRho =
      """
        |new getBlockData(`rho:block:data`), stdout(`rho:io:stdout`), tCh in {
        |  getBlockData!(*tCh) |
        |  for(@_, @t <- tCh) {
        |    match t {
        |      Nil => { stdout!("no block time; no blocks yet? Not connected to Casper network?") }
        |      _ => { stdout!({"block time": t}) }
        |    }
        |  }
        |}
      """.stripMargin

    HashSetCasperTestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      val n1     = nodes(0)
      val n2     = nodes(1)
      val n3     = nodes(2)
      val short  = ConstructDeploy.sourceDeploy("new x in { x!(0) }", 1L)
      val time   = ConstructDeploy.sourceDeploy(timeRho, 3L)
      val tuples = ConstructDeploy.sourceDeploy(tuplesRho, 2L)
      val reg    = ConstructDeploy.sourceDeploy(registryRho, 4L)
      for {
        b1n3 <- n3.addBlock(short)
        b1n2 <- n2.addBlock(time)
        b1n1 <- n1.addBlock(tuples)
        _    <- n2.receive()
        b2n2 <- n2.createBlock(reg)
      } yield ()
    }
  }

  it should "not merge blocks that touch the same channel involving joins" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        current0 <- timeEff.currentMillis
        deploy0 = ConstructDeploy.sourceDeploy(
          "@1!(47)",
          current0
        )
        current1 <- timeEff.currentMillis
        deploy1 = ConstructDeploy.sourceDeploy(
          "for(@x <- @1; @y <- @2){ @1!(x) }",
          current1
        )
        deploy2 <- ConstructDeploy.basicDeployData[Effect](2)
        deploys = Vector(
          deploy0,
          deploy1,
          deploy2
        )

        block0 <- nodes(0).addBlock(deploys(0))
        block1 <- nodes(1).addBlock(deploys(1))
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        singleParentBlock <- nodes(0).addBlock(deploys(2))
        _                 <- nodes(1).receive()

        _      = nodes(0).logEff.warns.isEmpty shouldBe true
        _      = nodes(1).logEff.warns.isEmpty shouldBe true
        _      = singleParentBlock.header.get.parentsHashList.size shouldBe 1
        _      <- nodes(0).casperEff.contains(singleParentBlock.blockHash) shouldBeF true
        result <- nodes(1).casperEff.contains(singleParentBlock.blockHash) shouldBeF true
      } yield result
    }
  }
}
