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
import coop.rchain.shared.Debug
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException
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
        _ = nodes(0).casperEff.contains(multiparentBlock) shouldBeF true
        _ = nodes(1).casperEff.contains(multiparentBlock) shouldBeF true
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 0).map(_ shouldBe Seq("0"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 1).map(_ shouldBe Seq("1"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 2).map(_ shouldBe Seq("2"))
      } yield ()
    }
  }

  it should "handle multi-parent blocks correctly when they operate on stdout" ignore effectTest {
    def echoContract(no: Int) =
      Rho(s"""new stdout(`rho:io:stdout`) in { stdout!("Contract $no") }""")
    merges(echoContract(1), echoContract(2), Rho("Nil"))
  }

  // TODO: Peek rows/column
  // Note this skips pairs that lead to infinite loops
  it should "handle multi-parent blocks correctly when they operate on volatile produce/consume pairs" in effectTest {
    // Sends (linear sends)
    val S0 = Rho("@0!(0)")
    val S1 = Rho("@0!(1)")
    // Repeats (persistent sends)
    val R0 = Rho("@0!!(0)")
    val R1 = Rho("@0!!(1)")
    // For-s (linear receives)
    val F_ = Rho("for (_ <- @0) { 0 }")
    val F0 = Rho("for (@0 <- @0) { 0 }")
    val F1 = Rho("for (@1 <- @0) { 0 }")
    // Contracts (persistent receives)
    val C_ = Rho("contract @0(id) = { 0 }")
    val C0 = Rho("contract @0(@0) = { 0 }")
    val C1 = Rho("contract @0(@1) = { 0 }")
    // Ground term
    val Z = Rho("0")
    //FIXME add missing cases for in-deploy COMM-s wherever there's a pair without X (a COMM)
    //FIXME all `conflictsForNow` should eventually be replaced with `merges`
    Map(
      "!X !X"   -> conflictsForNow(S0, S0, Z),
      "!X !4"   -> conflictsForNow(S0, S0, F_),
      "!X (!4)" -> merges(S0, S1 | F_, Z),
      "!X !C"   -> conflictsForNow(S0, S1, C1),
      "!X (!C)" -> merges(S0, S1 | C_, Z), //FIXME: THIS NEEDS TO CONFLICT
      "!X 4X"   -> conflicts(S0, F_, Z),
      "!X 4!"   -> conflictsForNow(S0, F_, S0),
      // !X (4!) covered above by the equivalent !X (!4)
      "!X 4!!"   -> conflictsForNow(S0, F_, R0),
      "!X (4!!)" -> merges(S0, F_ | R0, Z),
      "!X !!X"   -> conflictsForNow(S0, R0, Z),
      "!X !!4"   -> conflictsForNow(S0, R1, F1),
      // !X (!!4) covered above by the equivalent !X (4!!)
      "!X CX"        -> conflicts(S0, C_, Z),
      "!X C!"        -> conflicts(S0, C_, S0),
      "!X (C!)"      -> merges(S0, C_ | S0, Z), //FIXME: THIS NEEDS TO CONFLICT
      "!4 !4 same 4" -> conflicts(S0, S1, F_),
      "!4 !4 diff 4" -> conflicts(S0, S1, F0 | F1),
      "!4 (!4)"      -> merges(S0, S1 | F1, F0),
      "(!4) (!4)"    -> merges(S0 | F_, S0 | F_, Z),
      "!4 !C"        -> conflictsForNow(S0, S1, F0 | C1),
      "!4 4X"        -> conflictsForNow(S0, F_, F_),
      "!4 4!"        -> conflictsForNow(S0, F_, F0 | S1),
      "!4 4!!"       -> conflictsForNow(S0, F_, F0 | R1),
      "!4 !!X"       -> conflictsForNow(S0, R1, F0),
      "!4 !!4"       -> conflictsForNow(S0, R1, F_ | F1),
      "!4 CX"        -> conflictsForNow(S0, C_, F_),
      "!4 C!"        -> conflictsForNow(S0, C_, F0 | S1),
      "!C !C"        -> conflictsForNow(S0, S0, C_),
      "!C 4X"        -> conflictsForNow(S0, F_, C_),
      "!C 4!"        -> conflictsForNow(S0, F_, C0 | S1),
      "!C 4!!"       -> conflictsForNow(S0, F_, C0 | R1),
      "!C !!X"       -> conflictsForNow(S0, R1, C0),
      "!C !!4"       -> conflictsForNow(S0, R1, C0 | F1),
      "!C CX"        -> conflictsForNow(S0, C_, C_),
      "!C C!"        -> conflictsForNow(S0, C_, C0 | S1),
      "4X 4X"        -> conflictsForNow(F_, F_, Z),
      "4X 4!"        -> conflicts(F_, F_, S0),
      // Skipping 4X 4!! merges, 4X !!X may merge or not, 4X !!4 may merge or not
      "4X CX"        -> conflicts(F_, C_, Z),
      "4X C!"        -> conflictsForNow(F0, C1, S1),
      "4! 4! same !" -> conflicts(F_, F_, S0),
      "4! 4! diff !" -> conflictsForNow(F0, F1, S0 | S1),
      // Skipping 4! 4!! merges, 4! !!X merges, 4! !!4 merges
      "4! CX"        -> conflictsForNow(F_, C1, S0),
      "4! C! same !" -> conflictsForNow(F_, C_, S0),
      "4! C! diff !" -> conflictsForNow(F0, C1, S0 | S1),
      "CX CX"        -> conflictsForNow(C_, C_, Z),
      "C! C! same !" -> conflicts(C_, C_, S0),
      "C! C! diff !" -> conflictsForNow(C0, C1, S0 | S1)
      // 4!! / !!4 row is similar to !4 / 4! and thus skipped
      // C!! / !!C row is similar to !C / C! and thus skipped
    ).values.toList.parSequence_
  }

  case class Rho(value: String) {
    def |(other: Rho): Rho = Rho(s"$value | ${other.value}")
  }

  def conflictsForNow(b1: Rho, b2: Rho, base: Rho) = conflicts(b1, b2, base)

  private def conflicts(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    diamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 1) /*>>
      diamondConflictCheck(base, b2, b1, numberOfParentsForDiamondTip = 1)*/

  private def merges(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    diamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 2) /*>>
      diamondConflictCheck(base, b2, b1, numberOfParentsForDiamondTip = 2)*/

  private def diamondConflictCheck(
      base: Rho,
      b1: Rho,
      b2: Rho,
      numberOfParentsForDiamondTip: Int
  )(implicit file: sourcecode.File, line: sourcecode.Line): Effect[Unit] =
    Debug.print[Effect](base, b1, b2, numberOfParentsForDiamondTip) >>
      Vector(
        ConstructDeploy.sourceDeployNowF[Effect](base.value),
        ConstructDeploy.sourceDeployNowF[Effect](b1.value),
        ConstructDeploy.sourceDeployNowF[Effect](b2.value),
        ConstructDeploy.sourceDeployNowF[Effect]("Nil")
      ).sequence[Effect, DeployData]
        .flatMap { deploys =>
          HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
            for {
              _ <- nodes(0).addBlock(deploys(0))
              _ <- nodes(1).receive()
              _ <- nodes(0).addBlock(deploys(1))
              _ <- nodes(1).addBlock(deploys(2))
              _ <- nodes(0).receive()

              multiParentBlock <- nodes(0).addBlock(deploys(3))

              _ = nodes(0).logEff.warns.isEmpty shouldBe true
              _ = multiParentBlock.header.get.parentsHashList.size shouldBe numberOfParentsForDiamondTip
              _ = nodes(0).casperEff.contains(multiParentBlock) shouldBeF true
            } yield ()
          }
        }
        .adaptError {
          case _: TestFailedException =>
            new TestFailedException(s"""Expected
               | base = ${base.value}
               | b1   = ${b1.value}
               | b2   = ${b2.value}
               |
               | to produce a merge block with $numberOfParentsForDiamondTip parents, but it didn't
               |
               | go see it at ${file.value}:${line.value}
               | """.stripMargin, 5).severedAtStackDepth
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
        _      <- nodes(0).casperEff.contains(singleParentBlock) shouldBeF true
        result <- nodes(1).casperEff.contains(singleParentBlock) shouldBeF true
      } yield result
    }
  }

  it should "not produce UnusedCommEvent while merging non conflicting blocks in the presence of conflicting ones" in effectTest {

    val registryRho =
      """
        |"Expected output".
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
        _      <- nodes(0).casperEff.contains(singleParentBlock) shouldBeF true
        result <- nodes(1).casperEff.contains(singleParentBlock) shouldBeF true
      } yield result
    }
  }
}
