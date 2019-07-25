package coop.rchain.casper

import scala.util.Random._

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
        _ = nodes(0).casperEff.contains(multiparentBlock.blockHash) shouldBeF true
        _ = nodes(1).casperEff.contains(multiparentBlock.blockHash) shouldBeF true
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 0).map(_ shouldBe Seq("0"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 1).map(_ shouldBe Seq("1"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 2).map(_ shouldBe Seq("2"))
      } yield ()
    }
  }

  it should "handle multi-parent blocks correctly when they operate on stdout" in effectTest {
    def echoContract(no: Int) =
      Rho(s"""new stdout(`rho:io:stdout`) in { stdout!("Contract $no") }""")
    conflictsForNow(echoContract(1), echoContract(2), Rho("Nil"))
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
    //FIXME add missing cases for in-deploy COMM-s wherever there's a pair without X (a COMM)
    //FIXME all `conflictsForNow` should eventually be replaced with `merges`
    //same polarity, merges
    val SAME_POLARITY_MERGE =
      """
        |Two incoming sends/receives, at most one had a matching dual in TS.
        |The incoming events won't cause more COMMs together (same polarity).
        |They couldn't be competing for the same linear receive/send (at most one had a match).
        |
        |Notice this includes "two unsatisfied" and "must be looking for different data" cases.
        |""".stripMargin

    //same polarity, could sometimes merge
    val COULD_MATCH_SAME =
      """
        |Two incoming sends/receives each matched a receive/send that was in TS.
        |The incoming events won't cause more COMMs together (same polarity).
        |They could've matched the same linear event.
        |Mergeable if different events, or at least one matched event is non-linear.
        |""".stripMargin

    //diff polarity, merges
    val HAD_ITS_MATCH =
      """
        |A send and a receive were incoming, at least one had a match, either:
        | - both were linear
        | - one was non-linear, the other had a match
        |They couldn't match the same linear event (they have different polarity)
        |They couldn't spawn more work, because either:
        | - both were linear, one of them had a match in TS
        | - one was non-linear, but the other chose to go with its match
        |""".stripMargin

    //diff polarity, could sometimes merge
    val INCOMING_COULD_MATCH =
      """
        |An incoming send and an incoming receive could match each other,
        |leading to more COMMs needing to happen.
        |Mergeable if we use spatial matcher to prove they don't match.
        |""".stripMargin

    //deploy-local, merges
    val VOLATILE_EVENT =
      """
        |There's been a COMM within one of the deploys,
        |the other deploy saw none of it.
        |""".stripMargin

    //deploy-local, could sometimes merge
    val PRESISTENT_COULD_MATCH =
      """
        |There's been a COMM within one of the deploys, with one side non-linear.
        |The other deploy had an event without a match in TS, dual to the non-linear.
        |These could spawn more work.
        |Mergeable if we use spatial matcher to prove they don't match.
        |""".stripMargin

    //deploy-local, merges
    val PERSISTENT_COULD_NOT_MATCH =
      """
        |There's been a COMM within one of the deploys, with one side non-linear.
        |The other deploy had an event without a match in TS, of same polarity to the non-linear.
        |These could not spawn more work.
        |""".stripMargin

    Map(
      "!X !X"        -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, S0, Nil),
      "!X !4"        -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, S1, F1),
      "!X (!4)"      -> VOLATILE_EVENT             -> merges(S0, S0 | F_, Nil),
      "!X !C"        -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, S1, C1),
      "!X (!C)"      -> PRESISTENT_COULD_MATCH     -> conflicts(S0, S0 | C_, Nil),
      "!X 4X"        -> INCOMING_COULD_MATCH       -> conflicts(S0, F_, Nil),
      "!X 4!"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, S0),
      "!X (4!)"      -> VOLATILE_EVENT             -> coveredBy("!X (!4)"),
      "!X 4!!"       -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, R0),
      "!X (4!!)"     -> PERSISTENT_COULD_NOT_MATCH -> conflictsForNow(S0, F_ | R0, Nil),
      "!X !!X"       -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, R0, Nil),
      "!X !!4"       -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, R1, F1),
      "!X (!!4)"     -> PERSISTENT_COULD_NOT_MATCH -> coveredBy("!X (4!!)"),
      "!X CX"        -> INCOMING_COULD_MATCH       -> conflicts(S0, C_, Nil),
      "!X C!"        -> INCOMING_COULD_MATCH       -> conflicts(S0, C_, S0),
      "!X (C!)"      -> PRESISTENT_COULD_MATCH     -> conflicts(S0, C_ | S0, Nil),
      "!4 !4 same 4" -> COULD_MATCH_SAME           -> conflicts(S0, S1, F_),
      "!4 !4 diff 4" -> COULD_MATCH_SAME           -> conflictsForNow(S0, S1, F0 | F1),
      "!4 (!4)"      -> VOLATILE_EVENT             -> merges(S0, S1 | F_, F0),
      "(!4) (!4)"    -> VOLATILE_EVENT             -> merges(S0 | F_, S0 | F_, Nil),
      "!4 !C"        -> COULD_MATCH_SAME           -> conflictsForNow(S0, S1, F0 | C1),
      "!4 4X"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, F_),
      "!4 4!"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, F0 | S1),
      "!4 4!!"       -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, F0 | R1),
      "!4 !!X"       -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, R1, F0),
      "!4 !!4"       -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, R1, F_ | F1),
      "!4 CX"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, C_, F_),
      "!4 C!"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, C_, F0 | S1),
      "!C !C"        -> COULD_MATCH_SAME           -> conflictsForNow(S0, S0, C_),
      "!C 4X"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, C_),
      "!C 4!"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, C0 | S1),
      "!C 4!!"       -> HAD_ITS_MATCH              -> conflictsForNow(S0, F_, C0 | R1),
      "!C !!X"       -> SAME_POLARITY_MERGE        -> conflictsForNow(S0, R1, C0),
      "!C !!4"       -> COULD_MATCH_SAME           -> conflictsForNow(S0, R1, C0 | F1),
      "!C CX"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, C_, C_),
      "!C C!"        -> HAD_ITS_MATCH              -> conflictsForNow(S0, C_, C0 | S1),
      "4X 4X"        -> SAME_POLARITY_MERGE        -> conflictsForNow(F_, F_, Nil),
      "4X 4!"        -> SAME_POLARITY_MERGE        -> conflictsForNow(F0, F_, S1),
      // Skipping 4X 4!! merges, 4X !!X may merge or not, 4X !!4 may merge or not
      "4X CX"        -> SAME_POLARITY_MERGE    -> conflicts(F_, C_, Nil),
      "4X C!"        -> SAME_POLARITY_MERGE    -> conflictsForNow(F0, C1, S1),
      "4X (!!4)"     -> PRESISTENT_COULD_MATCH -> conflicts(F_, R0 | F_, Nil),
      "4! 4! same !" -> COULD_MATCH_SAME       -> conflicts(F_, F_, S0),
      "4! 4! diff !" -> COULD_MATCH_SAME       -> conflictsForNow(F0, F1, S0 | S1),
      // Skipping 4! 4!! merges, 4! !!X merges, 4! !!4 merges
      "4! CX"        -> SAME_POLARITY_MERGE -> conflictsForNow(F_, C1, S0),
      "4! C! same !" -> COULD_MATCH_SAME    -> conflicts(F_, C_, S0),
      "4! C! diff !" -> COULD_MATCH_SAME    -> conflictsForNow(F0, C1, S0 | S1),
      "CX CX"        -> SAME_POLARITY_MERGE -> conflictsForNow(C_, C_, Nil),
      "C! C! same !" -> COULD_MATCH_SAME    -> conflicts(C_, C_, S0),
      "C! C! diff !" -> COULD_MATCH_SAME    -> conflictsForNow(C0, C1, S0 | S1),
      // 4!! / !!4 row is similar to !4 / 4! and thus skipped
      // C!! / !!C row is similar to !C / C! and thus skipped
      "CX !!X" -> INCOMING_COULD_MATCH -> conflicts(R0, C_, Nil)
    ).values.toList.parSequence_
  }

  case class Rho(value: String) {
    def |(other: Rho): Rho = Rho(s"$value | ${other.value}")
  }
  object Nil extends Rho("Nil")

  def conflictsForNow(b1: Rho, b2: Rho, base: Rho) = conflicts(b1, b2, base)

  private def conflicts(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    randomDiamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 1)

  private def merges(b1: Rho, b2: Rho, base: Rho)(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    randomDiamondConflictCheck(base, b1, b2, numberOfParentsForDiamondTip = 2)

  def randomDiamondConflictCheck(
      base: Rho,
      b1: Rho,
      b2: Rho,
      numberOfParentsForDiamondTip: Int
  )(implicit file: sourcecode.File, line: sourcecode.Line): Effect[Unit] = {
    val shuffledBlocks = shuffle(Seq(b1, b2))
    diamondConflictCheck(base, shuffledBlocks(0), shuffledBlocks(1), numberOfParentsForDiamondTip)
  }

  private def coveredBy(equivalent: String) = ().pure[Effect]

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
              _ = nodes(0).casperEff.contains(multiParentBlock.blockHash) shouldBeF true
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
