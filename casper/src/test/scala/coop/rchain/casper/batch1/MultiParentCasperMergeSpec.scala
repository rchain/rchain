package coop.rchain.casper.batch1

import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.{ConstructDeploy, RSpaceUtil}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperMergeSpec extends FlatSpec with Matchers with Inspectors {

  import RSpaceUtil._
  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "HashSetCasper" should "handle multi-parent blocks correctly" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      implicit val rm = nodes(1).runtimeManager
      for {
        deployData0 <- ConstructDeploy.basicDeployData[Effect](0, sec = ConstructDeploy.defaultSec2)
        deployData1 <- ConstructDeploy.sourceDeployNowF("@1!(1) | for(@x <- @1){ @1!(x) }")
        deployData2 <- ConstructDeploy.basicDeployData[Effect](2)
        deploys = Vector(
          deployData0,
          deployData1,
          deployData2
        )
        block0 <- nodes(0).addBlock(deploys(0))
        block1 <- nodes(1).addBlock(deploys(1))
        _      <- TestNode.propagate(nodes)

        //multiparent block joining block0 and block1 since they do not conflict
        multiparentBlock <- nodes(0).propagateBlock(deploys(2))(nodes: _*)

        _ = multiparentBlock.header.parentsHashList.size shouldBe 2
        _ <- nodes(0).contains(multiparentBlock.blockHash) shouldBeF true
        _ <- nodes(1).contains(multiparentBlock.blockHash) shouldBeF true
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 0).map(_ shouldBe Seq("0"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 1).map(_ shouldBe Seq("1"))
        _ <- getDataAtPublicChannel[Effect](multiparentBlock, 2).map(_ shouldBe Seq("2"))
      } yield ()
    }
  }

  // TODO enable tests below
//  it should "handle multi-parent blocks correctly when they operate on stdout" in effectTest {
//    def echoContract(no: Int) =
//      Rho(s"""new stdout(`rho:io:stdout`) in { stdout!("Contract $no") }""")
//    merges(echoContract(1), echoContract(2), Rho("Nil"))
//  }
//
//  it should "not conflict on registry lookups" in effectTest {
//    val uri         = "rho:id:i1kuw4znrkazgbmc4mxe7ua4s1x41zd7qd8md96edxh1n87a5seht3"
//    val toSign: Par = ETuple(Seq(GInt(0), GString("foo")))
//    val toByteArray = Serialize[Par].encode(toSign).toArray
//    val sig         = Secp256k1.sign(Blake2b256.hash(toByteArray), ConstructDeploy.defaultSec)
//    val setup =
//      Rho(s"""
//             |new rs(`rho:registry:insertSigned:secp256k1`) in {
//             |  rs!(
//             |    "${ConstructDeploy.defaultPub.bytes.toHex}".hexToBytes(),
//             |    (0, "foo"),
//             |    "${sig.toHex}".hexToBytes(),
//             |    Nil
//             |  )
//             |}
//        """.stripMargin)
//    val lookup =
//      Rho(s"""
//             |new rl(`rho:registry:lookup`) in {
//             |  rl!(`$uri`, Nil)
//             |}
//        """.stripMargin)
//
//    merges(lookup, lookup, setup)
//  }

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
        |  for(@_, @t, @_ <- tCh) {
        |    match t {
        |      Nil => { stdout!("no block time; no blocks yet? Not connected to Casper network?") }
        |      _ => { stdout!({"block time": t}) }
        |    }
        |  }
        |}
      """.stripMargin

    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
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
        _    <- n2.handleReceive()
        b2n2 <- n2.createBlock(reg)
      } yield ()
    }
  }

  it should "not merge blocks that touch the same channel involving joins" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        current0 <- timeEff.currentMillis
        deploy0 = ConstructDeploy.sourceDeploy(
          "@1!(47)",
          current0,
          sec = ConstructDeploy.defaultSec2
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
        _      <- TestNode.propagate(nodes)

        singleParentBlock <- nodes(0).addBlock(deploys(2))
        _                 <- nodes(1).handleReceive()

        _ = singleParentBlock.header.parentsHashList.size shouldBe 1
        _ <- nodes(0).contains(singleParentBlock.blockHash) shouldBeF true
        _ <- nodes(1).knowsAbout(singleParentBlock.blockHash) shouldBeF true
      } yield ()
    }
  }

//  "This spec" should "cover all mergeability cases" in {
//    val allMergeabilityCases = {
//      val events = List(
//        "!X",
//        "!4",
//        "!C",
//        "4X",
//        "4!",
//        "4!!",
//        "PX",
//        "P!",
//        "P!!",
//        "!!X",
//        "!!4",
//        "!!C",
//        "CX",
//        "C!",
//        "C!!"
//      )
//
//      val pairs    = events.combinations(2)
//      val diagonal = events.map(x => List(x, x))
//      val cases    = (pairs ++ diagonal).toList
//
//      def isComm(s: String) = !s.contains("X")
//
//      def makeVolatile(s: String): List[String] = isComm(s) match {
//        case false => List(s)
//        case true  => List(s, s"($s)")
//      }
//
//      def makeVolatiles(v: List[String]): List[List[String]] =
//        for {
//          a <- makeVolatile(v(0))
//          b <- makeVolatile(v(1))
//        } yield List(a, b)
//
//      val withVolatiles = cases.flatMap(makeVolatiles)
//
//      // TODO: Do not filter out missing cases
//      withVolatiles
//        .map(_.mkString(" "))
//        .toSet
//    }
//
//    val testedMergeabilityCases =
//      (MergeabilityRules.baseMergeabilityCases ++ MergeabilityRules.peekMergeabilityCases)
//        .map(_._1)
//    withClue(s"""Missing cases: ${allMergeabilityCases
//      .diff(testedMergeabilityCases.toSet)
//      .toList
//      .sorted
//      .mkString(", ")}\n""") {
//      testedMergeabilityCases should contain allElementsOf allMergeabilityCases
//    }
//  }

}
