package coop.rchain.casper.addblock

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper._
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.{BlockUtil, TestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.models.PCost
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.collection.immutable

class MultiParentCasperAddBlockSpec extends FlatSpec with Matchers with Inspectors {

  import BlockError._
  import InvalidBlock._
  import RSpaceUtil._
  import ValidBlock._
  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "not allow multiple threads to process the same block" in {
    val scheduler = Scheduler.fixedPool("three-threads", 3)
    val testProgram =
      TestNode.standaloneEff(genesis)(scheduler).use { node =>
        val casper = node.casperEff
        for {
          deploy <- ConstructDeploy.basicDeployData[Effect](0)
          _      <- casper.deploy(deploy)
          block <- casper.createBlock.map {
                    case Created(block) => block
                    case _              => throw new RuntimeException()
                  }
          result <- Task
                     .racePair(
                       casper.addBlock(block),
                       casper.addBlock(block)
                     )
                     .flatMap {
                       case Left((statusA, running)) =>
                         running.join.map((statusA, _))

                       case Right((running, statusB)) =>
                         running.join.map((_, statusB))
                     }
        } yield result
      }
    val threadStatuses: (ValidBlockProcessing, ValidBlockProcessing) =
      testProgram.unsafeRunSync(scheduler)

    threadStatuses should matchPattern {
      case (Left(CasperIsBusy), Right(Valid)) | (Right(Valid), Left(CasperIsBusy)) =>
    }
  }

  it should "accept signed blocks" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy      <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock <- node.addBlock(deploy)
        dag         <- node.casperEff.blockDag
        estimate    <- node.casperEff.estimator(dag)
      } yield (estimate shouldBe IndexedSeq(signedBlock.blockHash))
    }
  }

  it should "be able to create a chain of blocks from different deploys" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val rm = node.runtimeManager

      for {
        deploy1 <- ConstructDeploy
                    .sourceDeployNowF("contract @\"add\"(@x, @y, ret) = { ret!(x + y) }")
        signedBlock1 <- node.addBlock(deploy1)
        deploy2 <- ConstructDeploy
                    .sourceDeployNowF("new unforgable in { @\"add\"!(5, 7, *unforgable) }")
        signedBlock2 <- node.addBlock(deploy2)
        dag          <- node.casperEff.blockDag
        estimate     <- node.casperEff.estimator(dag)
        data <- getDataAtPrivateChannel[Effect](
                 signedBlock2,
                 Base16.encode(
                   RegistrySigGen.generateUnforgeableNameId(
                     deploy2.pk,
                     deploy2.data.timestamp
                   )
                 )
               )
      } yield {
        ProtoUtil.parentHashes(signedBlock2) should be(Seq(signedBlock1.blockHash))
        estimate shouldBe IndexedSeq(signedBlock2.blockHash)
        data shouldBe Seq("12")
      }
    }
  }

  it should "allow multiple deploys in a single block" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val source = " for(@x <- @0){ @0!(x) } | @0!(0) "
      for {
        deploys  <- List(source, source).traverse(ConstructDeploy.sourceDeployNowF[Effect](_))
        block    <- node.addBlock(deploys: _*)
        deployed <- node.contains(block.blockHash)
      } yield deployed shouldBe true
    }
  }

  it should "not allow empty blocks with multiple parents" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployDatas <- (0 to 1).toList
                        .traverse[Effect, Signed[DeployData]](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )
        _ <- nodes(0).addBlock(deployDatas(0))
        _ <- nodes(1).addBlock(deployDatas(1))
        _ <- nodes(1).receive() // receive block1
        _ <- nodes(0).receive() // receive block2

        status <- nodes(1).casperEff.createBlock
      } yield (assert(status == NoNewDeploys))
    }
  }

  it should "create valid blocks when peek syntax is present in a deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val source = " for(@x <<- @0){ Nil } | @0!(0) "
      for {
        deploy  <- ConstructDeploy.sourceDeployNowF[Effect](source)
        block   <- node.addBlock(deploy)
        created <- node.contains(block.blockHash)
      } yield created shouldBe true
    }
  }

  it should "propose and replay peek" in effectTest {
    (1 to 50).toList.map { _ =>
      TestNode.networkEff(genesis, networkSize = 1).use { nodes =>
        for {
          deploy <- ConstructDeploy.sourceDeployNowF[Effect](
                     "for(_ <<- @0) { Nil } | @0!(0) | for(_ <- @0) { Nil }"
                   )
          block <- nodes(0).addBlock(deploy)
          added <- nodes(0).contains(block.blockHash)
        } yield added shouldBe true
      }
    }.parSequence_
  }

  //This test are not valid anymore because we have check for block validity before sending to Casper.
  //TODO move to Running tests
  /*
  it should "reject unsigned blocks" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData <- ConstructDeploy.basicDeployData[Effect](0)
        block           <- node.createBlock(basicDeployData)
        invalidBlock    = block.copy(sig = ByteString.EMPTY)
        status          <- node.casperEff.addBlock(invalidBlock)
        added           <- node.contains(invalidBlock.blockHash)
      } yield {
        status shouldBe Left(InvalidFormat)
        added shouldBe false
      }
    }
  }

  it should "not request invalid blocks from peers" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        data0 <- ConstructDeploy.sourceDeployNowF("@0!(0)")
        data1 <- ConstructDeploy.sourceDeployNowF("@1!(1)")
        unsignedBlock <- node0
                          .createBlock(data0)
                          .map(_.copy(sigAlgorithm = "invalid", sig = ByteString.EMPTY))

        _ <- node0.casperEff.addBlock(unsignedBlock)
        _ <- node1.shutoff() //node1 misses this block

        signedBlock <- node0.addBlock(data1)
        _           <- node1.syncWith(node0) //receives block1; should not ask for block0

        node0ContainsUnsigned <- node0.contains(unsignedBlock.blockHash)
        node1ContainsUnsigned <- node1.contains(unsignedBlock.blockHash)
      } yield {
        node0ContainsUnsigned shouldBe false
        node1ContainsUnsigned shouldBe false
      }
    }
  }
   */

  it should "reject blocks not from bonded validators" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData  <- ConstructDeploy.basicDeployData[Effect](0)
        block            <- node.createBlock(basicDeployData)
        dag              <- node.blockDagStorage.getRepresentation
        (sk, pk)         = Secp256k1.newKeyPair
        validatorId      = ValidatorIdentity(sk)
        sender           = ByteString.copyFrom(pk.bytes)
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
        illSignedBlock = validatorId
          .signBlock(block)
        status <- node.casperEff.addBlock(illSignedBlock)
      } yield (status shouldBe Left(InvalidSender))
    }
  }

  it should "propose blocks it adds to peers" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData  <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock <- nodes(0).publishBlock(deployData)(nodes: _*)
        proposed    <- nodes(1).knowsAbout(signedBlock.blockHash)
      } yield proposed shouldBe true
    }
  }

  it should "add a valid block from peer" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData            <- ConstructDeploy.basicDeployData[Effect](1)
        signedBlock1Prime     <- nodes(0).publishBlock(deployData)(nodes: _*)
        _                     <- nodes(1).syncWith(nodes(0)) // should receive BlockMessage here
        maybeHash             <- nodes(1).blockStore.get(signedBlock1Prime.blockHash)
        noMoreRequestedBlocks <- nodes(1).requestedBlocks.get.map(!_.exists(_._2.received == false))
      } yield {
        maybeHash shouldBe Some(signedBlock1Prime)
        noMoreRequestedBlocks shouldBe true
      }
    }
  }

  it should "reject addBlock when there exist deploy by the same (user, millisecond timestamp) in the chain" ignore effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployDatas <- (0 to 2).toList
                        .traverse[Effect, Signed[DeployData]](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )
        deployPrim0 = Signed(
          deployDatas(1).data
            .copy(
              timestamp = deployDatas(0).data.timestamp
            ),
          Secp256k1,
          ConstructDeploy.defaultSec
        ) // deployPrim0 has the same (user, millisecond timestamp) with deployDatas(0)

        signedBlock1 <- nodes(0).publishBlock(deployDatas(0))(nodes: _*)
        signedBlock2 <- nodes(0).publishBlock(deployDatas(1))(nodes: _*)
        signedBlock3 <- nodes(0).publishBlock(deployDatas(2))(nodes: _*)

        _ <- nodes(1).knowsAbout(signedBlock3.blockHash) shouldBeF true

        signedBlock4 <- nodes(1).publishBlock(deployPrim0)(nodes: _*)
        _            <- nodes(1) contains (signedBlock4.blockHash) shouldBeF true
        // Invalid blocks are still added
        // TODO: Fix with https://rchain.atlassian.net/browse/RHOL-1048
        //TODO: ticket is closed but this test will not pass, investigate further
        _ <- nodes(0).syncWith(nodes(1))
        _ <- nodes(0).contains(signedBlock4.blockHash) shouldBeF false
      } yield ()
    }
  }

  it should "ignore adding equivocation blocks" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        // Creates a pair that constitutes equivocation blocks
        basicDeployData0  <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock1      <- nodes(0).createBlock(basicDeployData0)
        basicDeployData1  <- ConstructDeploy.basicDeployData[Effect](1)
        signedBlock1Prime <- nodes(0).createBlock(basicDeployData1)

        _ <- nodes(0).casperEff.addBlock(signedBlock1)
        _ <- nodes(0).casperEff.addBlock(signedBlock1Prime)

        _ <- nodes(1).syncWith(nodes(0))

        _ <- nodes(1).contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(1).contains(signedBlock1Prime.blockHash) shouldBeF false // we still add the equivocation pair
        _ <- nodes(1).blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
        _ <- nodes(1).blockStore.get(signedBlock1Prime.blockHash) shouldBeF None
      } yield ()
    }
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployDatas <- (0 to 5).toList
                        .traverse[Effect, Signed[DeployData]](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )

        // Creates a pair that constitutes equivocation blocks
        signedBlock1      <- nodes(0).createBlock(deployDatas(0))
        signedBlock1Prime <- nodes(0).createBlock(deployDatas(1))

        _ <- nodes(1).casperEff.addBlock(signedBlock1)
        _ <- nodes(0).shutoff() //nodes(0) misses this block
        _ <- nodes(2).shutoff() //nodes(2) misses this block

        _ <- nodes(0).casperEff.addBlock(signedBlock1Prime)
        _ <- nodes(2).syncWith(nodes(0))
        _ <- nodes(1).shutoff() //nodes(1) misses this block

        _ <- nodes(1).contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(2).knowsAbout(signedBlock1.blockHash) shouldBeF false

        _ <- nodes(1).knowsAbout(signedBlock1Prime.blockHash) shouldBeF false
        _ <- nodes(2).contains(signedBlock1Prime.blockHash) shouldBeF true

        signedBlock2 <- nodes(1).addBlock(deployDatas(2))
        signedBlock3 <- nodes(2).addBlock(deployDatas(3))
        _            <- nodes(2).shutoff() //nodes(2) ignores block2

        _ <- nodes(1).syncWith(nodes(2))
        // 1 receives block3 hash; asks 2 for block3
        // 2 responds with block3
        // 1 receives block3; asks if has block1'
        // 2 receives request has block1'; sends i have block1'
        // 1 receives has block1 ack; asks for block1'
        // 2 receives request block1'; sends block1'
        // 1 receives block1'; adds both block3 and block1'

        _ <- nodes(1).contains(signedBlock3.blockHash) shouldBeF true
        _ <- nodes(1).contains(signedBlock1Prime.blockHash) shouldBeF true

        signedBlock4 <- nodes(1).addBlock(deployDatas(4))

        // Node 1 should contain both blocks constituting the equivocation
        _ <- nodes(1).contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(1).contains(signedBlock1Prime.blockHash) shouldBeF true

        _ <- nodes(1).contains(signedBlock4.blockHash) shouldBeF true // However, marked as invalid

        _ <- nodes(1).casperEff
              .normalizedInitialFault(ProtoUtil.weightMap(genesis.genesisBlock)) shouldBeF 1f / (1f + 3f + 5f + 7f)
        _ <- nodes(0).casperEff.contains(signedBlock1.blockHash) shouldBeF false
        _ <- nodes(0).casperEff.contains(signedBlock1Prime.blockHash) shouldBeF true
        _ <- nodes(1).blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
        _ <- nodes(1).blockStore.get(signedBlock4.blockHash) shouldBeF Some(signedBlock4)
        _ <- nodes(2).blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
        _ <- nodes(2).blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
              signedBlock1Prime
            )
      } yield ()
    }
  }

  it should "prepare to slash an block that includes a invalid block pointer" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deploys <- (0 to 5).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))
        deploysWithCost = deploys
          .map(
            d =>
              ProcessedDeploy(
                deploy = d,
                cost = PCost(0L),
                List.empty,
                isFailed = false
              )
          )
          .toIndexedSeq

        signedBlock <- nodes(0).createBlock(deploys(0))
        signedInvalidBlock = BlockUtil.resignBlock(
          signedBlock.copy(seqNum = -2),
          nodes(0).validatorId.get.privateKey
        ) // Invalid seq num

        blockWithInvalidJustification <- buildBlockWithInvalidJustification(
                                          nodes,
                                          deploysWithCost,
                                          signedInvalidBlock
                                        )

        _ <- nodes(1).casperEff
              .addBlock(blockWithInvalidJustification)
        _ <- nodes(0)
              .shutoff() // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock

        signedInvalidBlockPacketMessage = packet(
          nodes(0).local,
          "test",
          signedInvalidBlock.toProto
        )
        _ <- nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
        _ <- nodes(1).receive() // receives signedInvalidBlock; attempts to add both blocks

        result = nodes(1).logEff.warns
          .count(_ startsWith "Recording invalid block") should be(1) // TODO: is this the only way that we can test it?
      } yield result
    }
  }

  it should "estimate parent properly" in effectTest {

    val validatorKeyPairs = (1 to 5).map(_ => Secp256k1.newKeyPair)
    val (_, validatorPks) = validatorKeyPairs.unzip

    def deployment(ts: Long) =
      ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", timestamp = ts)

    def deploy(
        node: TestNode[Effect],
        dd: Signed[DeployData]
    ) = node.casperEff.deploy(dd)

    def create(
        node: TestNode[Effect]
    ) =
      for {
        createBlockResult1    <- node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
      } yield signedBlock1

    def add(node: TestNode[Effect], signed: BlockMessage) =
      Sync[Effect].attempt(
        node.casperEff.addBlock(signed)
      )

    TestNode
      .networkEff(
        buildGenesis(
          buildGenesisParameters(
            validatorKeyPairs,
            Map(
              validatorPks(0) -> 3L,
              validatorPks(1) -> 1L,
              validatorPks(2) -> 5L,
              validatorPks(3) -> 2L,
              validatorPks(4) -> 4L
            )
          )
        ),
        networkSize = 3
      )
      .map(_.toList)
      .use { nodes =>
        val v1 = nodes(0)
        val v2 = nodes(1)
        val v3 = nodes(2)
        for {
          _    <- deploy(v1, deployment(1)) >> create(v1) >>= (v1c1 => add(v1, v1c1)) //V1#1
          v2c1 <- deploy(v2, deployment(2)) >> create(v2) //V2#1
          _    <- v2.receive()
          _    <- v3.receive()
          _    <- deploy(v1, deployment(4)) >> create(v1) >>= (v1c2 => add(v1, v1c2)) //V1#2
          v3c2 <- deploy(v3, deployment(5)) >> create(v3) //V3#2
          _    <- v3.receive()
          _    <- add(v3, v3c2) //V3#2
          _    <- add(v2, v2c1) //V2#1
          _    <- v3.receive()
          r    <- deploy(v3, deployment(6)) >> create(v3) >>= (b => add(v3, b))
          _    = r shouldBe Right(Right(Valid))
        } yield ()
      }
  }

  it should "succeed at slashing" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployData            <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult     <- nodes(0).casperEff.deploy(deployData) >> nodes(0).casperEff.createBlock
        Created(signedBlock)  = createBlockResult
        invalidBlock          = signedBlock.copy(seqNum = 47)
        status1               <- nodes(1).casperEff.addBlock(invalidBlock)
        status2               <- nodes(2).casperEff.addBlock(invalidBlock)
        createBlockResult2    <- nodes(1).casperEff.createBlock
        Created(signedBlock2) = createBlockResult2
        status3               <- nodes(1).casperEff.addBlock(signedBlock2)
        bonds                 <- nodes(1).runtimeManager.computeBonds(ProtoUtil.postStateHash(signedBlock2))
        _                     = bonds.map(_.stake).min should be(0) // Slashed validator has 0 stake
        _                     <- nodes(2).receive()
        createBlockResult3    <- nodes(2).casperEff.createBlock
        Created(signedBlock3) = createBlockResult3
        status4               <- nodes(2).casperEff.addBlock(signedBlock3)
      } yield {
        status1 should be(Left(InvalidBlockHash))
        status2 should be(Left(InvalidBlockHash))
        status3 should be(Right(Valid))
        status4 should be(Right(Valid))
        // TODO: assert no effect as already slashed
      }
    }
  }

  private def buildBlockWithInvalidJustification(
      nodes: IndexedSeq[TestNode[Effect]],
      deploys: immutable.IndexedSeq[ProcessedDeploy],
      signedInvalidBlock: BlockMessage
  ): Effect[BlockMessage] = {
    val postState: RChainState =
      RChainState(
        preStateHash = ByteString.EMPTY,
        postStateHash = ByteString.EMPTY,
        bonds = ProtoUtil.bonds(genesis.genesisBlock).toList,
        blockNumber = 1
      )
    val header = Header(
      parentsHashList = signedInvalidBlock.header.parentsHashList,
      timestamp = 0L,
      version = 0L
    )
    val blockHash = Blake2b256.hash(header.toProto.toByteArray)
    val body      = Body(postState, deploys.toList, List.empty)
    val serializedJustifications =
      List(Justification(signedInvalidBlock.sender, signedInvalidBlock.blockHash))
    val serializedBlockHash = ByteString.copyFrom(blockHash)
    val blockThatPointsToInvalidBlock =
      BlockMessage(
        serializedBlockHash,
        header,
        body,
        serializedJustifications,
        sender = ByteString.EMPTY,
        seqNum = 0,
        sig = ByteString.EMPTY,
        sigAlgorithm = "",
        shardId = "root"
      )
    nodes(1).casperEff.blockDag.flatMap { dag =>
      val sender = blockThatPointsToInvalidBlock.sender
      for {
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
        block = ValidatorIdentity(defaultValidatorSks(1)).signBlock(
          blockThatPointsToInvalidBlock
        )
      } yield block
    }
  }
}
