package coop.rchain.casper.addblock

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer.NoNewDeploys
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.{BlockUtil, TestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.Tools
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.models.PCost
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Base16
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

class MultiParentCasperAddBlockSpec extends AnyFlatSpec with Matchers with Inspectors {

  import InvalidBlock._
  import RSpaceUtil._
  import ValidBlock._
  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]
  implicit val s       = Sync[Task]
  val genesis          = buildGenesis()
  private val SHARD_ID = genesis.genesisBlock.shardId

  //put a new casper instance at the start of each
  //test since we cannot reset it
//  "MultiParentCasper" should "not allow multiple threads to process the same block" in {
//    val scheduler = Scheduler.fixedPool("three-threads", 3)
//    val testProgram =
//      TestNode.standaloneEff(genesis)(scheduler).use { node =>
//        val casper = node.casperEff
//        for {
//          deploy <- ConstructDeploy.basicDeployData[Effect](0)
//          _      <- casper.deploy(deploy)
//          block <- casper.createBlock.map {
//                    case Created(block) => block
//                    case _              => throw new RuntimeException()
//                  }
//          result <- Task
//                     .racePair(
//                       casper.addBlock(block),
//                       casper.addBlock(block)
//                     )
//                     .flatMap {
//                       case Left((statusA, running)) =>
//                         running.join.map((statusA, _))
//
//                       case Right((running, statusB)) =>
//                         running.join.map((_, statusB))
//                     }
//        } yield result
//      }
//    val threadStatuses: (ValidBlockProcessing, ValidBlockProcessing) =
//      testProgram.unsafeRunSync(scheduler)
//
//    threadStatuses should matchPattern {
//      case (Left(CasperIsBusy), Right(Valid)) | (Right(Valid), Left(CasperIsBusy)) =>
//    }
//  }

  it should "be able to create a chain of blocks from different deploys" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val rm = node.runtimeManager

      for {
        deploy1 <- ConstructDeploy.sourceDeployNowF(
                    "contract @\"add\"(@x, @y, ret) = { ret!(x + y) }",
                    shardId = SHARD_ID
                  )
        signedBlock1 <- node.addBlock(deploy1)
        deploy2 <- ConstructDeploy.sourceDeployNowF(
                    "new unforgable in { @\"add\"!(5, 7, *unforgable) }",
                    shardId = SHARD_ID
                  )
        signedBlock2 <- node.addBlock(deploy2)
        data <- getDataAtPrivateChannel[Effect](
                 signedBlock2,
                 Base16.encode(Tools.unforgeableNameRng(deploy2.pk, deploy2.data.timestamp).next())
               )
      } yield {
        data shouldBe Seq("12")
      }
    }
  }

  it should "allow multiple deploys in a single block" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val source = " for(@x <- @0){ @0!(x) } | @0!(0) "
      for {
        deploys <- List(source, source)
                    .traverse(ConstructDeploy.sourceDeployNowF[Effect](_, shardId = SHARD_ID))
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
                          i => ConstructDeploy.basicDeployData[Effect](i, shardId = SHARD_ID)
                        )
        _ <- nodes(0).addBlock(deployDatas(0))
        _ <- nodes(1).addBlock(deployDatas(1))
        _ <- nodes(1).handleReceive() // receive block1
        _ <- nodes(0).handleReceive() // receive block2

        status <- nodes(1).createBlock()
      } yield (assert(status == NoNewDeploys))
    }
  }

  it should "create valid blocks when peek syntax is present in a deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val source = " for(@x <<- @0){ Nil } | @0!(0) "
      for {
        deploy  <- ConstructDeploy.sourceDeployNowF[Effect](source, shardId = SHARD_ID)
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
                     "for(_ <<- @0) { Nil } | @0!(0) | for(_ <- @0) { Nil }",
                     shardId = SHARD_ID
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
        status          <- node.processBlock(invalidBlock)
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

        _ <- node0.processBlock(unsignedBlock)
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

  it should "reject blocks not from bonded validators" ignore effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]
      implicit val bds     = node.blockDagStorage

      for {
        basicDeployData  <- ConstructDeploy.basicDeployData[Effect](0)
        block            <- node.createBlockUnsafe(basicDeployData)
        dag              <- node.blockDagStorage.getRepresentation
        (sk, pk)         = Secp256k1.newKeyPair
        validatorId      = ValidatorIdentity(sk)
        sender           = ByteString.copyFrom(pk.bytes)
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
        illSignedBlock = validatorId
          .signBlock(block)
        status <- node.processBlock(illSignedBlock)
      } yield (status shouldBe Left(InvalidSender))
    }
  }

  it should "propose blocks it adds to peers" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData  <- ConstructDeploy.basicDeployData[Effect](0, shardId = SHARD_ID)
        signedBlock <- nodes(0).publishBlock(deployData)(nodes: _*)
        proposed    <- nodes(1).knowsAbout(signedBlock.blockHash)
      } yield proposed shouldBe true
    }
  }

  it should "add a valid block from peer" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData            <- ConstructDeploy.basicDeployData[Effect](1, shardId = SHARD_ID)
        signedBlock1Prime     <- nodes(0).publishBlock(deployData)(nodes: _*)
        _                     <- nodes(1).syncWith(nodes(0)) // should receive BlockMessage here
        maybeHash             <- nodes(1).blockStore.get1(signedBlock1Prime.blockHash)
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

  it should "ignore adding equivocation blocks" ignore effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        // Creates a pair that constitutes equivocation blocks
        basicDeployData0  <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock1      <- nodes(0).createBlockUnsafe(basicDeployData0)
        basicDeployData1  <- ConstructDeploy.basicDeployData[Effect](1)
        signedBlock1Prime <- nodes(0).createBlockUnsafe(basicDeployData1)

        _ <- nodes(0).processBlock(signedBlock1)
        _ <- nodes(0).processBlock(signedBlock1Prime)

        _ <- nodes(1).syncWith(nodes(0))

        _ <- nodes(1).contains(signedBlock1.blockHash) shouldBeF true
        _ <- nodes(1).contains(signedBlock1Prime.blockHash) shouldBeF false // we still add the equivocation pair
        _ <- nodes(1).blockStore.get1(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
        _ <- nodes(1).blockStore.get1(signedBlock1Prime.blockHash) shouldBeF None
      } yield ()
    }
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" ignore effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployDatas <- (0 to 5).toList
                        .traverse[Effect, Signed[DeployData]](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )

        // Creates a pair that constitutes equivocation blocks
        signedBlock1      <- nodes(0).createBlockUnsafe(deployDatas(0))
        signedBlock1Prime <- nodes(0).createBlockUnsafe(deployDatas(1))

        _ <- nodes(1).processBlock(signedBlock1)
        _ <- nodes(0).shutoff() //nodes(0) misses this block
        _ <- nodes(2).shutoff() //nodes(2) misses this block

        _ <- nodes(0).processBlock(signedBlock1Prime)
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

        _ <- nodes(0).contains(signedBlock1.blockHash) shouldBeF false
        _ <- nodes(0).contains(signedBlock1Prime.blockHash) shouldBeF true
        _ <- nodes(1).blockStore.get1(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
        _ <- nodes(1).blockStore.get1(signedBlock4.blockHash) shouldBeF Some(signedBlock4)
        _ <- nodes(2).blockStore.get1(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
        _ <- nodes(2).blockStore.get1(signedBlock1Prime.blockHash) shouldBeF Some(
              signedBlock1Prime
            )
      } yield ()
    }
  }

  it should "prepare to slash an block that includes a invalid block pointer" ignore effectTest {
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

        signedBlock <- nodes(0).createBlockUnsafe(deploys(0))
        signedInvalidBlock = BlockUtil.resignBlock(
          signedBlock.copy(seqNum = -2),
          nodes(0).validatorIdOpt.get.privateKey
        ) // Invalid seq num

        blockWithInvalidJustification <- buildBlockWithInvalidJustification(
                                          nodes,
                                          deploysWithCost,
                                          signedInvalidBlock
                                        )

        _ <- nodes(1).processBlock(blockWithInvalidJustification)
        _ <- nodes(0)
              .shutoff() // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock

        signedInvalidBlockPacketMessage = packet(
          nodes(0).local,
          "test",
          signedInvalidBlock.toProto
        )
        _ <- nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
        _ <- nodes(1).handleReceive() // receives signedInvalidBlock; attempts to add both blocks

        result = nodes(1).logEff.warns
          .count(_ startsWith "Recording invalid block") should be(1) // TODO: is this the only way that we can test it?
      } yield result
    }
  }

  it should "estimate parent properly" in effectTest {

    val validatorKeyPairs = (1 to 5).map(_ => Secp256k1.newKeyPair)
    val (_, validatorPks) = validatorKeyPairs.unzip

    def deployment(ts: Long) =
      ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", timestamp = ts, shardId = SHARD_ID)

    def create(
        node: TestNode[Effect]
    ): Effect[BlockMessage] =
      for {
        createBlockResult1 <- node.createBlockUnsafe()
      } yield createBlockResult1

    def add(node: TestNode[Effect], signed: BlockMessage) =
      Sync[Effect].attempt(
        node.processBlock(signed)
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
          _    <- v1.deploy(deployment(1)) >> create(v1) >>= (v1c1 => add(v1, v1c1)) //V1#1
          v2c1 <- v2.deploy(deployment(2)) >> create(v2) //V2#1
          _    <- v2.handleReceive()
          _    <- v3.handleReceive()
          _    <- v1.deploy(deployment(4)) >> create(v1) >>= (v1c2 => add(v1, v1c2)) //V1#2
          v3c2 <- v3.deploy(deployment(5)) >> create(v3) //V3#2
          _    <- v3.handleReceive()
          _    <- add(v3, v3c2) //V3#2
          _    <- add(v2, v2c1) //V2#1
          _    <- v3.handleReceive()
          r    <- v3.deploy(deployment(6)) >> create(v3) >>= (b => add(v3, b))
          _    = r shouldBe Right(Right(Valid))
        } yield ()
      }
  }

  it should "succeed at slashing" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployData <- ConstructDeploy
                       .basicDeployData[Effect](0, shardId = SHARD_ID)
        signedBlock  <- nodes(0).deploy(deployData) >> nodes(0).createBlockUnsafe()
        invalidBlock = signedBlock.copy(seqNum = 47)
        status1      <- nodes(1).processBlock(invalidBlock)
        status2      <- nodes(2).processBlock(invalidBlock)
        signedBlock2 <- nodes(1).createBlockUnsafe()
        status3      <- nodes(1).processBlock(signedBlock2)
        bonds        <- nodes(1).runtimeManager.computeBonds(ProtoUtil.postStateHash(signedBlock2))
        _            = bonds.map(_.stake).min should be(0) // Slashed validator has 0 stake
        _            <- nodes(2).handleReceive()
        signedBlock3 <- nodes(2).createBlockUnsafe()
        status4      <- nodes(2).processBlock(signedBlock3)
      } yield {
        status1 should be(Left(InvalidSequenceNumber))
        status2 should be(Left(InvalidSequenceNumber))
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
    val body      = Body(postState, deploys.toList, List.empty, List.empty)
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
    nodes(1).blockDagStorage.getRepresentation.flatMap { dag =>
      val sender       = blockThatPointsToInvalidBlock.sender
      implicit val bds = nodes(1).blockDagStorage
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
