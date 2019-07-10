package coop.rchain.casper

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.helper.{BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.transport
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}

import scala.collection.immutable

class MultiParentCasperAddBlockSpec extends FlatSpec with Matchers with Inspectors {

  import RSpaceUtil._
  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis(buildGenesisParameters())

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "not allow multiple threads to process the same block" in {
    val scheduler = Scheduler.fixedPool("three-threads", 3)
    val testProgram =
      HashSetCasperTestNode.standaloneEff(genesis)(scheduler).use { node =>
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
                       casper.addBlock(block, ignoreDoppelgangerCheck[Effect]),
                       casper.addBlock(block, ignoreDoppelgangerCheck[Effect])
                     )
                     .flatMap {
                       case Left((statusA, running)) =>
                         running.join.map((statusA, _))

                       case Right((running, statusB)) =>
                         running.join.map((_, statusB))
                     }
        } yield result
      }
    val threadStatuses: (BlockStatus, BlockStatus) =
      testProgram.unsafeRunSync(scheduler)

    threadStatuses should matchPattern { case (Processing, Valid) | (Valid, Processing) => }
  }

  it should "accept signed blocks" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy      <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock <- node.addBlock(deploy)
        _           = logEff.warns.isEmpty should be(true)
        dag         <- node.casperEff.blockDag
        estimate    <- node.casperEff.estimator(dag)
        _           = estimate shouldBe IndexedSeq(signedBlock.blockHash)
      } yield ()
    }
  }

  it should "be able to create a chain of blocks from different deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      implicit val rm = node.runtimeManager

      for {
        deploy1 <- ConstructDeploy
                    .sourceDeployNowF("contract @\"add\"(@x, @y, ret) = { ret!(x + y) }")
        signedBlock1 <- node.addBlock(deploy1)
        deploy2 <- ConstructDeploy
                    .sourceDeployNowF("new unforgable in { @\"add\"!(5, 7, *unforgable) }")
        signedBlock2 <- node.addBlock(deploy2)
        _            = ProtoUtil.parentHashes(signedBlock2) should be(Seq(signedBlock1.blockHash))
        dag          <- node.casperEff.blockDag
        estimate     <- node.casperEff.estimator(dag)
        _            = estimate shouldBe IndexedSeq(signedBlock2.blockHash)
        data <- getDataAtPrivateChannel[Effect](
                 signedBlock2,
                 Base16.encode(
                   RegistrySigGen.generateUnforgeableNameId(
                     PublicKey(deploy2.deployer.toByteArray),
                     deploy2.timestamp
                   )
                 )
               )
        _ = data shouldBe Seq("12")
      } yield ()
    }
  }

  it should "allow multiple deploys in a single block" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val source = " for(@x <- @0){ @0!(x) } | @0!(0) "
      for {
        deploys <- List(source, source).traverse(ConstructDeploy.sourceDeployNowF[Effect](_))
        block   <- node.addBlock(deploys: _*)
        result  <- node.casperEff.contains(block) shouldBeF true
      } yield result
    }
  }

  it should "reject unsigned blocks" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData <- ConstructDeploy.basicDeployData[Effect](0)
        block           <- node.createBlock(basicDeployData)
        invalidBlock    = block.withSig(ByteString.EMPTY)
        status          <- node.casperEff.addBlock(invalidBlock, ignoreDoppelgangerCheck[Effect])
        _               = status shouldBe InvalidUnslashableBlock
        _               <- node.casperEff.contains(invalidBlock) shouldBeF false
        _               = node.logEff.warns.head.contains("Ignoring block") should be(true)
      } yield ()
    }
  }

  it should "not request invalid blocks from peers" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        data0 <- ConstructDeploy.sourceDeployNowF("@0!(0)")
        data1 <- ConstructDeploy.sourceDeployNowF("@1!(1)")
        unsignedBlock <- node0
                          .createBlock(data0)
                          .map(_.copy(sigAlgorithm = "invalid", sig = ByteString.EMPTY))

        _ <- node0.casperEff.addBlock(unsignedBlock, ignoreDoppelgangerCheck[Effect])
        _ <- node1.transportLayerEff.clear(node1.local) //node1 misses this block

        signedBlock <- node0.addBlock(data1)
        _           <- node1.receive() //receives block1; should not ask for block0

        _ <- node0.casperEff.contains(unsignedBlock) shouldBeF false
        _ <- node1.casperEff.contains(unsignedBlock) shouldBeF false
      } yield ()
    }
  }

  it should "reject blocks not from bonded validators" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData         <- ConstructDeploy.basicDeployData[Effect](0)
        block                   <- node.createBlock(basicDeployData)
        dag                     <- node.blockDagStorage.getRepresentation
        (sk, pk)                = Secp256k1.newKeyPair
        illSignedBlock          <- ProtoUtil.signBlock(block, dag, pk, sk, Secp256k1.name, block.shardId)
        status                  <- node.casperEff.addBlock(illSignedBlock, ignoreDoppelgangerCheck[Effect])
        InvalidUnslashableBlock = status
        _                       = node.logEff.warns.head.contains("Ignoring block") should be(true)
      } yield ()
    }
  }

  it should "propose blocks it adds to peers" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData  <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock <- nodes(0).addBlock(deployData)
        _           <- nodes(1).receive()
        result      <- nodes(1).casperEff.contains(signedBlock) shouldBeF true
        _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
              node.blockStore.get(signedBlock.blockHash) shouldBeF Some(signedBlock)
            }
      } yield result
    }
  }

  it should "add a valid block from peer" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployData        <- ConstructDeploy.basicDeployData[Effect](1)
        signedBlock1Prime <- nodes(0).addBlock(deployData)
        _                 <- nodes(1).receive()
        _                 = nodes(1).logEff.infos.count(_ startsWith "Added") should be(1)
        result            = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(0)
        _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
              node.blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(signedBlock1Prime)
            }
      } yield result
    }
  }

  it should "reject addBlock when there exist deploy by the same (user, millisecond timestamp) in the chain" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        deployDatas <- (0 to 2).toList
                        .traverse[Effect, DeployData](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )
        deployPrim0 = ConstructDeploy.sign(
          deployDatas(1)
            .withTimestamp(deployDatas(0).timestamp)
            .withDeployer(deployDatas(0).deployer)
        ) // deployPrim0 has the same (user, millisecond timestamp) with deployDatas(0)
        signedBlock1 <- nodes(0).addBlock(deployDatas(0))
        _            <- nodes(1).receive() // receive block1

        signedBlock2 <- nodes(0).addBlock(deployDatas(1))
        _            <- nodes(1).receive() // receive block2

        signedBlock3 <- nodes(0).addBlock(deployDatas(2))
        _            <- nodes(1).receive() // receive block3

        _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true

        signedBlock4 <- nodes(1).addBlock(deployPrim0) // should succeed
        _            <- nodes(0).receive()             // still receive signedBlock4

        result <- nodes(1).casperEff
                   .contains(signedBlock4) shouldBeF true // Invalid blocks are still added
        // TODO: Fix with https://rchain.atlassian.net/browse/RHOL-1048
        // nodes(0).casperEff.contains(signedBlock4) should be(false)
        //
        // nodes(0).logEff.warns
        //   .count(_ contains "found deploy by the same (user, millisecond timestamp) produced") should be(
        //   1
        // )
        _ = nodes.toList.traverse_[Effect, Assertion] { node =>
          for {
            _      <- node.blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
            _      <- node.blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
            result <- node.blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
          } yield result
        }
      } yield result
    }
  }

  it should "ignore adding equivocation blocks" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      for {
        // Creates a pair that constitutes equivocation blocks
        basicDeployData0  <- ConstructDeploy.basicDeployData[Effect](0)
        signedBlock1      <- nodes(0).createBlock(basicDeployData0)
        basicDeployData1  <- ConstructDeploy.basicDeployData[Effect](1)
        signedBlock1Prime <- nodes(0).createBlock(basicDeployData1)

        _ <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(1).receive()
        _ <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(1).receive()

        _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
        result <- nodes(1).casperEff
                   .contains(signedBlock1Prime) shouldBeF false // we still add the equivocation pair
        _ <- nodes(1).blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
        _ <- nodes(1).blockStore.get(signedBlock1Prime.blockHash) shouldBeF None
      } yield result
    }
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployDatas <- (0 to 5).toList
                        .traverse[Effect, DeployData](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )

        // Creates a pair that constitutes equivocation blocks
        signedBlock1      <- nodes(0).createBlock(deployDatas(0))
        signedBlock1Prime <- nodes(0).createBlock(deployDatas(1))

        _ <- nodes(1).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(0).transportLayerEff.clear(nodes(0).local) //nodes(0) misses this block
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

        _ <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(2).receive()
        _ <- nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block

        _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
        _ <- nodes(2).casperEff.contains(signedBlock1) shouldBeF false

        _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF false
        _ <- nodes(2).casperEff.contains(signedBlock1Prime) shouldBeF true

        signedBlock2 <- nodes(1).addBlock(deployDatas(2))
        signedBlock3 <- nodes(2).addBlock(deployDatas(3))
        _            <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) ignores block2
        _            <- nodes(1).receive() // receives block3; asks for block1'
        _            <- nodes(2).receive() // receives request for block1'; sends block1'
        _            <- nodes(1).receive() // receives block1'; adds both block3 and block1'

        _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true
        _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

        signedBlock4 <- nodes(1).addBlock(deployDatas(4))

        // Node 1 should contain both blocks constituting the equivocation
        _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
        _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

        _ <- nodes(1).casperEff
              .contains(signedBlock4) shouldBeF true // However, marked as invalid

        _ = nodes(1).logEff.warns.exists(
          _.matches("Recording invalid block .* for AdmissibleEquivocation.")
        ) should be(true)
        _ = nodes(2).logEff.warns.size should be(0)
        _ = nodes(1).logEff.warns.size should be(3)
        _ = nodes(0).logEff.warns.size should be(0)

        _ <- nodes(1).casperEff
              .normalizedInitialFault(ProtoUtil.weightMap(genesis.genesisBlock)) shouldBeF 1f / (1f + 3f + 5f + 7f)
        _ <- nodes(0).blockStore.get(signedBlock1.blockHash) shouldBeF None
        _ <- nodes(0).blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
              signedBlock1Prime
            )
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
    HashSetCasperTestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deploys         <- (0 to 5).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))
        deploysWithCost = deploys.map(d => ProcessedDeploy(deploy = Some(d))).toIndexedSeq

        signedBlock <- nodes(0).createBlock(deploys(0))
        signedInvalidBlock = BlockUtil.resignBlock(
          signedBlock.withSeqNum(-2),
          nodes(0).validatorId.privateKey
        ) // Invalid seq num

        blockWithInvalidJustification <- buildBlockWithInvalidJustification(
                                          nodes,
                                          deploysWithCost,
                                          signedInvalidBlock
                                        )

        _ <- nodes(1).casperEff
              .addBlock(blockWithInvalidJustification, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(0).transportLayerEff
              .clear(nodes(0).local) // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock

        signedInvalidBlockPacketMessage = packet(
          nodes(0).local,
          "test",
          transport.BlockMessage,
          signedInvalidBlock.toByteString
        )
        _ <- nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
        _ <- nodes(1).receive() // receives signedInvalidBlock; attempts to add both blocks

        result = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(1)
      } yield result
    }
  }

  it should "estimate parent properly" in effectTest {

    val validatorKeyPairs = (1 to 5).map(_ => Secp256k1.newKeyPair)
    val (_, validatorPks) = validatorKeyPairs.unzip

    def deployment(ts: Long) =
      ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", timestamp = ts)

    def deploy(
        node: HashSetCasperTestNode[Effect],
        dd: DeployData
    ) = node.casperEff.deploy(dd)

    def create(
        node: HashSetCasperTestNode[Effect]
    ) =
      for {
        createBlockResult1    <- node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
      } yield signedBlock1

    def add(node: HashSetCasperTestNode[Effect], signed: BlockMessage) =
      Sync[Effect].attempt(
        node.casperEff.addBlock(signed, ignoreDoppelgangerCheck[Effect])
      )

    HashSetCasperTestNode
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
          _    = r shouldBe Right(Valid)
          _    = v3.logEff.warns shouldBe empty
        } yield ()
      }
  }

  it should "succeed at slashing" in effectTest {
    HashSetCasperTestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      for {
        deployData            <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult     <- nodes(0).casperEff.deploy(deployData) >> nodes(0).casperEff.createBlock
        Created(signedBlock)  = createBlockResult
        invalidBlock          = signedBlock.withSeqNum(47)
        status1               <- nodes(1).casperEff.addBlock(invalidBlock, ignoreDoppelgangerCheck[Effect])
        status2               <- nodes(2).casperEff.addBlock(invalidBlock, ignoreDoppelgangerCheck[Effect])
        createBlockResult2    <- nodes(1).casperEff.createBlock
        Created(signedBlock2) = createBlockResult2
        status3               <- nodes(1).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
        bonds                 <- nodes(1).runtimeManager.computeBonds(ProtoUtil.postStateHash(signedBlock2))
        _                     = bonds.map(_.stake).min should be(0) // Slashed validator has 0 stake
        _                     <- nodes(2).receive()
        createBlockResult3    <- nodes(2).casperEff.createBlock
        Created(signedBlock3) = createBlockResult3
        status4               <- nodes(2).casperEff.addBlock(signedBlock3, ignoreDoppelgangerCheck[Effect])
        _                     = status1 should be(InvalidBlockHash)
        _                     = status2 should be(InvalidBlockHash)
        _                     = status3 should be(Valid)
        _                     = status4 should be(Valid)
        // TODO: assert no effect as already slashed
      } yield ()
    }
  }

  private def buildBlockWithInvalidJustification(
      nodes: IndexedSeq[HashSetCasperTestNode[Effect]],
      deploys: immutable.IndexedSeq[ProcessedDeploy],
      signedInvalidBlock: BlockMessage
  ): Effect[BlockMessage] = {
    val postState =
      RChainState().withBonds(ProtoUtil.bonds(genesis.genesisBlock)).withBlockNumber(1)
    val postStateHash = Blake2b256.hash(postState.toByteArray)
    val header = Header()
      .withPostStateHash(ByteString.copyFrom(postStateHash))
      .withParentsHashList(signedInvalidBlock.header.get.parentsHashList)
      .withDeploysHash(ProtoUtil.protoSeqHash(deploys))
    val blockHash = Blake2b256.hash(header.toByteArray)
    val body      = Body().withState(postState).withDeploys(deploys)
    val serializedJustifications =
      Seq(Justification(signedInvalidBlock.sender, signedInvalidBlock.blockHash))
    val serializedBlockHash = ByteString.copyFrom(blockHash)
    val blockThatPointsToInvalidBlock =
      BlockMessage(serializedBlockHash, Some(header), Some(body), serializedJustifications)
    nodes(1).casperEff.blockDag.flatMap { dag =>
      ProtoUtil.signBlock[Effect](
        blockThatPointsToInvalidBlock,
        dag,
        defaultValidatorPks(1),
        defaultValidatorSks(1),
        "secp256k1",
        "rchain"
      )
    }
  }
}
