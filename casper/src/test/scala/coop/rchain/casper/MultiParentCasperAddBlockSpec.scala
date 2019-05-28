package coop.rchain.casper

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.helper.{BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.transport
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}

import scala.collection.immutable

class MultiParentCasperAddBlockSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._
  import RSpaceUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, createBonds(validatorPks))
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "not allow multiple threads to process the same block" in {
    val scheduler = Scheduler.fixedPool("three-threads", 3)
    val testProgram =
      HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)(scheduler).use { node =>
        val casper = node.casperEff
        for {
          deploy <- ConstructDeploy.basicDeployData[Effect](0)
          _      <- casper.deploy(deploy)
          block <- casper.createBlock.map {
                    case Created(block) => block
                    case _              => throw new RuntimeException()
                  }
          result <- EitherT(
                     Task
                       .racePair(
                         casper.addBlock(block, ignoreDoppelgangerCheck[Effect]).value,
                         casper.addBlock(block, ignoreDoppelgangerCheck[Effect]).value
                       )
                       .flatMap {
                         case Left((statusA, running)) =>
                           running.join.map((statusA, _).tupled)

                         case Right((running, statusB)) =>
                           running.join.map((_, statusB).tupled)
                       }
                   )
        } yield result
      }
    val threadStatuses: (BlockStatus, BlockStatus) =
      testProgram.value.unsafeRunSync(scheduler).right.get

    threadStatuses should matchPattern { case (Processing, Valid) | (Valid, Processing) => }
  }

  it should "accept signed blocks" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy               <- ConstructDeploy.basicDeployData[Effect](0)
        _                    <- MultiParentCasper[Effect].deploy(deploy)
        createBlockResult    <- MultiParentCasper[Effect].createBlock
        Created(signedBlock) = createBlockResult
        _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        _                    = logEff.warns.isEmpty should be(true)
        dag                  <- MultiParentCasper[Effect].blockDag
        estimate             <- MultiParentCasper[Effect].estimator(dag)
        _                    = estimate shouldBe IndexedSeq(signedBlock.blockHash)
      } yield ()
    }
  }

  it should "be able to create a chain of blocks from different deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { implicit node =>
      import node._
      implicit val rm = node.runtimeManager

      val start = 0L

      val deployDatas = Vector(
        "contract @\"add\"(@x, @y, ret) = { ret!(x + y) }",
        "new unforgable, stdout(`rho:io:stdout`) in { @\"add\"!(5, 7, *unforgable) | stdout!([\"hi\", *unforgable]) }"
      ).zipWithIndex
        .map(s => ConstructDeploy.sourceDeploy(s._1, start + s._2, accounting.MAX_VALUE))

      for {
        createBlockResult1 <- MultiParentCasper[Effect]
                               .deploy(deployDatas.head) *> MultiParentCasper[
                               Effect
                             ].createBlock
        Created(signedBlock1) = createBlockResult1
        _                     <- MultiParentCasper[Effect].addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
        createBlockResult2 <- MultiParentCasper[Effect].deploy(deployDatas(1)) *> MultiParentCasper[
                               Effect
                             ].createBlock
        Created(signedBlock2) = createBlockResult2
        _                     <- MultiParentCasper[Effect].addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
        storage               <- blockTuplespaceContents(signedBlock2)

        _        = logEff.warns should be(Nil)
        _        = ProtoUtil.parentHashes(signedBlock2) should be(Seq(signedBlock1.blockHash))
        dag      <- MultiParentCasper[Effect].blockDag
        estimate <- MultiParentCasper[Effect].estimator(dag)
        _        = estimate shouldBe IndexedSeq(signedBlock2.blockHash)
        // channel is deterministic because of the fixed timestamp
        data <- getDataAtPrivateChannel[Effect](
                 signedBlock2,
                 "ad0dd958a6acf8e58c2ecfdbf5f23b3c5beb74a7091c21a10c79cbb0b591872d"
               )
        _ = data shouldBe Seq("12")
      } yield ()
    }
  }

  it should "allow multiple deploys in a single block" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._

      val startTime = System.currentTimeMillis()
      val source    = " for(@x <- @0){ @0!(x) } | @0!(0) "
      val deploys = (source #:: source #:: Stream.empty[String]).zipWithIndex
        .map(s => ConstructDeploy.sourceDeploy(s._1, startTime + s._2, accounting.MAX_VALUE))
      for {
        _                 <- deploys.traverse_(MultiParentCasper[Effect].deploy(_))
        createBlockResult <- MultiParentCasper[Effect].createBlock
        Created(block)    = createBlockResult
        _                 <- MultiParentCasper[Effect].addBlock(block, ignoreDoppelgangerCheck[Effect])
        result            <- MultiParentCasper[Effect].contains(block) shouldBeF true
      } yield result
    }
  }

  it should "reject unsigned blocks" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult <- MultiParentCasper[Effect].deploy(basicDeployData) *> MultiParentCasper[
                              Effect
                            ].createBlock
        Created(block) = createBlockResult
        invalidBlock   = block.withSig(ByteString.EMPTY)
        status         <- MultiParentCasper[Effect].addBlock(invalidBlock, ignoreDoppelgangerCheck[Effect])
        _              = status shouldBe InvalidUnslashableBlock
        _              <- node.casperEff.contains(invalidBlock) shouldBeF false
        _              = logEff.warns.head.contains("Ignoring block") should be(true)
      } yield ()
    }
  }

  it should "not request invalid blocks from peers" in effectTest {
    val List(data0, data1) =
      (0 to 1)
        .map(i => ConstructDeploy.sourceDeploy(s"@$i!($i)", i.toLong, accounting.MAX_VALUE))
        .toList
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        unsignedBlock <- (node0.casperEff.deploy(data0) *> node0.casperEff.createBlock)
                          .map {
                            case Created(block) =>
                              block.copy(sigAlgorithm = "invalid", sig = ByteString.EMPTY)
                            case _ => throw new RuntimeException()
                          }

        _ <- node0.casperEff.addBlock(unsignedBlock, ignoreDoppelgangerCheck[Effect])
        _ <- node1.transportLayerEff.clear(node1.local) //node1 misses this block

        signedBlock <- (node0.casperEff.deploy(data1) *> node0.casperEff.createBlock)
                        .map {
                          case Created(block) => block
                          case _              => throw new RuntimeException()
                        }

        _ <- node0.casperEff.addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        _ <- node1.receive() //receives block1; should not ask for block0

        _ <- node0.casperEff.contains(unsignedBlock) shouldBeF false
        _ <- node1.casperEff.contains(unsignedBlock) shouldBeF false
      } yield ()
    }
  }

  it should "reject blocks not from bonded validators" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, Secp256k1.newKeyPair._1).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        basicDeployData <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult <- MultiParentCasper[Effect].deploy(basicDeployData) *> MultiParentCasper[
                              Effect
                            ].createBlock
        Created(signedBlock) = createBlockResult
        _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        _                    = logEff.warns.head.contains("Ignoring block") should be(true)
      } yield ()
    }
  }

  it should "propose blocks it adds to peers" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
      for {
        deployData           <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult    <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
        Created(signedBlock) = createBlockResult
        _                    <- nodes(0).casperEff.addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        _                    <- nodes(1).receive()
        result               <- nodes(1).casperEff.contains(signedBlock) shouldBeF true
        _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
              validateBlockStore(node) { blockStore =>
                blockStore.get(signedBlock.blockHash) shouldBeF Some(signedBlock)
              }(nodes(0).metricEff, nodes(0).logEff)
            }
      } yield result
    }
  }

  it should "add a valid block from peer" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
      for {
        deployData                 <- ConstructDeploy.basicDeployData[Effect](1)
        createBlockResult          <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
        Created(signedBlock1Prime) = createBlockResult
        _                          <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
        _                          <- nodes(1).receive()
        _                          = nodes(1).logEff.infos.count(_ startsWith "Added") should be(1)
        result                     = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(0)
        _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
              validateBlockStore(node) { blockStore =>
                blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(signedBlock1Prime)
              }(nodes(0).metricEff, nodes(0).logEff)
            }
      } yield result
    }
  }

  it should "reject addBlock when there exist deploy by the same (user, millisecond timestamp) in the chain" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
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
        createBlockResult1 <- nodes(0).casperEff
                               .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
        _                     <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
        _                     <- nodes(1).receive() // receive block1

        createBlockResult2 <- nodes(0).casperEff
                               .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
        Created(signedBlock2) = createBlockResult2
        _                     <- nodes(0).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
        _                     <- nodes(1).receive() // receive block2

        createBlockResult3 <- nodes(0).casperEff
                               .deploy(deployDatas(2)) *> nodes(0).casperEff.createBlock
        Created(signedBlock3) = createBlockResult3
        _                     <- nodes(0).casperEff.addBlock(signedBlock3, ignoreDoppelgangerCheck[Effect])
        _                     <- nodes(1).receive() // receive block3

        _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true

        createBlockResult4 <- nodes(1).casperEff
                               .deploy(deployPrim0) *> nodes(1).casperEff.createBlock
        Created(signedBlock4) = createBlockResult4
        _ <- nodes(1).casperEff
              .addBlock(signedBlock4, ignoreDoppelgangerCheck[Effect]) // should succeed
        _ <- nodes(0).receive() // still receive signedBlock4

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
          validateBlockStore(node) { blockStore =>
            for {
              _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
              _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              result <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
            } yield result
          }(nodes(0).metricEff, nodes(0).logEff)
        }
      } yield result
    }
  }

  it should "ignore adding equivocation blocks" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
      for {
        // Creates a pair that constitutes equivocation blocks
        basicDeployData0 <- ConstructDeploy.basicDeployData[Effect](0)
        createBlockResult1 <- nodes(0).casperEff
                               .deploy(basicDeployData0) *> nodes(0).casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
        basicDeployData1      <- ConstructDeploy.basicDeployData[Effect](1)
        createBlockResult1Prime <- nodes(0).casperEff
                                    .deploy(basicDeployData1) *> nodes(0).casperEff.createBlock
        Created(signedBlock1Prime) = createBlockResult1Prime

        _ <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(1).receive()
        _ <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(1).receive()

        _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
        result <- nodes(1).casperEff
                   .contains(signedBlock1Prime) shouldBeF false // we still add the equivocation pair
        _ <- validateBlockStore(nodes(1)) { blockStore =>
              for {
                _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
                result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF None
              } yield result
            }(nodes(0).metricEff, nodes(0).logEff)
      } yield result
    }
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis).use { nodes =>
      for {
        deployDatas <- (0 to 5).toList
                        .traverse[Effect, DeployData](
                          i => ConstructDeploy.basicDeployData[Effect](i)
                        )

        // Creates a pair that constitutes equivocation blocks
        createBlockResult1 <- nodes(0).casperEff
                               .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
        createBlockResult1Prime <- nodes(0).casperEff
                                    .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
        Created(signedBlock1Prime) = createBlockResult1Prime

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

        createBlockResult2 <- nodes(1).casperEff
                               .deploy(deployDatas(2)) *> nodes(1).casperEff.createBlock
        Created(signedBlock2) = createBlockResult2
        createBlockResult3 <- nodes(2).casperEff
                               .deploy(deployDatas(3)) *> nodes(2).casperEff.createBlock
        Created(signedBlock3) = createBlockResult3

        _ <- nodes(2).casperEff.addBlock(signedBlock3, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(1).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) ignores block2
        _ <- nodes(1).receive() // receives block3; asks for block1'
        _ <- nodes(2).receive() // receives request for block1'; sends block1'
        _ <- nodes(1).receive() // receives block1'; adds both block3 and block1'

        _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true
        _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

        createBlockResult4 <- nodes(1).casperEff
                               .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
        Created(signedBlock4) = createBlockResult4
        _                     <- nodes(1).casperEff.addBlock(signedBlock4, ignoreDoppelgangerCheck[Effect])

        // Node 1 should contain both blocks constituting the equivocation
        _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
        _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

        _ <- nodes(1).casperEff
              .contains(signedBlock4) shouldBeF true // However, marked as invalid

        _ = nodes(1).logEff.infos.count(_ startsWith "Added admissible equivocation") should be(1)
        _ = nodes(2).logEff.warns.size should be(0)
        _ = nodes(1).logEff.warns.size should be(1)
        _ = nodes(0).logEff.warns.size should be(0)

        _ <- nodes(1).casperEff
              .normalizedInitialFault(ProtoUtil.weightMap(genesis)) shouldBeF 1f / (1f + 3f + 5f + 7f)
        _ <- validateBlockStore(nodes(0)) { blockStore =>
              for {
                _ <- blockStore.get(signedBlock1.blockHash) shouldBeF None
                result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                           signedBlock1Prime
                         )
              } yield result
            }(nodes(0).metricEff, nodes(0).logEff)
        _ <- validateBlockStore(nodes(1)) { blockStore =>
              for {
                _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
                result <- blockStore.get(signedBlock4.blockHash) shouldBeF Some(signedBlock4)
              } yield result
            }(nodes(1).metricEff, nodes(1).logEff)
        result <- validateBlockStore(nodes(2)) { blockStore =>
                   for {
                     _ <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
                     result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                                signedBlock1Prime
                              )
                   } yield result
                 }(nodes(2).metricEff, nodes(2).logEff)
      } yield result
    }
  }

  it should "prepare to slash an block that includes a invalid block pointer" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis).use { nodes =>
      for {
        deploys         <- (0 to 5).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))
        deploysWithCost = deploys.map(d => ProcessedDeploy(deploy = Some(d))).toIndexedSeq

        createBlockResult <- nodes(0).casperEff
                              .deploy(deploys(0)) *> nodes(0).casperEff.createBlock
        Created(signedBlock) = createBlockResult
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

    val (validatorKeys, validatorPks) = (1 to 5).map(_ => Secp256k1.newKeyPair).unzip

    def deployment(i: Int, ts: Long): DeployData =
      ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", ts, accounting.MAX_VALUE)

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

    val network = TestNetwork.empty[Effect]
    HashSetCasperTestNode
      .networkEff(
        validatorKeys.take(3),
        buildGenesis(
          buildGenesisParameters(
            5,
            Map(
              validatorPks(0) -> 3L,
              validatorPks(1) -> 1L,
              validatorPks(2) -> 5L,
              validatorPks(3) -> 2L,
              validatorPks(4) -> 4L
            )
          )
        ),
        testNetwork = network
      )
      .map(_.toList)
      .use { nodes =>
        val v1 = nodes(0)
        val v2 = nodes(1)
        val v3 = nodes(2)
        for {
          _    <- deploy(v1, deployment(0, 1)) >> create(v1) >>= (v1c1 => add(v1, v1c1)) //V1#1
          v2c1 <- deploy(v2, deployment(0, 2)) >> create(v2) //V2#1
          _    <- v2.receive()
          _    <- v3.receive()
          _    <- deploy(v1, deployment(0, 4)) >> create(v1) >>= (v1c2 => add(v1, v1c2)) //V1#2
          v3c2 <- deploy(v3, deployment(0, 5)) >> create(v3) //V3#2
          _    <- v3.receive()
          _    <- add(v3, v3c2) //V3#2
          _    <- add(v2, v2c1) //V2#1
          _    <- v3.receive()
          r    <- deploy(v3, deployment(0, 6)) >> create(v3) >>= (b => add(v3, b))
          _    = r shouldBe Right(Valid)
          _    = v3.logEff.warns shouldBe empty
        } yield ()
      }
  }

  private def buildBlockWithInvalidJustification(
      nodes: IndexedSeq[HashSetCasperTestNode[Effect]],
      deploys: immutable.IndexedSeq[ProcessedDeploy],
      signedInvalidBlock: BlockMessage
  ): Effect[BlockMessage] = {
    val postState     = RChainState().withBonds(ProtoUtil.bonds(genesis)).withBlockNumber(1)
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
        validatorPks(1),
        validatorKeys(1),
        "secp256k1",
        "rchain"
      )
    }
  }
}
