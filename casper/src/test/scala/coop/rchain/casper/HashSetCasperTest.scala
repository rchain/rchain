package coop.rchain.casper

import java.nio.file.Files

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{BondingUtil, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{signBlock, toJustification}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.{transport, CommError, TimeOut}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.models.{Expr, Par}
import coop.rchain.shared.StoreType
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.eitherT._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.comm.rp.Connect.Connections
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import org.scalatest

import scala.collection.immutable
import scala.util.Random
import scala.concurrent.duration._

class HashSetCasperTest extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (otherSk, otherPk)          = Ed25519.newKeyPair
  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (ethPivKeys, ethPubKeys)    = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
  private val bonds       = createBonds(validators)
  private val minimumBond = 100L
  private val genesis =
    buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "create blocks based on deploys" in effectTest {
    val node            = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    implicit val casper = node.casperEff

    for {
      deploy <- ConstructDeploy.basicDeployData[Effect](0)
      _      <- MultiParentCasper[Effect].deploy(deploy)

      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
      deploys           = block.body.get.deploys.flatMap(_.deploy)
      parents           = ProtoUtil.parentHashes(block)
      storage           <- blockTuplespaceContents(block)

      _      = parents.size should be(1)
      _      = parents.head should be(genesis.blockHash)
      _      = deploys.size should be(1)
      _      = deploys.head should be(deploy)
      result = storage.contains("@{0}!(0)") should be(true)
      _      <- node.tearDown()
    } yield result
  }

  it should "be able to use the registry" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node.casperEff

    def now = System.currentTimeMillis()
    val registerDeploy = ConstructDeploy.sourceDeploy(
      """new uriCh, rr(`rho:registry:insertArbitrary`), hello in {
        |  contract hello(@name, return) = { return!("Hello, ${name}!" %% {"name" : name}) } |
        |  rr!(bundle+{*hello}, *uriCh)
        |}
      """.stripMargin,
      1539788365118L, //fix the timestamp so that `uriCh` is known
      accounting.MAX_VALUE
    )

    for {
      _                 <- casperEff.deploy(registerDeploy)
      createBlockResult <- casperEff.createBlock
      Created(block)    = createBlockResult
      blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
      id <- casperEff
             .storageContents(ProtoUtil.postStateHash(block))
             .map(
               _.split('|')
                 .find(
                   _.contains(
                     //based on the timestamp of registerDeploy, this is uriCh
                     "@{Unforgeable(0xe2615f3fd74c2b83230a3bfc44001d38aded3d2ade5e9b15ffd20e8566d3426f)}!"
                   )
                 )
                 .get
                 .split('`')(1)
             )
      callDeploy = ConstructDeploy.sourceDeploy(
        s"""new rl(`rho:registry:lookup`), helloCh, out in {
           |  rl!(`$id`, *helloCh) |
           |  for(hello <- helloCh){ hello!("World", *out) }
           |}
      """.stripMargin,
        now,
        accounting.MAX_VALUE
      )
      _                  <- casperEff.deploy(callDeploy)
      createBlockResult2 <- casperEff.createBlock
      Created(block2)    = createBlockResult2
      block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _                  = blockStatus shouldBe Valid
      _                  = block2Status shouldBe Valid
      result <- casperEff
                 .storageContents(ProtoUtil.postStateHash(block2))
                 .map(_.contains("Hello, World!")) shouldBeF true

      _ <- node.tearDown()
    } yield result
  }

  it should "have a working faucet (in testnet)" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node.casperEff

    //val skStr = "6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14"
    val pkStr = "16989775f3f207a717134216816d3c9d97b0bfb8d560b29485f23f6ead435f09"
    val sigStr = "51c2b091559745d51c7270189911d9d894d538f76150ed67d164705dcf0af52" +
      "e101fa06396db2b2ac21a4bfbe3461567b5f8b3d2e666c377cb92d96bc38e2c08"
    val amount = 157L
    val createWalletCode =
      s"""new
         |  walletCh, rl(`rho:registry:lookup`), SystemInstancesCh, faucetCh,
         |  rs(`rho:registry:insertSigned:ed25519`), uriOut
         |in {
         |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
         |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
         |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
         |    for(faucet <- faucetCh){ faucet!($amount, "ed25519", "$pkStr", *walletCh) } |
         |    for(@[wallet] <- walletCh){ walletCh!!(wallet) }
         |  } |
         |  rs!(
         |    "$pkStr".hexToBytes(),
         |    (9223372036854775807, bundle-{*walletCh}),
         |    "$sigStr".hexToBytes(),
         |    *uriOut
         |  )
         |}""".stripMargin

    //with the fixed user+timestamp we know that walletCh is registered at `rho:id:mrs88izurkgki71dpjqamzg6tgcjd6sk476c9msks7tumw4a6e39or`
    val createWalletDeploy = ConstructDeploy.sourceDeploy(
      source = createWalletCode,
      timestamp = 1540570144121L,
      phlos = accounting.MAX_VALUE,
      sec = PrivateKey(
        Base16.unsafeDecode("6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14")
      )
    )

    for {
      createBlockResult <- casperEff.deploy(createWalletDeploy) *> casperEff.createBlock
      Created(block)    = createBlockResult
      blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
      balanceQuery = ConstructDeploy.sourceDeploy(
        s"""new
           |  rl(`rho:registry:lookup`), walletFeedCh
           |in {
           |  rl!(`rho:id:mrs88izurkgki71dpjqamzg6tgcjd6sk476c9msks7tumw4a6e39or`, *walletFeedCh) |
           |  for(@(_, walletFeed) <- walletFeedCh) {
           |    for(wallet <- @walletFeed) { wallet!("getBalance", "__SCALA__") }
           |  }
           |}""".stripMargin,
        0L,
        accounting.MAX_VALUE,
        sec = PrivateKey(
          Base16.unsafeDecode("6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14")
        )
      )
      newWalletBalance <- node.runtimeManager
                           .captureResults(
                             ProtoUtil.postStateHash(block),
                             balanceQuery
                           )
      _      = blockStatus shouldBe Valid
      result = newWalletBalance.head.exprs.head.getGInt shouldBe amount

      _ <- node.tearDown()
    } yield result
  }

  it should "estimate parent properly" in effectTest {
    val (otherSk, otherPk)          = Ed25519.newKeyPair
    val (validatorKeys, validators) = (1 to 5).map(_ => Ed25519.newKeyPair).unzip
    val (ethPivKeys, ethPubKeys)    = (1 to 5).map(_ => Secp256k1.newKeyPair).unzip
    val ethAddresses =
      ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
    val wallets = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
    val bonds = Map(
      validators(0) -> 3L,
      validators(1) -> 1L,
      validators(2) -> 5L,
      validators(3) -> 2L,
      validators(4) -> 4L
    )
    val minimumBond = 100L
    val genesis =
      buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

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

    for {
      nodes <- HashSetCasperTestNode
                .networkEff(validatorKeys.take(3), genesis, testNetwork = network)
                .map(_.toList)
      v1   = nodes(0)
      v2   = nodes(1)
      v3   = nodes(2)
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

      _ <- nodes.map(_.tearDown()).sequence
    } yield ()
  }

  it should "prepare to slash an block that includes a invalid block pointer" in effectTest {
    for {
      nodes           <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
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
        transport.BlockMessage,
        signedInvalidBlock.toByteString
      )
      _ <- nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
      _ <- nodes(1).receive() // receives signedInvalidBlock; attempts to add both blocks

      result = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(1)
      _      <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "handle a long chain of block requests appropriately" in effectTest {
    for {
      nodes <- HashSetCasperTestNode.networkEff(
                validatorKeys.take(2),
                genesis,
                storageSize = 1024L * 1024 * 10
              )

      _ <- (0 to 9).toList.traverse_[Effect, Unit] { i =>
            for {
              deploy <- ConstructDeploy.basicDeployData[Effect](i)
              createBlockResult <- nodes(0).casperEff
                                    .deploy(deploy) *> nodes(0).casperEff.createBlock
              Created(block) = createBlockResult

              _ <- nodes(0).casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
              _ <- nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block
            } yield ()
          }
      deployData10 <- ConstructDeploy.basicDeployData[Effect](10)
      createBlock11Result <- nodes(0).casperEff.deploy(deployData10) *> nodes(
                              0
                            ).casperEff.createBlock
      Created(block11) = createBlock11Result
      _                <- nodes(0).casperEff.addBlock(block11, ignoreDoppelgangerCheck[Effect])

      // Cycle of requesting and passing blocks until block #3 from nodes(0) to nodes(1)
      _ <- (0 to 8).toList.traverse_[Effect, Unit] { i =>
            nodes(1).receive() *> nodes(0).receive()
          }

      // We simulate a network failure here by not allowing block #2 to get passed to nodes(1)

      // And then we assume fetchDependencies eventually gets called
      _ <- nodes(1).casperEff.fetchDependencies
      _ <- nodes(0).receive()

      _ = nodes(1).logEff.infos.count(_ startsWith "Requested missing block") should be(10)
      result = nodes(0).logEff.infos.count(
        s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
      ) should be(10)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "increment last finalized block as appropriate in round robin" in effectTest {
    val stake      = 10L
    val equalBonds = validators.map(_ -> stake).toMap
    val genesisWithEqualBonds =
      buildGenesis(Seq.empty, equalBonds, 1L, Long.MaxValue, Faucet.noopFaucet, 0L)
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesisWithEqualBonds)
      deployDatas <- (0 to 7).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))

      createBlock1Result <- nodes(0).casperEff
                             .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
      Created(block1) = createBlock1Result
      _               <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      createBlock2Result <- nodes(1).casperEff
                             .deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
      Created(block2) = createBlock2Result
      _               <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      createBlock3Result <- nodes(2).casperEff
                             .deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
      Created(block3) = createBlock3Result
      _               <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(1).receive()

      createBlock4Result <- nodes(0).casperEff
                             .deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
      Created(block4) = createBlock4Result
      _               <- nodes(0).casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      createBlock5Result <- nodes(1).casperEff
                             .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
      Created(block5) = createBlock5Result
      _               <- nodes(1).casperEff.addBlock(block5, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block1
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      createBlock6Result <- nodes(2).casperEff
                             .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
      Created(block6) = createBlock6Result
      _               <- nodes(2).casperEff.addBlock(block6, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(1).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block2
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      createBlock7Result <- nodes(0).casperEff
                             .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
      Created(block7) = createBlock7Result
      _               <- nodes(0).casperEff.addBlock(block7, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      _ <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block3
      _ = state.deployHistory.size should be(1)

      createBlock8Result <- nodes(1).casperEff
                             .deploy(deployDatas(7)) *> nodes(1).casperEff.createBlock
      Created(block8) = createBlock8Result
      _               <- nodes(1).casperEff.addBlock(block8, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block4
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield ()
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
        validators(1),
        validatorKeys(1),
        "ed25519",
        "rchain"
      )
    }
  }
}
