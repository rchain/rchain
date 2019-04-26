package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.casper.util.{BondingUtil, ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperBondingSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (otherSk, otherPk)            = Ed25519.newKeyPair
  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (ethPivKeys, ethPubKeys)      = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets = ethAddresses.map(PreWallet(_, BigInt(10001)))
  private val bonds   = createBonds(validatorPks)

  private val genesisParameters =
    Genesis(
      shardId = "MultiParentCasperBondingSpec",
      timestamp = 0L,
      wallets = wallets,
      proofOfStake = ProofOfStake(
        minimumBond = 0L,
        maximumBond = Long.MaxValue,
        validators = bonds.map(Validator.tupled).toSeq
      ),
      faucet = true,
      genesisPk = Ed25519.newKeyPair._2,
      vaults = bonds.toList.map {
        case (pk, stake) =>
          RevAddress.fromPublicKey(pk).map(Vault(_, stake))
      }.flattenOption,
      supply = Long.MaxValue
    )

  private val genesis = buildGenesis(genesisParameters)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "allow bonding" in effectTest {
    for {
      nodes <- HashSetCasperTestNode.networkEff(
                validatorKeys :+ otherSk,
                genesis,
                storageSize = 1024L * 1024 * 10
              )
      runtimeManager = nodes(0).runtimeManager
      pubKey         = Base16.encode(ethPubKeys.head.bytes.drop(1))
      secKey         = ethPivKeys.head.bytes
      ethAddress     = ethAddresses.head
      bondKey        = Base16.encode(otherPk.bytes)
      walletUnlockDeploy <- RevIssuanceTest.preWalletUnlockDeploy(
                             ethAddress,
                             pubKey,
                             secKey,
                             "unlockOut"
                           )(Concurrent[Effect], runtimeManager)
      bondingForwarderAddress = BondingUtil.bondingForwarderAddress(ethAddress)
      bondingForwarderDeploy = ConstructDeploy.sourceDeploy(
        BondingUtil.bondingForwarderDeploy(bondKey, ethAddress),
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      transferStatusOut = BondingUtil.transferStatusOut(ethAddress)
      bondingTransferDeploy <- RevIssuanceTest.walletTransferDeploy(
                                0,
                                wallets.head.initRevBalance.toLong,
                                bondingForwarderAddress,
                                transferStatusOut,
                                pubKey,
                                secKey
                              )(Concurrent[Effect], runtimeManager)

      createBlock1Result <- nodes(0).casperEff.deploy(walletUnlockDeploy) *> nodes(0).casperEff
                             .deploy(bondingForwarderDeploy) *> nodes(0).casperEff.createBlock
      Created(block1) = createBlock1Result
      block1Status    <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive()) //send to all peers

      createBlock2Result <- nodes(1).casperEff
                             .deploy(bondingTransferDeploy) *> nodes(1).casperEff.createBlock
      Created(block2) = createBlock2Result
      block2Status    <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive())

      helloWorldDeploy = ConstructDeploy.sourceDeploy(
        """new s(`rho:io:stdout`) in { s!("Hello, World!") }""",
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      //new validator does deploy/propose
      createBlock3Result <- nodes.last.casperEff
                             .deploy(helloWorldDeploy) *> nodes.last.casperEff.createBlock
      Created(block3) = createBlock3Result
      block3Status    <- nodes.last.casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])

      //previous validator does deploy/propose
      createBlock3PrimeResult <- nodes.head.casperEff
                                  .deploy(helloWorldDeploy) *> nodes.head.casperEff.createBlock
      Created(block3Prime) = createBlock3PrimeResult
      block3PrimeStatus <- nodes.head.casperEff
                            .addBlock(block3Prime, ignoreDoppelgangerCheck[Effect])

      _ <- nodes.toList.traverse_(_.receive()) //all nodes get the blocks

      _ = block1Status shouldBe Valid
      _ = block2Status shouldBe Valid
      _ = block3Status shouldBe Valid
      _ = block3PrimeStatus shouldBe Valid
      _ = nodes.forall(_.logEff.warns.isEmpty) shouldBe true

      rankedValidatorQuery = ConstructDeploy.sourceDeploy(
        """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
          |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
          |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
          |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
          |    for(pos <- posCh){
          |      new bondsCh, getRanking in {
          |        contract getRanking(@bonds, @acc, return) = {
          |          match bonds {
          |            {key:(stake, _, _, index) ...rest} => {
          |              getRanking!(rest, acc ++ [(key, stake, index)], *return)
          |            }
          |            _ => { return!(acc) }
          |          }
          |        } |
          |        pos!("getBonds", *bondsCh) | for(@bonds <- bondsCh) {
          |          getRanking!(bonds, [], "__SCALA__")
          |        }
          |      }
          |    }
          |  }
          |}""".stripMargin,
        0L,
        accounting.MAX_VALUE
      )
      validatorBondsAndRanksT <- runtimeManager
                                  .captureResults(
                                    ProtoUtil.postStateHash(block1),
                                    rankedValidatorQuery
                                  )

      validatorBondsAndRanks = validatorBondsAndRanksT.head.exprs.head.getEListBody.ps
        .map(
          _.exprs.head.getETupleBody.ps match {
            case Seq(a, b, c) =>
              (a.exprs.head.getGByteArray, b.exprs.head.getGInt, c.exprs.head.getGInt.toInt)
          }
        )

      correctBonds = validatorBondsAndRanks.map {
        case (keyA, stake, _) =>
          Bond(keyA, stake)
      }.toSet + Bond(
        ByteString.copyFrom(otherPk.bytes),
        wallets.head.initRevBalance.toLong
      )

      newBonds = block2.getBody.getState.bonds
      result   = newBonds.toSet shouldBe correctBonds

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "allow bonding via the faucet" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node.casperEff

    implicit val runtimeManager = node.runtimeManager
    val (sk, pk)                = Ed25519.newKeyPair
    val pkStr                   = Base16.encode(pk.bytes)
    val amount                  = 314L
    val forwardCode             = BondingUtil.bondingForwarderDeploy(pkStr, pkStr)

    for {
      bondingCode <- BondingUtil.faucetBondDeploy[Effect](amount, "ed25519", pkStr, sk)
      forwardDeploy = ConstructDeploy.sourceDeploy(
        forwardCode,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      bondingDeploy = ConstructDeploy.sourceDeploy(
        bondingCode,
        forwardDeploy.timestamp + 1,
        accounting.MAX_VALUE
      )

      createBlockResult1 <- casperEff.deploy(forwardDeploy) *> casperEff.createBlock
      Created(block1)    = createBlockResult1
      block1Status       <- casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      createBlockResult2 <- casperEff.deploy(bondingDeploy) *> casperEff.createBlock
      Created(block2)    = createBlockResult2
      block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      oldBonds           = block1.getBody.getState.bonds
      newBonds           = block2.getBody.getState.bonds
      _                  = block1Status shouldBe Valid
      _                  = block2Status shouldBe Valid
      result             = (oldBonds.size + 1) shouldBe newBonds.size

      _ <- node.tearDown()
    } yield result
  }

  it should "allow bonding in an existing network" in effectTest {
    def deployment(i: Int): DeployData =
      ConstructDeploy.sourceDeploy(
        s"@$i!({$i})",
        System.currentTimeMillis() + i,
        accounting.MAX_VALUE
      )

    def deploy(
        node: HashSetCasperTestNode[Effect],
        dd: DeployData
    ): Effect[(BlockMessage, Either[Throwable, BlockStatus])] =
      for {
        createBlockResult1    <- node.casperEff.deploy(dd) *> node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1

        status <- Sync[Effect].attempt(
                   node.casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
                 )
      } yield (signedBlock1, status)

    def stepSplit(
        nodes: List[HashSetCasperTestNode[Effect]]
    ): Effect[List[Either[Throwable, Unit]]] =
      for {
        _  <- nodes.zipWithIndex.traverse { case (n, i) => deploy(n, deployment(i)) }
        vs <- nodes.traverse(v => Sync[Effect].attempt(v.receive()))
      } yield vs

    def bond(
        node: HashSetCasperTestNode[Effect],
        keys: (PrivateKey, PublicKey)
    ): Effect[Unit] = {
      implicit val runtimeManager = node.runtimeManager
      val (sk, pk)                = keys
      val pkStr                   = Base16.encode(pk.bytes)
      val amount                  = 314L
      val forwardCode             = BondingUtil.bondingForwarderDeploy(pkStr, pkStr)
      for {
        bondingCode <- BondingUtil.faucetBondDeploy[Effect](amount, "ed25519", pkStr, sk)
        forwardDeploy = ConstructDeploy.sourceDeploy(
          forwardCode,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
        bondingDeploy = ConstructDeploy.sourceDeploy(
          bondingCode,
          forwardDeploy.timestamp + 1,
          accounting.MAX_VALUE
        )
        fr       <- deploy(node, forwardDeploy)
        br       <- deploy(node, bondingDeploy)
        oldBonds = fr._1.getBody.getState.bonds
        newBonds = br._1.getBody.getState.bonds
        _        = fr._2 shouldBe Right(Valid)
        _        = br._2 shouldBe Right(Valid)
        _        = (oldBonds.size + 1) shouldBe newBonds.size
      } yield ()
    }

    val network = TestNetwork.empty[Effect]

    for {
      nodes <- HashSetCasperTestNode
                .networkEff(validatorKeys.take(3), genesis, testNetwork = network)
                .map(_.toList)
      _        <- stepSplit(nodes)
      _        <- stepSplit(nodes)
      (sk, pk) = Ed25519.newKeyPair
      newNode  = HashSetCasperTestNode.standaloneEff(genesis, sk, testNetwork = network)
      _        <- bond(nodes(0), (sk, pk))
      all      <- HashSetCasperTestNode.rigConnectionsF[Effect](newNode, nodes)

      s1 <- stepSplit(all)
      _ = forAll(s1) { v =>
        v.isRight should be(true)
      }
      s2 <- stepSplit(all)
      _ = forAll(s2) { v =>
        v.isRight should be(true)
      }

      _ <- all.map(_.tearDown()).sequence
    } yield ()
  }

  it should "not fail if the forkchoice changes after a bonding event" in effectTest {
    val localValidators = validatorKeys.take(3)
    val localBonds      = localValidators.map(Ed25519.toPublic).zip(List(10L, 30L, 5000L)).toMap
    val localGenesis =
      buildGenesis(
        genesisParameters.copy(
          wallets = Nil,
          proofOfStake = genesisParameters.proofOfStake.copy(
            validators = localBonds.map(Validator.tupled).toSeq
          )
        )
      )
    for {
      nodes <- HashSetCasperTestNode.networkEff(localValidators, localGenesis)

      rm          = nodes.head.runtimeManager
      (sk, pk)    = Ed25519.newKeyPair
      pkStr       = Base16.encode(pk.bytes)
      forwardCode = BondingUtil.bondingForwarderDeploy(pkStr, pkStr)
      bondingCode <- BondingUtil
                      .faucetBondDeploy[Effect](50, "ed25519", pkStr, sk)(
                        Concurrent[Effect],
                        rm
                      )
      forwardDeploy = ConstructDeploy.sourceDeploy(
        forwardCode,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      bondingDeploy = ConstructDeploy.sourceDeploy(
        bondingCode,
        forwardDeploy.timestamp + 1,
        accounting.MAX_VALUE
      )

      _                    <- nodes.head.casperEff.deploy(forwardDeploy)
      _                    <- nodes.head.casperEff.deploy(bondingDeploy)
      createBlockResult1   <- nodes.head.casperEff.createBlock
      Created(bondedBlock) = createBlockResult1

      bondedBlockStatus <- nodes.head.casperEff
                            .addBlock(bondedBlock, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).receive()
      _ <- nodes.head.receive()
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses bonding

      createBlockResult2 <- {
        val n = nodes(1)
        import n.casperEff._
        (ConstructDeploy.basicDeployData[Effect](0) >>= deploy) *> createBlock
      }
      Created(block2) = createBlockResult2
      status2         <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.head.receive()
      _               <- nodes(1).receive()
      _               <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses block built on bonding

      createBlockResult3 <- { //nodes(2) proposes a block
        val n = nodes(2)
        import n.casperEff._
        (ConstructDeploy.basicDeployData[Effect](1) >>= deploy) *> createBlock
      }
      Created(block3) = createBlockResult3
      status3         <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive())
      //Since weight of nodes(2) is higher than nodes(0) and nodes(1)
      //their fork-choice changes, thus the new validator
      //is no longer bonded

      createBlockResult4 <- { //nodes(0) proposes a new block
        val n = nodes.head
        import n.casperEff._
        (ConstructDeploy.basicDeployData[Effect](2) >>= deploy) *> createBlock
      }
      Created(block4) = createBlockResult4
      status4         <- nodes.head.casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive())

      _      = bondedBlockStatus shouldBe Valid
      _      = status2 shouldBe Valid
      _      = status3 shouldBe Valid
      result = status4 shouldBe Valid
      _      = nodes.foreach(_.logEff.warns shouldBe Nil)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }
}
