package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.casper.util.{BondingUtil, ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperBondingSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (otherSk, otherPk)            = Secp256k1.newKeyPair
  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
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
      genesisPk = Secp256k1.newKeyPair._2,
      vaults = bonds.toList.map {
        case (pk, stake) =>
          RevAddress.fromPublicKey(pk).map(Vault(_, stake))
      }.flattenOption,
      supply = Long.MaxValue
    )

  private val genesis = buildGenesis(genesisParameters)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "allow bonding" ignore effectTest { //TODO rewrite this test for the new PoS
    HashSetCasperTestNode
      .networkEff(
        validatorKeys :+ otherSk,
        genesis,
        storageSize = 1024L * 1024 * 10
      )
      .use { nodes =>
        val runtimeManager = nodes(0).runtimeManager
        val pubKey         = Base16.encode(ethPubKeys.head.bytes.drop(1))
        val secKey         = ethPivKeys.head.bytes
        val ethAddress     = ethAddresses.head
        val bondKey        = Base16.encode(otherPk.bytes)
        for {
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
            |  rl!(`rho:rchain:systemInstancesRegistry`, *SystemInstancesCh) |
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
        } yield result
      }
  }


}
