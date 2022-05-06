package coop.rchain.casper.genesis

import cats.syntax.all._
import coop.rchain.casper.genesis.contracts.ProofOfStake
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.GDeployId
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Base16
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.models.syntax._
import coop.rchain.rholang.build.CompiledRholangTemplate
import coop.rchain.rholang.interpreter.RhoType.{Boolean, String, Tuple2}
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class POSUpdate extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff           = new LogicalTime[Effect]
  private val shardId            = "root"
  private val hexP1              = "fc743bd08a822d544bfbe05a5663fc325039a44c8f0c7fbea95a85517da5c36b"
  private val hexP2              = "6e88cf274735f3f7f73ec3d7f0362c439ab508427682b5bd788007aca665d810"
  private val hexP3              = "87369d132ed6a7626dc4c5dfbaf41e954dd0ec55830e613a3f868c74d64a7a22"
  private val p1                 = PrivateKey(hexP1.unsafeDecodeHex)
  private val pub1               = Secp256k1.toPublic(p1)
  private val noPermissionKey    = PrivateKey(hexP2.unsafeDecodeHex)
  private val noPermissionKeyPub = Secp256k1.toPublic(noPermissionKey)
  private val p3                 = PrivateKey(hexP3.unsafeDecodeHex)
  private val pub3               = Secp256k1.toPublic(p3)
  private val validatorKeys      = Seq((p1, pub1), (noPermissionKey, noPermissionKeyPub), (p3, pub3))
  private val genesisVaults =
    Seq((p1, 100000000000L), (noPermissionKey, 100000000000L), (p3, 100000000000L))
  private val bonds       = Map(pub1 -> 1000000L, noPermissionKeyPub -> 1000000L, pub3 -> 1000000L)
  private val genesis     = buildGenesis(buildGenesisParameters(validatorKeys, genesisVaults, bonds))
  private val epochLength = 1

  private val updatePOSTerm = CompiledRholangTemplate.loadTemplate(
    "updatePOS/ProposePOS.rho",
    Seq(
      ("minimumBond", 1),
      ("maximumBond", 10000000000L),
      ("epochLength", epochLength),
      ("quarantineLength", 3),
      ("numberOfActiveValidators", 100),
      (
        "posMultiSigPublicKeys",
        ProofOfStake.publicKeys(
          List(
            Base16.encode(pub1.bytes),
            Base16.encode(noPermissionKey.bytes),
            Base16.encode(pub3.bytes)
          )
        )
      ),
      ("posMultiSigQuorum", 2)
    )
  )
  "deploy with correct private key" should "update the rho:rchain:pos right" in effectTest {
    val updateDeploy =
      ConstructDeploy.sourceDeployNow(updatePOSTerm, p1, 100000000L, 0L, shardId = shardId)

    val transferAmount = 100000

    val pub1RevAddr = RevAddress.fromPublicKey(pub1).get.toBase58
    val pub2RevAddr = RevAddress.fromPublicKey(noPermissionKeyPub).get.toBase58

    val transferTerm =
      s"""
         #new rl(`rho:registry:lookup`), RevVaultCh, vaultCh, toVaultCh, deployerId(`rho:rchain:deployerId`), stdout(`rho:io:stdout`),revVaultKeyCh, resultCh in {
         #  rl!(`rho:rchain:revVault`, *RevVaultCh) |
         #  for (@(_, RevVault) <- RevVaultCh) {
         #    @RevVault!("findOrCreate", "${pub1RevAddr}", *vaultCh) |
         #    @RevVault!("findOrCreate", "${pub2RevAddr}", *toVaultCh) |
         #    @RevVault!("deployerAuthKey", *deployerId, *revVaultKeyCh) |
         #    for (@(true, vault) <- vaultCh; key <- revVaultKeyCh; @(true, toVault) <- toVaultCh) {
         #      @vault!("transfer", "${pub2RevAddr}", $transferAmount, *key, *resultCh) |
         #      for (@res <- resultCh) { stdout!(("outcome", res)) }
         #    }
         #  }
         #}""".stripMargin('#')

    val getBalanceTerm =
      s"""new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh in {
         #  rl!(`rho:rchain:revVault`, *RevVaultCh) |
         #  for (@(_, RevVault) <- RevVaultCh) {
         #    @RevVault!("findOrCreate", "${pub2RevAddr}", *vaultCh) |
         #    for (@(true, vault) <- vaultCh) {
         #      @vault!("balance", *balanceCh) |
         #      for (@balance <- balanceCh) {
         #        return!(balance)
         #      }
         #    }
         #  }
         #}
         #""".stripMargin('#')

    val exploreUpdateResultTerm =
      """new return, registryLookup(`rho:registry:lookup`),stdout(`rho:io:stdout`), resCh, ret in {
                                    #  registryLookup!(`rho:rchain:pos`, *resCh) |
                                    #  for (@(nonce, pos) <- resCh){
                                    #    stdout!((pos, "get pos"))|
                                    #    @pos!("getEpochLength", *return)
                                    #  }
                                    #}""".stripMargin('#')
    TestNode.standaloneEff(genesis).use { node =>
      val rm = node.runtimeManager
      for {
        b2 <- node.addBlock(updateDeploy)
        _  = assert(b2.body.deploys.head.cost.cost > 0L, s"$b2 deploy cost is 0L")
        _  = assert(b2.body.deploys.head.systemDeployError.isEmpty, s"$b2 system deploy failed")
        _  = assert(!b2.body.deploys.head.isFailed, s"$b2 deploy failed")
        originalEpoch <- rm.playExploratoryDeploy(
                          exploreUpdateResultTerm,
                          genesis.genesisBlock.body.state.postStateHash
                        )
        _       = assert(originalEpoch.head.exprs.head.getGInt == 1000)
        ret     <- rm.playExploratoryDeploy(getBalanceTerm, b2.body.state.postStateHash)
        balance = ret.head.exprs.head.getGInt
        b5 <- node
               .addBlock(
                 ConstructDeploy
                   .sourceDeployNow(transferTerm, sec = p1, shardId = shardId, phloLimit = 9000000L)
               )
        _    = assert(b5.body.deploys.head.cost.cost > 0L, s"$b5 deploy cost is 0L")
        _    = assert(b5.body.deploys.head.systemDeployError.isEmpty, s"$b5 system deploy failed")
        _    = assert(!b5.body.deploys.head.isFailed, s"$b5 deploy failed")
        ret2 <- rm.playExploratoryDeploy(getBalanceTerm, b5.body.state.postStateHash)
        _    = assert(ret2.head.exprs.head.getGInt == balance + transferAmount.toLong)
        ret3 <- rm.playExploratoryDeploy(exploreUpdateResultTerm, b5.body.state.postStateHash)
        _    = assert(ret3.head.exprs.head.getGInt == epochLength)
      } yield ()
    }
  }

  "deploy with incorrect private key" should "return auth failure" in effectTest {
    val updateDeploy =
      ConstructDeploy.sourceDeployNow(
        updatePOSTerm,
        noPermissionKey,
        100000000L,
        0L,
        shardId = shardId
      )
    TestNode.standaloneEff(genesis).use { node =>
      val rm = node.runtimeManager
      for {
        b2  <- node.addBlock(updateDeploy)
        _   = assert(b2.body.deploys.head.cost.cost > 0L, s"$b2 deploy cost is 0L")
        _   = assert(b2.body.deploys.head.systemDeployError.isEmpty, s"$b2 system deploy failed")
        _   = assert(!b2.body.deploys.head.isFailed, s"$b2 deploy failed")
        // Get update contract result
        updateResult                               <- rm.getData(b2.body.state.postStateHash)(GDeployId(updateDeploy.sig))
        Tuple2((Boolean(success), String(errMsg))) = updateResult.head
        // Expect failed update
        _ = assert(!success, s"update should fail, instead: $errMsg")
      } yield ()
    }
  }
}
