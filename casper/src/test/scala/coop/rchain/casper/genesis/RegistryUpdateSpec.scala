package coop.rchain.casper.genesis

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class RegistryUpdateSpec extends AnyFlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  it should "update the testLib right" in effectTest {
    val shardId = "root"
    val p1 =
      PrivateKey("fc743bd08a822d544bfbe05a5663fc325039a44c8f0c7fbea95a85517da5c36b".unsafeDecodeHex)
    val pub1 = Secp256k1.toPublic(p1)
    val p2 =
      PrivateKey("6e88cf274735f3f7f73ec3d7f0362c439ab508427682b5bd788007aca665d810".unsafeDecodeHex)
    val pub2 = Secp256k1.toPublic(p2)
    val p3 =
      PrivateKey("87369d132ed6a7626dc4c5dfbaf41e954dd0ec55830e613a3f868c74d64a7a22".unsafeDecodeHex)
    val pub3          = Secp256k1.toPublic(p3)
    val validatorKeys = Seq((p1, pub1), (p2, pub2), (p3, pub3))
    val genesisVaults = Seq((p1, 100000000000L), (p2, 100000000000L), (p3, 100000000000L))
    val bonds         = Map(pub1 -> 1000000L, pub2 -> 1000000L, pub3 -> 1000000L)
    val genesis       = buildGenesis(buildGenesisParameters(validatorKeys, genesisVaults, bonds))

    val updateRegistry = Source.fromResource("UpdateRegistry/UpdateRegistry.rho").mkString
    val updateRegistryDeploy =
      ConstructDeploy.sourceDeployNow(updateRegistry, p1, 100000000L, 0, shardId = shardId)
    val transferAmount = 100000

    val pub1RevAddr = RevAddress.fromPublicKey(pub1).get.toBase58
    val pub2RevAddr = RevAddress.fromPublicKey(pub2).get.toBase58

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
      """new return, registryLookup(`rho:registry:lookup`), resCh,stdout(`rho:io:stdout`), ret in {
        #  registryLookup!(`rho:registry:systemContractManager`, *resCh) |
        #  for (@(_, systemContractManager) <- resCh){
        #    @systemContractManager!("sayHello", *return)
        #  }
        #}""".stripMargin('#')
    TestNode.standaloneEff(genesis).use { node =>
      val rm = node.runtimeManager
      for {
        b1      <- node.addBlock(updateRegistryDeploy)
        ret     <- rm.playExploratoryDeploy(getBalanceTerm, b1.postStateHash)
        _       = assert(b1.state.deploys.head.cost.cost > 0L, s"$b1 deploy cost is 0L")
        _       = assert(b1.state.deploys.head.systemDeployError.isEmpty, s"$b1 system deploy failed")
        _       = assert(!b1.state.deploys.head.isFailed, s"$b1 deploy failed")
        balance = ret.head.exprs.head.getGInt
        b2 <- node
               .addBlock(
                 ConstructDeploy
                   .sourceDeployNow(transferTerm, sec = p1, shardId = shardId, phloLimit = 900000L)
               )
        ret2 <- rm.playExploratoryDeploy(getBalanceTerm, b2.postStateHash)
        _    = assert(b2.state.deploys.head.cost.cost > 0L, s"$b2 deploy cost is 0L")
        _    = assert(b2.state.deploys.head.systemDeployError.isEmpty, s"$b2 system deploy failed")
        _    = assert(!b2.state.deploys.head.isFailed, s"$b2 deploy failed")
        _    = assert(ret2.head.exprs.head.getGInt == balance + transferAmount.toLong)
        ret3 <- rm.playExploratoryDeploy(exploreUpdateResultTerm, b2.postStateHash)
        _    = assert(ret3.head.exprs.head.getGString == "hello")
      } yield ()
    }
  }

}
