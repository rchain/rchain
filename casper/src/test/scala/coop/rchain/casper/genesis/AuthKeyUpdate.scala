package coop.rchain.casper.genesis

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.ConstructDeploy.defaultSec
import coop.rchain.casper.util.rholang.Tools
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GUri}
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models.{ETuple, Expr, GPrivate, GUnforgeable, Par}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Base16
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.io.Source

class AuthKeyUpdate extends FlatSpec with Matchers with Inspectors {

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

    val proposeTimestamp = 1648038427475L
    val agreeTimestamp   = 1648038427476L
    val updateTimestamp  = 1648038427477L
    val proposeRnd       = Tools.unforgeableNameRng(pub1, proposeTimestamp)
    val propose          = Source.fromResource("updateAuthKey/ProposeAuthKey.rho").mkString
    val agree            = Source.fromResource("updateAuthKey/AgreeAuthKey.rho").mkString
    val update           = Source.fromResource("updateAuthKey/UpdateAuthKey.rho").mkString

    val uri = Par(exprs = Seq(Expr(GUri("rho:rchain:authKey"))))
    val updateCon = Par(
      unforgeables =
        Seq(GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(proposeRnd.next())))))
    )
    val newContract =
      Par(
        unforgeables =
          Seq(GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(proposeRnd.next())))))
      )
    val signedTarget = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(uri, updateCon, newContract))))))

    val proposeDeploy =
      ConstructDeploy.sourceDeploy(propose, proposeTimestamp, 100000000L, 1L, p1, shardId = shardId)
    val proposeSig = Secp256k1.sign(Blake2b256.hash(signedTarget.toByteArray), p1)
//    println(proposeDeploy)
    println(Base16.encode(proposeSig))

    val agreeDeploy =
      ConstructDeploy.sourceDeploy(agree, agreeTimestamp, 100000000L, 1L, p2, shardId = shardId)
    val agreeSig = Secp256k1.sign(Blake2b256.hash(signedTarget.toByteArray), p2)

    val updateDeploy =
      ConstructDeploy.sourceDeploy(update, updateTimestamp, 100000000L, 1L, p1, shardId = shardId)
    println(Base16.encode(agreeSig))

    val transferAmount = 100000

    val transferTerm =
      s"""
        #new rl(`rho:registry:lookup`), RevVaultCh, vaultCh, toVaultCh, deployerId(`rho:rchain:deployerId`), stdout(`rho:io:stdout`),revVaultKeyCh, resultCh in {
        #  rl!(`rho:rchain:revVault`, *RevVaultCh) |
        #  for (@(_, RevVault) <- RevVaultCh) {
        #    @RevVault!("findOrCreate", "${RevAddress
           .fromPublicKey(pub1)
           .get
           .toBase58}", *vaultCh) |
        #    @RevVault!("findOrCreate", "${RevAddress
           .fromPublicKey(pub2)
           .get
           .toBase58}", *toVaultCh) |
        #    @RevVault!("deployerAuthKey", *deployerId, *revVaultKeyCh) |
        #    for (@(true, vault) <- vaultCh; key <- revVaultKeyCh; @(true, toVault) <- toVaultCh) {
        #      @vault!("transfer", "${RevAddress
           .fromPublicKey(pub2)
           .get
           .toBase58}", $transferAmount, *key, *resultCh) |
        #      for (@res <- resultCh) { stdout!(("outcome", res)) }
        #    }
        #  }
        #}""".stripMargin('#')

    val getBalanceTerm =
      s"""new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh in {
         #  rl!(`rho:rchain:revVault`, *RevVaultCh) |
         #  for (@(_, RevVault) <- RevVaultCh) {
         #    @RevVault!("findOrCreate", "${RevAddress
           .fromPublicKey(pub2)
           .get
           .toBase58}", *vaultCh) |
         #    for (@(true, vault) <- vaultCh) {
         #      @vault!("balance", *balanceCh) |
         #      for (@balance <- balanceCh) {
         #        return!(balance)
         #      }
         #    }
         #  }
         #}
         #""".stripMargin('#')
    TestNode.standaloneEff(genesis).use { node =>
      val rm = node.runtimeManager
      for {
        b2      <- node.addBlock(proposeDeploy)
        _       = println(b2.body.deploys.head.cost)
        _       = println(b2.body.deploys.head.systemDeployError)
        _       = println(b2.body.deploys.head.isFailed)
        b3      <- node.addBlock(agreeDeploy)
        _       = println(b3.body.deploys.head.cost)
        _       = println(b3.body.deploys.head.systemDeployError)
        _       = println(b3.body.deploys.head.isFailed)
        b4      <- node.addBlock(updateDeploy)
        _       = println(b4.body.deploys.head.cost)
        _       = println(b4.body.deploys.head.systemDeployError)
        _       = println(b4.body.deploys.head.isFailed)
        ret     <- rm.playExploratoryDeploy(getBalanceTerm, b4.body.state.postStateHash)
        balance = ret.head.exprs.head.getGInt
        b5 <- node
               .addBlock(ConstructDeploy.sourceDeployNow(transferTerm, sec = p1, shardId = shardId))
        _    = println(b5.body.deploys.head.cost)
        _    = println(b5.body.deploys.head.systemDeployError)
        _    = println(b5.body.deploys.head.isFailed)
        ret2 <- rm.playExploratoryDeploy(getBalanceTerm, b5.body.state.postStateHash)
        _    = assert(ret2.head.exprs.head.getGInt == balance + transferAmount.toLong)
      } yield ()
    }
  }

}
