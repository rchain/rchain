package coop.rchain.casper.batch2

import cats.syntax.all._
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.RSpaceUtil._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.crypto.PrivateKey
import coop.rchain.models.syntax._
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.{Base16, RChainScheduler}
import coop.rchain.shared.scalatestcontrib._
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.casper.util.ConstructDeploy

import scala.io.Source
class POSProofOfConcept extends FlatSpec with Matchers {

  implicit val scheduler = RChainScheduler.interpreterScheduler
  val genesis            = buildGenesis()

  "proof of concept" should "work" in effectTest {
    TestNode
      .standaloneEff(genesis)
      .use { node =>
        import node._

        val humanControllPriKey = ConstructDeploy.defaultSec2
        val humanControlPubkey  = Secp256k1.toPublic(humanControllPriKey)

        val humanTransferTo = PrivateKey.apply(
          "2222222222222222222222222222222222222222222222222222222222222222".unsafeDecodeHex
        )
        val pub2       = Secp256k1.toPublic(humanTransferTo)
        val pubRevAddr = RevAddress.fromPublicKey(pub2).get.toBase58

        val humanTransAmount = 1000
        val fromAddr         = RevAddress.fromPublicKey(ConstructDeploy.defaultPub).get.toBase58
        def posCode(pubKey: String) =
          Source.fromResource("proofOfConcept/pos.rho").mkString.replace(s"$$pubKey", pubKey)
        val getPOSinfo = Source.fromResource("proofOfConcept/getPosInfo.rho").mkString

        def transferCode(fromAddr: String, toAddr: String, amount: Int) =
          Source
            .fromResource("proofOfConcept/transfer.rho")
            .mkString
            .replace(s"$$fromAddr", fromAddr)
            .replace(s"$$to", toAddr)
            .replace(s"$$amount", amount.toString)
        def checkBalance(addr: String) =
          Source.fromResource("proofOfConcept/checkBalance.rho").mkString.replace(s"$$addr", addr)
        def posHumanTransfer(addr: String, amount: Int) =
          Source
            .fromResource("proofOfConcept/posHumanTransfer.rho")
            .mkString
            .replace(s"$$addr", addr)
            .replace(s"$$humanTransAmount", amount.toString)

        for {
          deploy <- ConstructDeploy.sourceDeployNowF(
                     posCode(Base16.encode(humanControlPubkey.bytes))
                   )
          block1 <- node.addBlock(deploy)

          // get the pos vault info to put some rev inside
          posUnf <- node.runtimeManagerEffect
                     .playExploratoryDeploy(getPOSinfo, block1.body.state.postStateHash)
          _       = println(posUnf)
          posAddr = posUnf(1).exprs.head.getGString

          // this step is just some presteps to make sure pos vault have rev so that we can humanly call transfer
          transferToPos <- ConstructDeploy.sourceDeployNowF(
                            transferCode(fromAddr, posAddr, 10000),
                            sec = ConstructDeploy.defaultSec
                          )
          block2 <- node.addBlock(transferToPos)
          _      = println(block2)

          // transfer the money inside the pos using the private key correspond to the pub key provided in the first block
          humanTransfer <- ConstructDeploy.sourceDeployNowF(
                            posHumanTransfer(pubRevAddr, humanTransAmount),
                            sec = humanControllPriKey
                          )
          block3 <- node.addBlock(humanTransfer)

          // make sure the transfer is done.
          isTransferOk <- node.runtimeManagerEffect.playExploratoryDeploy(
                           checkBalance(pubRevAddr),
                           block3.body.state.postStateHash
                         )
          amount = isTransferOk.head.exprs.head.getGInt
          _      = println(amount)
          _      = assert(amount == humanTransAmount)
        } yield ()
      }
  }

}
