package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.HashSetCasperTest.createBonds
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.{Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath

import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class RevIssuanceTest extends FlatSpec with Matchers {
  "Rev" should "be issued and accessible based on inputs from Ethereum" in {
    val activeRuntime  = TestSetUtil.runtime("rev-issuance")
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
    val emptyHash      = runtimeManager.emptyStateHash

    val ethAddress      = "0x041e1eec23d118f0c4ffc814d4f415ac3ef3dcff"
    val initBalance     = 37
    val wallet          = PreWallet(ethAddress, initBalance)
    val (_, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
    val bonds           = createBonds(validators)
    val genesisDeploys =
      Genesis.defaultBlessedTerms(
        0L,
        bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq,
        wallet :: Nil
      )

    val secKey = Base16.decode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
    val pubKey =
      "f700a417754b775d95421973bdbdadb2d23c8a5af46f1829b1431f5c136e549e8a0d61aa0c793f1a614f8e437711c7758473c6ceb0859ac7e9e07911ca66b5c4"

    val statusOut = "out"
    val unlockDeployData =
      RevIssuanceTest.preWalletUnlockDeploy(ethAddress, pubKey, secKey, statusOut)(runtimeManager)
    val unlockDeploy = ProtoUtil.deployDataToDeploy(unlockDeployData)

    val nonce             = 0
    val amount            = 15L
    val destination       = "deposit"
    val transferStatusOut = "tOut"
    val transferDeployData = RevIssuanceTest.walletTransferDeploy(
      nonce,
      amount,
      destination,
      transferStatusOut,
      pubKey,
      secKey
    )(runtimeManager)
    val transferDeploy      = ProtoUtil.deployDataToDeploy(transferDeployData)
    val (postGenHash, _)    = runtimeManager.computeState(emptyHash, genesisDeploys)
    val (postUnlockHash, _) = runtimeManager.computeState(postGenHash, unlockDeploy :: Nil)
    val unlockResult =
      runtimeManager.getData(
        postUnlockHash,
        Channel(Quote(Par().copy(exprs = Seq(Expr(GString(statusOut))))))
      )
    assert(unlockResult.head.exprs.head.getEListBody.ps.head.exprs.head.getGBool) //assert unlock success

    val (postTransferHash, _) = runtimeManager.computeState(postUnlockHash, transferDeploy :: Nil)
    val transferSuccess = runtimeManager.getData(
      postTransferHash,
      Channel(Quote(Par().copy(exprs = Seq(Expr(GString(transferStatusOut))))))
    )
    val transferResult =
      runtimeManager.getData(
        postTransferHash,
        Channel(Quote(Par().copy(exprs = Seq(Expr(GString(destination))))))
      )
    assert(transferSuccess.head.exprs.head.getGString == "Success") //assert transfer success
    assert(transferResult.nonEmpty)

    activeRuntime.close()
  }
}

object RevIssuanceTest {
  def preWalletUnlockDeploy(
      ethAddress: String,
      pubKey: String,
      secKey: Array[Byte],
      statusOut: String
  )(implicit runtimeManager: RuntimeManager): DeployData = {
    assert(Base16.encode(Keccak256.hash(Base16.decode(pubKey)).drop(12)) == ethAddress.drop(2))
    val unlockSigDataTerm =
      mkTerm(s""" @"__SCALA__"!(["$pubKey", "$statusOut"].toByteArray())  """).right.get
    val unlockSigData = Keccak256.hash(
      runtimeManager
        .captureResults(runtimeManager.emptyStateHash, unlockSigDataTerm)
        .head
        .exprs
        .head
        .getGByteArray
        .toByteArray
    )
    val unlockSig = Secp256k1.sign(unlockSigData, secKey)
    assert(Secp256k1.verify(unlockSigData, unlockSig, Base16.decode("04" + pubKey)))

    ProtoUtil.sourceDeploy(
      s"""@"$ethAddress"!(["$pubKey", "$statusOut"], "${Base16.encode(unlockSig)}")""",
      System.currentTimeMillis()
    )
  }

  def walletTransferDeploy(
      nonce: Int,
      amount: Long,
      destination: String,
      transferStatusOut: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager): DeployData = {
    val transferSigDataTerm =
      mkTerm(s""" @"__SCALA__"!([$nonce, $amount, "$destination"].toByteArray())  """).right.get
    val transferSigData = Blake2b256.hash(
      runtimeManager
        .captureResults(runtimeManager.emptyStateHash, transferSigDataTerm)
        .head
        .exprs
        .head
        .getGByteArray
        .toByteArray
    )
    val transferSig = Secp256k1.sign(transferSigData, secKey)

    ProtoUtil.sourceDeploy(
      s"""
       |for(@wallet <- @"$pubKey") {
       |  @(wallet, "transfer")!($amount, $nonce, "${Base16
           .encode(transferSig)}", "$destination", "$transferStatusOut")
       |}
     """.stripMargin,
      System.currentTimeMillis()
    )
  }
}
