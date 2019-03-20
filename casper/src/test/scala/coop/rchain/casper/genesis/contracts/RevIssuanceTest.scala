package coop.rchain.casper.genesis.contracts

import cats.effect.Concurrent
import cats.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.HashSetCasperTest.createBonds
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{BondingUtil, ProtoUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RevIssuanceTest extends FlatSpec with Matchers {

  private def getDataUnsafe(
      runtimeManager: RuntimeManager[Task],
      postTransferHash: StateHash,
      transferStatusOut: String
  ) = {
    val transferSuccess = runtimeManager
      .getData(
        postTransferHash,
        Par().copy(exprs = Seq(Expr(GString(transferStatusOut))))
      )
      .unsafeRunSync
    transferSuccess
  }

  "Rev" should "be issued and accessible based on inputs from Ethereum" in {
    val activeRuntime  = TestUtil.runtime()
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync
    val emptyHash      = runtimeManager.emptyStateHash

    val ethAddress      = "0x041e1eec23d118f0c4ffc814d4f415ac3ef3dcff"
    val initBalance     = 37
    val wallet          = PreWallet(ethAddress, initBalance)
    val (_, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
    val bonds           = createBonds(validators)
    val posValidators   = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
    val genesisDeploys =
      Genesis.defaultBlessedTerms(
        0L,
        ProofOfStakeParams(1L, Long.MaxValue, posValidators),
        wallet :: Nil,
        Faucet.noopFaucet
      )

    val secKey = Base16.unsafeDecode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
    val pubKey =
      "f700a417754b775d95421973bdbdadb2d23c8a5af46f1829b1431f5c136e549e8a0d61aa0c793f1a614f8e437711c7758473c6ceb0859ac7e9e07911ca66b5c4"

    val statusOut = "out"
    val unlockDeployData =
      RevIssuanceTest
        .preWalletUnlockDeploy[Task](ethAddress, pubKey, secKey, statusOut)(
          Concurrent[Task],
          runtimeManager
        )
        .unsafeRunSync

    val nonce             = 0
    val amount            = 15L
    val destination       = "deposit"
    val transferStatusOut = "tOut"
    val transferDeployData = RevIssuanceTest
      .walletTransferDeploy(
        nonce,
        amount,
        destination,
        transferStatusOut,
        pubKey,
        secKey
      )(Concurrent[Task], runtimeManager)
      .unsafeRunSync
    val (postGenHash, _) =
      runtimeManager.computeState(emptyHash, genesisDeploys).runSyncUnsafe(10.seconds)
    val (postUnlockHash, _) =
      runtimeManager.computeState(postGenHash, unlockDeployData :: Nil).runSyncUnsafe(10.seconds)
    val unlockResult = getDataUnsafe(runtimeManager, postUnlockHash, statusOut)
    assert(unlockResult.head.exprs.head.getETupleBody.ps.head.exprs.head.getGBool) //assert unlock success

    val (postTransferHash, _) =
      runtimeManager
        .computeState(postUnlockHash, transferDeployData :: Nil)
        .runSyncUnsafe(10.seconds)
    val transferSuccess = getDataUnsafe(runtimeManager, postTransferHash, transferStatusOut)
    val transferResult  = getDataUnsafe(runtimeManager, postTransferHash, destination)
    assert(transferSuccess.head.exprs.head.getGString == "Success") //assert transfer success
    assert(transferResult.nonEmpty)

    activeRuntime.close()
  }
}

object RevIssuanceTest {
  def preWalletUnlockDeploy[F[_]: Concurrent](
      ethAddress: String,
      pubKey: String,
      secKey: Array[Byte],
      statusOut: String
  )(implicit runtimeManager: RuntimeManager[F]): F[DeployData] =
    BondingUtil.preWalletUnlockDeploy[F](ethAddress, pubKey, secKey, statusOut).map { code =>
      ProtoUtil.sourceDeploy(
        code,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
    }

  def walletTransferDeploy[F[_]: Concurrent](
      nonce: Int,
      amount: Long,
      destination: String,
      transferStatusOut: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager[F]): F[DeployData] =
    BondingUtil
      .issuanceWalletTransferDeploy[F](
        nonce,
        amount,
        destination,
        transferStatusOut,
        pubKey,
        secKey
      )
      .map { code =>
        ProtoUtil.sourceDeploy(
          code,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
      }
}
