package coop.rchain.casper.genesis.contracts

import cats.effect.Concurrent
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasperTestUtil.createBonds
import coop.rchain.casper.SignDeployment
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{BondingUtil, ConstructDeploy}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.Resources._
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RevIssuanceTest extends FlatSpec with Matchers {
  implicit val logger: Log[Task]         = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]

  "Rev" should "be issued and accessible based on inputs from Ethereum" in {
    mkRuntime[Task, Task.Par]("rev-issuance-test", 1024 * 1024 * 16L)
      .use { activeRuntime =>
        val secKey =
          Base16.unsafeDecode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
        val pubKey = Secp256k1.toPublic(secKey)
        val prefix = "0x"
        val ethAddress = prefix + Base16
          .encode(Keccak256.hash(pubKey.drop(1)))
          .takeRight(40)
        val revAddress  = RevAddress.fromEthAddress(ethAddress.drop(2))
        val initBalance = 100000L
        val vaults      = List(Vault(revAddress, initBalance))

        // These are unused but are inserted due to require(validators.nonEmpty)
        val (_, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
        val bonds           = createBonds(validators)
        val posValidators   = bonds.map(Validator.tupled).toSeq

        val (_, genesisPk) = Ed25519.newKeyPair

        val genesisDeploys =
          Genesis.defaultBlessedTerms(
            0L,
            ProofOfStake(1L, Long.MaxValue, posValidators),
            Nil,
            Faucet.noopFaucet,
            genesisPk,
            vaults,
            Long.MaxValue
          )

        val from   = revAddress
        val amount = 15L
        val destinationPublicKey = PublicKey(
          Base16.unsafeDecode("7e59141cbc0dfb24ca0e4fe57666895190740778871744be3cec262147e02f56")
        )
        val destinationRevAddress = RevAddress.fromPublicKey(destinationPublicKey).get
        val transferStatusOut     = "tOut"
        val transferDeployData = RevIssuanceTest.walletTransfer(
          from,
          amount,
          destinationRevAddress,
          transferStatusOut,
          secKey
        )
        for {
          runtimeManager        <- RuntimeManager.fromRuntime(activeRuntime)
          emptyHash             = runtimeManager.emptyStateHash
          postGenState          <- runtimeManager.computeState(emptyHash)(genesisDeploys)
          (postGenHash, _)      = postGenState
          postDeployState       <- runtimeManager.computeState(postGenHash)(transferDeployData :: Nil)
          (postTransferHash, _) = postDeployState
          transferSuccess <- runtimeManager.getData(postTransferHash)(
                              Par().copy(exprs = Seq(Expr(GString(transferStatusOut))))
                            )
          _ = assert(transferSuccess.head.exprs.head.getGInt == 15) //assert transfer success
        } yield ()
      }
      .runSyncUnsafe(60.seconds)
  }
}

object RevIssuanceTest {
  private def walletTransfer(
      from: RevAddress,
      amount: Long,
      destination: RevAddress,
      transferStatusOut: String,
      secKey: Array[Byte]
  ): DeployData = {
    val transferDeployCode =
      s"""
         |new rl(`rho:registry:lookup`), revVaultCh, vaultCh, revVaultKeyCh, stdout(`rho:io:stdout`), resultCh, destinationCh, balanceCh in {
         |  rl!(`rho:id:1o93uitkrjfubh43jt19owanuezhntag5wh74c6ur5feuotpi73q8z`, *revVaultCh) |
         |  for(@(_, RevVault) <- revVaultCh) {
         |    @RevVault!("findOrCreate", "${from.toBase58}", *vaultCh) |
         |    @RevVault!("deployerAuthKey", *revVaultKeyCh) |
         |    for (@(true, vault) <- vaultCh; key <- revVaultKeyCh) {
         |      @RevVault!("findOrCreate", "${destination.toBase58}", *destinationCh) | // Creates vault in case it doesn't exist
         |      @vault!("transfer", "${destination.toBase58}", $amount, *key, *resultCh) |
         |      for (@(true, Nil) <- resultCh) {
         |        for (@(true, destinationVault) <- destinationCh) {
         |          @destinationVault!("balance", *balanceCh) |
         |          for (@balance <- balanceCh) {
         |            @"$transferStatusOut"!(balance)
         |          }
         |        }
         |      }
         |    }
         |  }
         |}""".stripMargin
    val data = DeployData(
      deployer = ByteString.copyFrom(Secp256k1.toPublic(secKey)),
      timestamp = System.currentTimeMillis(),
      term = transferDeployCode,
      phloLimit = accounting.MAX_VALUE
    )
    SignDeployment.sign(PrivateKey(secKey), data, Secp256k1)
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
        ConstructDeploy.sourceDeploy(
          code,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
      }
}
