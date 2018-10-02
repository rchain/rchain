package coop.rchain.casper.util

import cats.effect.{Sync}
import cats.implicits._

import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath

import java.io.PrintWriter
import java.nio.file.Files

import monix.execution.Scheduler

object BondingUtil {
  def bondingForwarderDeploy(bondKey: String, ethAddress: String): String = {
    val bondingStatusOut        = s"${ethAddress}_bondingOut"
    val bondingForwarderAddress = s"${ethAddress}_bondingForwarder"
    s"""for(@purse <- @"$bondingForwarderAddress"; @pos <- @"proofOfStake"){
       |  @(pos, "bond")!("$bondKey".hexToBytes(), "ed25519Verify", purse, "$ethAddress", "$bondingStatusOut")
       |}""".stripMargin
  }

  def unlockDeploy[F[_]: Sync](ethAddress: String, pubKey: String, secKey: String)(
      implicit runtimeManager: RuntimeManager,
      scheduler: Scheduler
  ): F[String] =
    preWalletUnlockDeploy(ethAddress, pubKey, Base16.decode(secKey), s"${ethAddress}_unlockOut")

  def bondDeploy[F[_]: Sync](amount: Long, ethAddress: String, pubKey: String, secKey: String)(
      implicit runtimeManager: RuntimeManager,
      scheduler: Scheduler
  ): F[String] = {
    val bondingForwarderAddress = s"${ethAddress}_bondingForwarder"
    val transferStatusOut       = s"${ethAddress}_transferOut"
    walletTransferDeploy(
      0, //nonce
      amount,
      bondingForwarderAddress,
      transferStatusOut,
      pubKey,
      Base16.decode(secKey)
    )
  }

  def preWalletUnlockDeploy[F[_]: Sync](
      ethAddress: String,
      pubKey: String,
      secKey: Array[Byte],
      statusOut: String
  )(implicit runtimeManager: RuntimeManager, scheduler: Scheduler): F[String] = {
    require(Base16.encode(Keccak256.hash(Base16.decode(pubKey)).drop(12)) == ethAddress.drop(2))
    val unlockSigDataTerm =
      mkTerm(s""" @"__SCALA__"!(["$pubKey", "$statusOut"].toByteArray())""").right.get
    for {
      sigBytes <- Sync[F].delay {
                   runtimeManager
                     .captureResults(runtimeManager.emptyStateHash, unlockSigDataTerm)
                     .head
                     .exprs
                     .head
                     .getGByteArray
                     .toByteArray
                 }
      unlockSigData = Keccak256.hash(sigBytes)
      unlockSig     = Secp256k1.sign(unlockSigData, secKey)
      _             = assert(Secp256k1.verify(unlockSigData, unlockSig, Base16.decode("04" + pubKey)))
    } yield s"""@"$ethAddress"!(["$pubKey", "$statusOut"], "${Base16.encode(unlockSig)}")"""
  }

  def walletTransferDeploy[F[_]: Sync](
      nonce: Int,
      amount: Long,
      destination: String,
      transferStatusOut: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager, scheduler: Scheduler): F[String] = {
    val transferSigDataTerm =
      mkTerm(s""" @"__SCALA__"!([$nonce, $amount, "$destination"].toByteArray())""").right.get

    for {
      sigBytes <- Sync[F].delay {
                   runtimeManager
                     .captureResults(runtimeManager.emptyStateHash, transferSigDataTerm)
                     .head
                     .exprs
                     .head
                     .getGByteArray
                     .toByteArray
                 }
      transferSigData = Blake2b256.hash(sigBytes)
      transferSig     = Secp256k1.sign(transferSigData, secKey)
    } yield s"""
             |for(@wallet <- @"$pubKey") {
             |  @(wallet, "transfer")!($amount, $nonce, "${Base16.encode(transferSig)}", "$destination", "$transferStatusOut")
             |}""".stripMargin
  }

  def writeFile[F[_]: Sync](name: String, content: String): F[Unit] =
    for {
      out <- Sync[F].delay { new PrintWriter(name) }
      _   <- Sync[F].delay { out.println(content) }
      _   <- Sync[F].delay { out.close() }
    } yield ()

  def writeRhoFiles[F[_]: Sync](
      bondKey: String,
      ethAddress: String,
      amount: Long,
      secKey: String,
      pubKey: String
  )(implicit scheduler: Scheduler): F[Unit] =
    for {
      runtimeDir     <- Sync[F].delay { Files.createTempDirectory("casper-bonding-helper-") }
      activeRuntime  <- Sync[F].delay { Runtime.create(runtimeDir, 1024L * 1024 * 1024) }
      runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
      unlockCode     <- unlockDeploy[F](ethAddress, pubKey, secKey)(Sync[F], runtimeManager, scheduler)
      forwardCode    = bondingForwarderDeploy(bondKey, ethAddress)
      bondCode <- bondDeploy[F](amount, ethAddress, pubKey, secKey)(
                   Sync[F],
                   runtimeManager,
                   scheduler
                 )
      _ <- writeFile(s"unlock_${ethAddress}.rho", unlockCode)
      _ <- writeFile(s"forward_${ethAddress}_${bondKey}.rho", forwardCode)
      _ <- writeFile(s"bond_${ethAddress}.rho", bondCode)
    } yield ()
}
