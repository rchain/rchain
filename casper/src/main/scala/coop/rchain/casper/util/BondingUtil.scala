package coop.rchain.casper.util

import cats.effect.{ContextShift, Resource, Sync}
import cats.implicits._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.ProtoUtil.{deployDataToDeploy, sourceDeploy}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.shared.PathOps.RichPath
import java.io.PrintWriter
import java.nio.file.{Files, Path}

import cats.Parallel

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import coop.rchain.shared.Language.ignore

object BondingUtil {
  def bondingForwarderAddress(ethAddress: String): String = s"${ethAddress}_bondingForwarder"
  def bondingStatusOut(ethAddress: String): String        = s"${ethAddress}_bondingOut"
  def transferStatusOut(ethAddress: String): String       = s"${ethAddress}_transferOut"

  def bondingForwarderDeploy(bondKey: String, ethAddress: String): String =
    s"""new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
       |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
       |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
       |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
       |    for(@purse <- @"${bondingForwarderAddress(ethAddress)}"; pos <- posCh){
       |      pos!("bond", "$bondKey".hexToBytes(), "ed25519Verify", purse, "$ethAddress", "${bondingStatusOut(
         ethAddress
       )}")
       |    }
       |  }
       |}""".stripMargin

  def unlockDeploy[F[_]: Sync](ethAddress: String, pubKey: String, secKey: String)(
      implicit runtimeManager: RuntimeManager[F]
  ): F[String] =
    preWalletUnlockDeploy(ethAddress, pubKey, Base16.decode(secKey), s"${ethAddress}_unlockOut")

  def issuanceBondDeploy[F[_]: Sync](
      amount: Long,
      ethAddress: String,
      pubKey: String,
      secKey: String
  )(implicit runtimeManager: RuntimeManager[F]): F[String] =
    issuanceWalletTransferDeploy(
      0, //nonce
      amount,
      bondingForwarderAddress(ethAddress),
      transferStatusOut(ethAddress),
      pubKey,
      Base16.decode(secKey)
    )

  def preWalletUnlockDeploy[F[_]: Sync](
      ethAddress: String,
      pubKey: String,
      secKey: Array[Byte],
      statusOut: String
  )(implicit runtimeManager: RuntimeManager[F]): F[String] = {
    require(Base16.encode(Keccak256.hash(Base16.decode(pubKey)).drop(12)) == ethAddress.drop(2))
    val unlockSigDataTerm = deployDataToDeploy(
      sourceDeploy(
        s""" @"__SCALA__"!(["$pubKey", "$statusOut"].toByteArray())""",
        0L,
        accounting.MAX_VALUE
      )
    )
    runtimeManager
      .captureResults(runtimeManager.emptyStateHash, unlockSigDataTerm)
      .map { pars =>
        val unlockSigData = Keccak256.hash(pars.head.exprs.head.getGByteArray.toByteArray)
        val unlockSig     = Secp256k1.sign(unlockSigData, secKey)
        assert(Secp256k1.verify(unlockSigData, unlockSig, Base16.decode("04" + pubKey)))
        val sigString = Base16.encode(unlockSig)
        s"""new rl(`rho:registry:lookup`), WalletCheckCh in {
           |  rl!(`rho:id:oqez475nmxx9ktciscbhps18wnmnwtm6egziohc3rkdzekkmsrpuyt`, *WalletCheckCh) |
           |  for(@(_, WalletCheck) <- WalletCheckCh) {
           |    @WalletCheck!("claim", "$ethAddress", "$pubKey", "$sigString", "$statusOut")
           |  }
           |}""".stripMargin
      }
  }

  def walletTransferSigData[F[_]: Sync](
      nonce: Int,
      amount: Long,
      destination: String
  )(implicit runtimeManager: RuntimeManager[F]): F[Array[Byte]] = {
    val transferSigDataTerm = deployDataToDeploy(
      sourceDeploy(
        s""" @"__SCALA__"!([$nonce, $amount, "$destination"].toByteArray())""",
        0L,
        accounting.MAX_VALUE
      )
    )

    runtimeManager
      .captureResults(runtimeManager.emptyStateHash, transferSigDataTerm)
      .map { pars =>
        Blake2b256.hash(pars.head.exprs.head.getGByteArray.toByteArray)
      }
  }

  def issuanceWalletTransferDeploy[F[_]: Sync](
      nonce: Int,
      amount: Long,
      destination: String,
      transferStatusOut: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager[F]): F[String] =
    for {
      transferSigData <- walletTransferSigData[F](nonce, amount, destination)
      transferSig     = Secp256k1.sign(transferSigData, secKey)
    } yield s"""new rl(`rho:registry:lookup`), WalletCheckCh, result in {
               |  rl!(`rho:id:oqez475nmxx9ktciscbhps18wnmnwtm6egziohc3rkdzekkmsrpuyt`, *WalletCheckCh) |
               |  for(@(_, WalletCheck) <- WalletCheckCh) {
               |    @WalletCheck!("access", "$pubKey", *result) |
               |    for(@(true, wallet) <- result) {
               |      @wallet!("transfer", $amount, $nonce, "${Base16.encode(transferSig)}", "$destination", "$transferStatusOut")
               |    }
               |  }
               |}""".stripMargin

  def faucetBondDeploy[F[_]: Sync](
      amount: Long,
      sigAlgorithm: String,
      pubKey: String,
      secKey: Array[Byte]
  )(implicit runtimeManager: RuntimeManager[F]): F[String] =
    for {
      sigFunc <- sigAlgorithm match {
                  case "ed25519"   => ((d: Array[Byte]) => Ed25519.sign(d, secKey)).pure[F]
                  case "secp256k1" => ((d: Array[Byte]) => Secp256k1.sign(d, secKey)).pure[F]
                  case _ =>
                    Sync[F].raiseError(
                      new IllegalArgumentException(
                        "sigAlgorithm must be one of ed25519 or secp256k1"
                      )
                    )
                }
      nonce           = 0 //first and only time we will use this fresh wallet
      destination     = bondingForwarderAddress(pubKey)
      statusOut       = transferStatusOut(pubKey)
      transferSigData <- walletTransferSigData(nonce, amount, destination)
      transferSig     = sigFunc(transferSigData)
    } yield s"""new rl(`rho:registry:lookup`), SystemInstancesCh, walletCh, faucetCh in {
               |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
               |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
               |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
               |    for(faucet <- faucetCh){ faucet!($amount, "ed25519", "$pubKey", *walletCh) } |
               |    for(@[wallet] <- walletCh) {
               |      @wallet!("transfer", $amount, $nonce, "${Base16.encode(transferSig)}", "$destination", "$statusOut")
               |    }
               |  }
               |}""".stripMargin

  def writeFile[F[_]: Sync](name: String, content: String): F[Unit] = {
    val file =
      Resource.make[F, PrintWriter](Sync[F].delay { new PrintWriter(name) })(
        pw => Sync[F].delay { pw.close() }
      )
    file.use(pw => Sync[F].delay { pw.println(content) })
  }

  def makeRuntimeDir[F[_]: Sync]: Resource[F, Path] =
    Resource.make[F, Path](Sync[F].delay { Files.createTempDirectory("casper-bonding-helper-") })(
      runtimeDir => Sync[F].delay { ignore(runtimeDir.recursivelyDelete()) }
    )

  def makeRuntimeResource[M[_], F[_]](
      runtimeDirResource: Resource[M, Path]
  )(
      implicit
      syncM: Sync[M],
      parallelMF: Parallel[M, F],
      contextShift: ContextShift[M],
      scheduler: ExecutionContext
  ): Resource[M, Runtime[M]] =
    runtimeDirResource.flatMap(
      runtimeDir =>
        Resource
          .make(Runtime.create(runtimeDir, 1024L * 1024 * 1024))(_.close())
    )

  def makeRuntimeManagerResource[F[_]: Sync](
      runtimeResource: Resource[F, Runtime[F]]
  ): Resource[F, RuntimeManager[F]] =
    runtimeResource.flatMap(
      activeRuntime => Resource.make(RuntimeManager.fromRuntime(activeRuntime))(_ => Sync[F].unit)
    )

  def writeIssuanceBasedRhoFiles[M[_], F[_]](
      bondKey: String,
      ethAddress: String,
      amount: Long,
      secKey: String,
      pubKey: String
  )(
      implicit
      syncM: Sync[M],
      parallelMF: Parallel[M, F],
      contextShift: ContextShift[M],
      executionContext: ExecutionContext
  ): M[Unit] = {
    val runtimeDirResource     = makeRuntimeDir[M]
    val runtimeResource        = makeRuntimeResource[M, F](runtimeDirResource)
    val runtimeManagerResource = makeRuntimeManagerResource[M](runtimeResource)
    runtimeManagerResource.use(
      implicit runtimeManager =>
        for {
          unlockCode  <- unlockDeploy[M](ethAddress, pubKey, secKey)
          forwardCode = bondingForwarderDeploy(bondKey, ethAddress)
          bondCode    <- issuanceBondDeploy[M](amount, ethAddress, pubKey, secKey)
          _           <- writeFile[M](s"unlock_$ethAddress.rho", unlockCode)
          _           <- writeFile[M](s"forward_${ethAddress}_$bondKey.rho", forwardCode)
          _           <- writeFile[M](s"bond_$ethAddress.rho", bondCode)
        } yield ()
    )
  }

  def writeFaucetBasedRhoFiles[M[_], F[_]](
      amount: Long,
      sigAlgorithm: String,
      secKey: String,
      pubKey: String
  )(
      implicit
      syncM: Sync[M],
      parallelMF: Parallel[M, F],
      contextShift: ContextShift[M],
      executionContext: ExecutionContext
  ): M[Unit] = {
    val runtimeDirResource     = makeRuntimeDir[M]
    val runtimeResource        = makeRuntimeResource[M, F](runtimeDirResource)
    val runtimeManagerResource = makeRuntimeManagerResource[M](runtimeResource)
    runtimeManagerResource.use(
      implicit runtimeManager =>
        for {
          bondCode    <- faucetBondDeploy[M](amount, sigAlgorithm, pubKey, Base16.decode(secKey))
          forwardCode = bondingForwarderDeploy(pubKey, pubKey)
          _           <- writeFile[M](s"forward_$pubKey.rho", forwardCode)
          _           <- writeFile[M](s"bond_$pubKey.rho", bondCode)
        } yield ()
    )
  }
}
