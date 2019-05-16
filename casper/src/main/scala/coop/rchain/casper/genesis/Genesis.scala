package coop.rchain.casper.genesis

import java.io.{File, PrintWriter}
import java.nio.file.{Path, Paths}

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Foldable, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{ProcessedDeployUtil, RuntimeManager}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.{Log, LogSource, Time}

import scala.io.Source
import scala.util.{Failure, Success, Try}

final case class Genesis(
    shardId: String,
    timestamp: Long,
    wallets: Seq[PreWallet],
    proofOfStake: ProofOfStake,
    faucet: Boolean,
    genesisPk: PublicKey,
    vaults: Seq[Vault],
    supply: Long
)

object Genesis {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStake,
      wallets: Seq[PreWallet],
      faucetCode: String => String,
      genesisPk: PublicKey,
      vaults: Seq[Vault],
      supply: Long
  ): Seq[DeployData] =
    Seq(
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.makePoS,
      StandardDeploys.PoS,
      StandardDeploys.basicWallet,
      StandardDeploys.basicWalletFaucet,
      StandardDeploys.walletCheck,
      StandardDeploys.systemInstances,
      StandardDeploys.lockbox,
      StandardDeploys.authKey,
      StandardDeploys.rev(wallets, faucetCode, posParams),
      StandardDeploys.revVault,
      StandardDeploys.revGenerator(genesisPk, vaults, supply),
      StandardDeploys.poSGenerator(genesisPk, posParams)
    ) ++ posParams.validators.map { validator =>
      DeployData(
        deployer = ByteString.copyFrom(validator.pk.bytes),
        timestamp = System.currentTimeMillis(),
        term = validator.code,
        phloLimit = accounting.MAX_VALUE
      )
    }

  //TODO: Decide on version number and shard identifier
  def fromInputFiles[F[_]: Concurrent: Sync: Log: Time: RaiseIOError](
      maybeBondsPath: Option[String],
      numValidators: Int,
      genesisPath: Path,
      maybeWalletsPath: Option[String],
      minimumBond: Long,
      maximumBond: Long,
      faucet: Boolean,
      runtimeManager: RuntimeManager[F],
      shardId: String,
      deployTimestamp: Option[Long]
  ): F[BlockMessage] =
    for {
      timestamp <- deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      wallets   <- getWallets[F](maybeWalletsPath, genesisPath.resolve("wallets.txt"))
      bonds <- getBonds[F](
                maybeBondsPath,
                genesisPath.resolve("bonds.txt"),
                numValidators,
                genesisPath
              )
      vaults <- bonds.toList.foldMapM {
                 case (pk, stake) =>
                   RevAddress.fromPublicKey(pk) match {
                     case Some(ra) => List(Vault(ra, stake)).pure[F]
                     case None =>
                       Log[F].warn(
                         s"Validator public key $pk is invalid. Proceeding without entry."
                       ) *> List.empty[Vault].pure[F]
                   }
               }
      validators = bonds.toSeq.map(Validator.tupled)
      genesisBlock <- createGenesisBlock(
                       runtimeManager,
                       Genesis(
                         shardId = shardId,
                         timestamp = timestamp,
                         wallets = wallets,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           validators = validators
                         ),
                         faucet = faucet,
                         genesisPk = Ed25519.newKeyPair._2,
                         vaults = vaults,
                         supply = Long.MaxValue
                       )
                     )
    } yield genesisBlock

  def createGenesisBlock[F[_]: Concurrent](
      runtimeManager: RuntimeManager[F],
      genesis: Genesis
  ): F[BlockMessage] = {
    import genesis._

    val initial = withoutContracts(
      bonds = proofOfStake.validators.flatMap(Validator.unapply).toMap,
      timestamp = 1L,
      version = 1L,
      shardId = shardId
    )

    val faucetCode = if (faucet) Faucet.basicWalletFaucet _ else Faucet.noopFaucet

    withContracts(
      initial,
      proofOfStake,
      wallets,
      faucetCode,
      runtimeManager.emptyStateHash,
      runtimeManager,
      timestamp,
      genesisPk,
      vaults,
      Long.MaxValue
    )
  }

  //TODO simplify and/or remove the with* methods
  private def withContracts[F[_]: Concurrent](
      initial: BlockMessage,
      posParams: ProofOfStake,
      wallets: Seq[PreWallet],
      faucetCode: String => String,
      startHash: StateHash,
      runtimeManager: RuntimeManager[F],
      timestamp: Long,
      genesisPk: PublicKey,
      vaults: Seq[Vault],
      supply: Long
  ): F[BlockMessage] =
    withContracts(
      defaultBlessedTerms(timestamp, posParams, wallets, faucetCode, genesisPk, vaults, supply),
      initial,
      startHash,
      runtimeManager
    )

  private def withContracts[F[_]: Concurrent](
      blessedTerms: Seq[DeployData],
      initial: BlockMessage,
      startHash: StateHash,
      runtimeManager: RuntimeManager[F]
  ): F[BlockMessage] =
    runtimeManager
      .computeState(startHash)(blessedTerms)
      .map {
        case (stateHash, processedDeploys) =>
          val stateWithContracts = for {
            bd <- initial.body
            ps <- bd.state
          } yield ps.withPreStateHash(runtimeManager.emptyStateHash).withPostStateHash(stateHash)
          val version   = initial.header.get.version
          val timestamp = initial.header.get.timestamp
          val blockDeploys =
            processedDeploys.filterNot(_.status.isFailed).map(ProcessedDeployUtil.fromInternal)
          val sortedDeploys = blockDeploys.map(d => d.copy(log = d.log.sortBy(_.toByteArray)))
          val body          = Body(state = stateWithContracts, deploys = sortedDeploys)
          val header        = blockHeader(body, List.empty[ByteString], version, timestamp)
          unsignedBlockProto(body, header, List.empty[Justification], initial.shardId)
      }

  private def withoutContracts(
      bonds: Map[PublicKey, Long],
      version: Long,
      timestamp: Long,
      shardId: String
  ): BlockMessage = {
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    //sort to have deterministic order (to get reproducible hash)
    val bondsProto = bonds.toIndexedSeq.sorted.map {
      case (pk, stake) =>
        val validator = ByteString.copyFrom(pk.bytes)
        Bond(validator, stake)
    }

    val state = RChainState()
      .withBlockNumber(0)
      .withBonds(bondsProto)
    val body = Body()
      .withState(state)
    val header = blockHeader(body, List.empty[ByteString], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification], shardId)
  }

  def getWallets[F[_]: Sync: Log: RaiseIOError](
      maybeWalletsPath: Option[String],
      defaultWalletPath: Path
  ): F[Seq[PreWallet]] = {
    def walletFromFile(walletsPath: Path): F[Seq[PreWallet]] =
      for {
        maybeLines <- SourceIO.open(walletsPath).use(_.getLines).attempt
        wallets <- maybeLines match {
                    case Right(lines) =>
                      lines
                        .traverse(PreWallet.fromLine(_) match {
                          case Right(wallet) => wallet.some.pure[F]
                          case Left(errMsg) =>
                            Log[F]
                              .warn(s"Error in parsing wallets file: $errMsg")
                              .map(_ => none[PreWallet])
                        })
                        .map(_.flatten)
                    case Left(ex) =>
                      Log[F]
                        .warn(
                          s"Failed to read ${walletsPath.toAbsolutePath} for reason: ${ex.getMessage}"
                        )
                        .map(_ => Seq.empty[PreWallet])
                  }
      } yield wallets

    maybeWalletsPath match {
      case Some(walletsPathStr) =>
        val walletsPath = Paths.get(walletsPathStr)
        Monad[F].ifM(exists(walletsPath))(
          walletFromFile(walletsPath),
          Sync[F].raiseError(new Exception(s"Specified wallets file $walletsPath does not exist"))
        )
      case None =>
        Monad[F].ifM(exists(defaultWalletPath))(
          Log[F].info(s"Using default file $defaultWalletPath") >> walletFromFile(
            defaultWalletPath
          ),
          Log[F]
            .warn(
              "No wallets file specified and no default file found. No wallets will exist at genesis."
            )
            .map(_ => Seq.empty[PreWallet])
        )
    }
  }

  private def readBonds[F[_]: Sync: RaiseIOError](
      bondsPath: Path
  ): F[Map[PublicKey, Long]] =
    SourceIO
      .open[F](bondsPath)
      .use(_.getLines)
      .map { lines =>
        Try {
          lines
            .map(line => {
              val Array(pk, stake) = line.trim.split(" ")
              PublicKey(Base16.unsafeDecode(pk)) -> stake.toLong
            })
            .toMap
        }
      }
      .flatMap {
        case Success(bonds) => bonds.pure[F]
        case Failure(_) =>
          Sync[F].raiseError(new Exception(s"Bonds file $bondsPath cannot be parsed"))
      }

  def getBonds[F[_]: Sync: Log: RaiseIOError](
      maybeBondsPath: Option[String],
      defaultBondsPath: Path,
      numValidators: Int,
      genesisPath: Path
  ): F[Map[PublicKey, Long]] =
    maybeBondsPath match {
      case Some(bondsPathStr) =>
        val bondsPath = Paths.get(bondsPathStr)
        Monad[F].ifM(exists(bondsPath))(
          readBonds(bondsPath),
          Sync[F].raiseError(new Exception(s"Specified bonds file $bondsPath does not exist"))
        )
      case None =>
        Monad[F].ifM(exists(defaultBondsPath))(
          Log[F].info(s"Using default file $defaultBondsPath") >> readBonds(defaultBondsPath),
          Log[F].warn(
            s"Bonds file was not specified and default bonds file does not exist. Falling back on generating random validators."
          ) >> newValidators[F](numValidators, genesisPath)
        )
    }

  private def newValidators[F[_]: Monad: Sync: Log](
      numValidators: Int,
      genesisPath: Path
  ): F[Map[PublicKey, Long]] = {
    val keys         = Vector.fill(numValidators)(Ed25519.newKeyPair)
    val (_, pubKeys) = keys.unzip
    val bonds        = pubKeys.zipWithIndex.toMap.mapValues(_.toLong + 1L)
    val genBondsFile = genesisPath.resolve(s"bonds.txt").toFile

    val skFiles =
      Sync[F].delay(genesisPath.toFile.mkdir()) >>
        Sync[F].delay {
          keys.foreach { //create files showing the secret key for each public key
            case (sec, pub) =>
              val sk      = Base16.encode(sec.bytes)
              val pk      = Base16.encode(pub.bytes)
              val skFile  = genesisPath.resolve(s"$pk.sk").toFile
              val printer = new PrintWriter(skFile)
              printer.println(sk)
              printer.close()
          }
        }

    //create bonds file for editing/future use
    for {
      _       <- skFiles
      printer <- Sync[F].delay { new PrintWriter(genBondsFile) }
      _ <- Foldable[List].foldM[F, (PublicKey, Long), Unit](bonds.toList, ()) {
            case (_, (pub, stake)) =>
              val pk = Base16.encode(pub.bytes)
              Log[F].info(s"Created validator $pk with bond $stake") *>
                Sync[F].delay { printer.println(s"$pk $stake") }
          }
      _ <- Sync[F].delay { printer.close() }
    } yield bonds
  }

}
