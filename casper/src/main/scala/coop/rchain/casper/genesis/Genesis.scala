package coop.rchain.casper.genesis

import java.io.{FileNotFoundException, PrintWriter}
import java.nio.file.{Path, Paths}

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Foldable, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{InternalProcessedDeploy, RuntimeManager}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.{Log, LogSource, Time}

import scala.util.{Failure, Success, Try}

final case class Genesis(
    shardId: String,
    timestamp: Long,
    proofOfStake: ProofOfStake,
    vaults: Seq[Vault],
    supply: Long
)

object Genesis {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStake,
      vaults: Seq[Vault],
      supply: Long
  ): Seq[DeployData] =
    Seq(
      StandardDeploys.registry,
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.lockbox,
      StandardDeploys.authKey,
      StandardDeploys.revVault,
      StandardDeploys.revGenerator(vaults, supply),
      StandardDeploys.poSGenerator(posParams),
      StandardDeploys.treeHashMap
    )

  //TODO: Decide on version number and shard identifier
  def fromInputFiles[F[_]: Concurrent: Sync: Log: Time: RaiseIOError](
      maybeBondsPath: Option[String],
      numValidators: Int,
      genesisPath: Path,
      maybeVaultsPath: Option[String],
      minimumBond: Long,
      maximumBond: Long,
      shardId: String,
      deployTimestamp: Option[Long]
  )(implicit runtimeManager: RuntimeManager[F]): F[BlockMessage] =
    for {
      timestamp <- deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      vaults    <- getVaults[F](maybeVaultsPath, genesisPath.resolve("wallets.txt"))
      bonds <- getBonds[F](
                maybeBondsPath,
                genesisPath.resolve("bonds.txt"),
                numValidators,
                genesisPath
              )
      validators = bonds.toSeq.map(Validator.tupled)
      genesisBlock <- createGenesisBlock(
                       runtimeManager,
                       Genesis(
                         shardId = shardId,
                         timestamp = timestamp,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           validators = validators
                         ),
                         vaults = vaults,
                         supply = Long.MaxValue
                       )
                     )
    } yield genesisBlock

  private def getVaults[F[_]: Sync: Log: RaiseIOError](
      maybeVaultPath: Option[String],
      defaultVaultPath: Path
  ): F[Seq[Vault]] =
    maybeVaultPath match {
      case Some(vaultsPathStr) =>
        val vaultsPath = Paths.get(vaultsPathStr)
        Monad[F].ifM(exists(vaultsPath))(
          vaultFromFile[F](vaultsPath),
          RaiseIOError[F].raise(
            FileNotFound(
              new FileNotFoundException(s"Specified vaults file $vaultsPath does not exist")
            )
          )
        )
      case None =>
        Monad[F].ifM(exists(defaultVaultPath))(
          Log[F].info(s"Using default file $defaultVaultPath") >> vaultFromFile[F](
            defaultVaultPath
          ),
          Log[F]
            .warn(
              "No vaults file specified and no default file found. No vaults will exist at genesis."
            )
            .map(_ => Seq.empty[Vault])
        )
    }

  def vaultFromFile[F[_]: Sync: Log: RaiseIOError](vaultsPath: Path): F[Seq[Vault]] =
    for {
      lines <- SourceIO
                .open(vaultsPath)
                .use(_.getLines)
                .adaptError {
                  case ex: Throwable =>
                    new RuntimeException(
                      s"Failed to read ${vaultsPath.toAbsolutePath} for reason: ${ex.getMessage}"
                    )
                }
      vaults <- lines.traverse(parseLine[F])
    } yield vaults

  private def parseLine[F[_]: Sync: Log: RaiseIOError](line: String): F[Vault] =
    Sync[F].fromEither(
      fromLine(line)
        .leftMap(errMsg => new RuntimeException(s"Error in parsing vaults file: $errMsg"))
    )

  private def fromLine(line: String): Either[String, Vault] = line.split(",") match {
    case Array(ethAddressString, initRevBalanceStr, _) =>
      Try(initRevBalanceStr.toLong) match {
        case Success(initRevBalance) if initRevBalance >= 0 =>
          RevAddress
            .fromEthAddress(ethAddressString)
            .map(Vault(_, initRevBalance))
            .toRight(s"Ethereum address $ethAddressString is invalid.")
        case Failure(_) =>
          Left(s"Failed to parse given initial balance $initRevBalanceStr as positive long.")
      }

    case _ => Left(s"Invalid vault specification:\n$line")
  }

  def createGenesisBlock[F[_]: Concurrent](
      runtimeManager: RuntimeManager[F],
      genesis: Genesis
  ): F[BlockMessage] = {
    import genesis._

    val blessedTerms = defaultBlessedTerms(
      timestamp,
      proofOfStake,
      vaults,
      supply = Long.MaxValue
    )

    runtimeManager
      .computeGenesis(blessedTerms, timestamp)
      .map {
        case (startHash, stateHash, processedDeploys) =>
          createInternalProcessedDeploy(genesis, startHash, stateHash, processedDeploys)
      }
  }

  private def createInternalProcessedDeploy(
      genesis: Genesis,
      startHash: StateHash,
      stateHash: StateHash,
      processedDeploys: Seq[InternalProcessedDeploy]
  ): BlockMessage = {
    import genesis._

    val state = RChainState(
      blockNumber = 0,
      bonds = bondsProto(proofOfStake),
      preStateHash = startHash,
      postStateHash = stateHash
    )

    val blockDeploys =
      processedDeploys.filterNot(_.status.isFailed).map(_.toProcessedDeploy)
    val sortedDeploys = blockDeploys.map(
      d =>
        d.copy(
          deployLog = d.deployLog.sortBy(_.toByteArray),
          paymentLog = d.paymentLog.sortBy(_.toByteArray)
        )
    )

    val body    = Body(state = Some(state), deploys = sortedDeploys)
    val version = 1L //FIXME make this part of Genesis, and pass it from upstream
    val header  = blockHeader(body, List.empty[StateHash], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification], shardId)
  }

  private def bondsProto(proofOfStake: ProofOfStake): Seq[Bond] = {
    val bonds = proofOfStake.validators.flatMap(Validator.unapply).toMap
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    //sort to have deterministic order (to get reproducible hash)
    bonds.toIndexedSeq.sorted.map {
      case (pk, stake) =>
        val validator = ByteString.copyFrom(pk.bytes)
        Bond(validator, stake)
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
    val keys         = Vector.fill(numValidators)(Secp256k1.newKeyPair)
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
              Log[F].info(s"Created validator $pk with bond $stake") >>
                Sync[F].delay { printer.println(s"$pk $stake") }
          }
      _ <- Sync[F].delay { printer.close() }
    } yield bonds
  }

}
