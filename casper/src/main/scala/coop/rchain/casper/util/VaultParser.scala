package coop.rchain.casper.util

import java.io.FileNotFoundException
import java.nio.file.{Path, Paths}

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{FileNotFound, SourceIO, exists}
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log

import scala.util.{Failure, Success, Try}

object VaultParser {
  def parse[F[_]: Sync: Log: RaiseIOError](
      maybeVaultPath: Option[String],
      defaultVaultPath: Path
  ): F[Seq[Vault]] =
    maybeVaultPath match {
      case Some(vaultsPathStr) =>
        val vaultsPath = Paths.get(vaultsPathStr)
        exists(vaultsPath).ifM(
          parse[F](vaultsPath),
          RaiseIOError[F].raise(
            FileNotFound(
              new FileNotFoundException(s"Specified vaults file $vaultsPath does not exist")
            )
          )
        )
      case None =>
        exists(defaultVaultPath).ifM(
          Log[F].info(s"Using default file $defaultVaultPath") >> parse[F](
            defaultVaultPath
          ),
          Log[F]
            .warn(
              "No vaults file specified and no default file found. No vaults will exist at genesis."
            )
            .map(_ => Seq.empty[Vault])
        )
    }

  def parse[F[_]: Sync: Log: RaiseIOError](vaultsPath: Path): F[Seq[Vault]] =
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

}
