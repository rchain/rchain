package coop.rchain.casper.util

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{exists, SourceIO}
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success, Try}

object VaultParser {
  def parse[F[_]: Sync: Log: RaiseIOError](
      vaultsPathStr: String
  ): F[Seq[Vault]] = {
    val vaultsPath = Paths.get(vaultsPathStr)
    exists(vaultsPath).ifM(
      Log[F]
        .info(s"Parsing wallets file ${vaultsPath}.") >>
        parse[F](vaultsPath),
      Log[F]
        .warn(s"WALLETS FILE NOT FOUND: ${vaultsPath}. No vaults will be put in genesis block.")
        .as(Seq.empty[Vault])
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
                      s"FAILED TO READ ${vaultsPath.toAbsolutePath}: ${ex.getMessage}"
                    )
                }
      vaults <- lines.traverse(parseLine[F])
    } yield vaults

  private def parseLine[F[_]: Sync: Log: RaiseIOError](line: String): F[Vault] =
    Sync[F]
      .fromEither(
        fromLine(line)
          .leftMap(errMsg => new RuntimeException(s"FAILED PARSING WALLETS FILE: $errMsg"))
      ) <* Log[F].info(s"Wallet loaded: ${line}")

  private def fromLine(line: String): Either[String, Vault] = line.split(",") match {
    case Array(ethAddressString, initRevBalanceStr, _) =>
      Try(initRevBalanceStr.toLong) match {
        case Success(initRevBalance) if initRevBalance >= 0 =>
          RevAddress
            .fromEthAddress(ethAddressString)
            .map(Vault(_, initRevBalance))
            .toRight(s"INVALID ETH ADDRESS while parsing wallets file: $ethAddressString")
        case Failure(_) =>
          Left(s"INVALID WALLET BALANCE $initRevBalanceStr. Please put positive long.")
      }

    case _ => Left(s"INVALID WALLET FORMAT:\n$line")
  }

}
