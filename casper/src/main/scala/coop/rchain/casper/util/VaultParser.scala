package coop.rchain.casper.util

import cats.effect.{Async, Resource, Sync}
import cats.syntax.all._
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log
import fs2.text
import fs2.io.file.{Files, Path}

object VaultParser {

  /**
    * Parser for wallets file used in genesis ceremony to set initial REV accounts.
    *
    * TODO: Create Blocker scheduler for file operations. For now it's ok because it's used only once at genesis.
    *   Cats Effect 3 removed ContextShift and Blocker.
    *    - https://typelevel.org/cats-effect/docs/migration-guide#blocker
    */
  def parse[F[_]: Async: Log](vaultsPath: Path): F[Seq[Vault]] = {
    def readLines =
      Files[F]
        .readAll(vaultsPath)
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.trim.nonEmpty)
        .evalMap { line =>
          val lineFormat = "<REV_address>,<balance>"
          val lineRegex  = raw"^([1-9a-zA-Z]+),([0-9]+)".r.unanchored

          // Line parser
          val revAndBalance = tryWithMsg {
            line match { case lineRegex(fst, snd, _*) => (fst, snd) }
          }(failMsg = s"INVALID LINE FORMAT: `$lineFormat`, actual: `$line`")

          // REV address parser, converter to REV address
          def revAddress(revAddressString: String) =
            RevAddress
              .parse(revAddressString)
              .leftMap(ex => new Exception(s"PARSE ERROR: $ex, `$lineFormat`, actual: `$line`"))
              .liftTo[F]

          // Balance parser
          def revBalance(revBalanceStr: String) = tryWithMsg(revBalanceStr.toLong)(
            failMsg = s"INVALID WALLET BALANCE `$revBalanceStr`. Please put positive number."
          )

          // Parse REV address and balance
          revAndBalance
            .flatMap(_.bitraverse(revAddress, revBalance))
            .map(Vault.tupled)
            .tupleRight(line)
        }
        .evalMap {
          case (vault, line) => Log[F].info(s"Wallet loaded: $line").as(vault)
        }
        .compile
        .to(Seq)
        .adaptErr {
          case ex: Throwable =>
            new Exception(s"FAILED PARSING WALLETS FILE: $vaultsPath\n$ex")
        }
    Resource.unit[F].use(_ => readLines)
  }

  def parse[F[_]: Async: Log](vaultsPathStr: String): F[Seq[Vault]] = {
    val vaultsPath = Path(vaultsPathStr)

    def readLines =
      Files[F]
        .exists(vaultsPath)
        .ifM(
          Log[F].info(s"Parsing wallets file $vaultsPath.") >> parse(vaultsPath),
          Log[F]
            .warn(s"WALLETS FILE NOT FOUND: $vaultsPath. No vaults will be put in genesis block.")
            .as(Seq.empty[Vault])
        )
    Resource.unit[F].use(_ => readLines)
  }

  private def tryWithMsg[F[_]: Sync, A](f: => A)(failMsg: => String) =
    Sync[F].delay(f).adaptError { case _ => new Exception(failMsg) }
}
