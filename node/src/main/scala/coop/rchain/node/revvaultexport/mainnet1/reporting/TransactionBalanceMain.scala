package coop.rchain.node.revvaultexport.mainnet1.reporting

import cats.effect._
import coop.rchain.node.revvaultexport.reporting.TransactionBalances

import org.rogach.scallop.ScallopConf

import java.io.PrintWriter
import java.nio.file.{Files, Path}

/**
  *
  * The `transaction-balance-main` would generate a csv file and the format is like below
  * ```
  * revAddress1HashedValue,revAddress,transactionBalance1,accountType
  * revAddress2HashedValue,revAddress,transactionBalance2,accountType
  * revAddress3HashedValue,revAddress,transactionBalance3,accountType
  * ...
  * ```
  *
  * AccountType would be either `NormalVault,PerValidatorVault,PosStakingVault or CoopPosMultiSigVault`
  * The `amount` is calculated based on the `bonds.txt`, `wallets.txt` and the transaction-server data.
  *
  * **Shorcoming**: Because of currently
  * [we are not replaying systemDeploy in reporting](https://github.com/rchain/rchain/issues/2981),
  * we don't get the transfer in the systemDeploy in the transaction server and
  * the balance on POS vault and the perValidator Vault would be calculated wrong.
  *
  */
final case class TransationOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "transaction-balance-main"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val blockHash = opt[String](
    descr = s"Target block for generate the balances.",
    required = true
  )
  val outputDir = opt[Path](
    descr = s"The output dir for generating the results. There are 3 files would be generated->" +
      s"tupleSpaceBalance.csv, transactionBalance.csv and PosBalance.csv.",
    required = true
  )
  val walletPath = opt[Path](
    descr = "Genesis wallet path.",
    required = true
  )
  val bondPath = opt[Path](
    descr = "Genesis bonds path.",
    required = true
  )

  verify()

}
object TransactionBalanceMain {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val options    = TransationOptions(args.toIndexedSeq)
    val dataDir    = options.dataDir()
    val blockHash  = options.blockHash()
    val walletPath = options.walletPath()
    val bondsPath  = options.bondPath()
    val outputDir  = options.outputDir()
    if (!Files.exists(outputDir)) {
      Files.createDirectory(outputDir)
    }

    val transactionBalancesFile = outputDir.resolve("transactionBalances.csv")
    val historyFile             = outputDir.resolve("history.csv")

    implicit val tc = Async[IO]

    val task: IO[Unit] = for {
      result <- TransactionBalances.main(
                 dataDir,
                 walletPath,
                 bondsPath,
                 blockHash
               )
      (transactionBalances, history) = result
      _ = {
        val file = transactionBalancesFile.toFile
        val bw   = new PrintWriter(file)
        transactionBalances.vaultMaps.toList.foreach {
          case (key, account) =>
            bw.write(
              s"${account.keccakHashedAddress},${key},${account.amount},${account.typeString}\n"
            )
        }
        bw.close()
      }
      _ = {
        val file = historyFile.toFile
        val bw   = new PrintWriter(file)
        history.foreach {
          case t =>
            bw.write(
              s"${t.blockNumber},${t.isFinalized},${t.transaction.transactionType}," +
                s"${t.transaction.transaction.fromAddr},${t.transaction.transaction.toAddr}," +
                s"${t.transaction.transaction.amount},${t.transaction.transaction.failReason}\n"
            )
        }
        bw.close()
      }
    } yield ()

    import cats.effect.unsafe.implicits.global
    task.unsafeRunSync()
  }
}
