package coop.rchain.node.balance

import cats.effect._
import coop.rchain.crypto.codec.Base16
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.rogach.scallop.ScallopConf

import java.io.PrintWriter
import java.nio.file.{Files, Path}
final case class Options(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "balance-main"

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
  val transactionDir = opt[Path](
    descr = "Transaction server lmdb path.",
    required = true
  )

  verify()

}
object BalanceMain {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val options        = Options(args)
    val dataDir        = options.dataDir()
    val blockHash      = options.blockHash()
    val walletPath     = options.walletPath()
    val bondsPath      = options.bondPath()
    val transactionDir = options.transactionDir()
    val outputDir      = options.outputDir()
    if (!Files.exists(outputDir)) {
      Files.createDirectory(outputDir)
    }

    val stateBalancesFile       = outputDir.resolve("stateBalances.csv")
    val transactionBalancesFile = outputDir.resolve("transactionBalances.csv")

    implicit val tc = Concurrent[Task]

    val task: Task[Unit] = for {
      stateBalances <- StateBalances.main(blockHash, dataDir)
      transactionBalances <- TransactionBalances.main(
                              dataDir,
                              walletPath,
                              bondsPath,
                              transactionDir,
                              blockHash
                            )
      _ = {
        val file = stateBalancesFile.toFile
        val bw   = new PrintWriter(file)
        stateBalances.foreach {
          case (key, balance) => bw.write(s"${Base16.encode(key.toByteArray)},${balance}\n")
        }
        bw.close()
      }
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
    } yield ()

    task.runSyncUnsafe()
  }
}
