package coop.rchain.node.revvaultexport.mainnet1

import cats.effect._
import coop.rchain.models.Par
import coop.rchain.models.rholang.RhoType.RhoName
import coop.rchain.models.syntax._
import coop.rchain.node.revvaultexport.StateBalances
import coop.rchain.shared.Base16
import org.rogach.scallop.ScallopConf

import java.io.PrintWriter
import java.nio.file.{Files, Path}

/**
  * The `state-balance-main` would generate a csv file and the format is like below
  *
  *  ```
  *  revAddress1HashedValue,stateBalance1
  *  revAddress2HashedValue,stateBalance2
  *  revAddress3HashedValue,stateBalance3
  *  ...
  *  ```
  *
  * The `amount` is calculated by traversing [the treeHashMap in rholang]
  * (https://github.com/rchain/rchain/blob/dev/casper/src/main/resources/RevVault.rho#L39)
  * to get the balance.
  *
  * **Shorcoming**: The **Not Created Vault** would get 0 balance even if someone else transfer some rev to the vault.
  */
final case class StateOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "state-balance-main"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val blockHash = opt[String](
    descr = s"Target block for generate the balances.",
    required = true
  )
  val shardId = opt[String](
    descr = "ShardId of the node",
    required = true
  )
  val outputDir = opt[Path](
    descr = s"The output dir for generating the results. There are 3 files would be generated->" +
      s"tupleSpaceBalance.csv, transactionBalance.csv and PosBalance.csv.",
    required = true
  )

  verify()

}
object StateBalanceMain {

  // hard-coded value in RevVault.rho
  val genesisVaultMapDepth = 2

  // TODO support mainnet1 and mainnetx
  val mainnet1VaultMapPar: Par =
    RhoName("af4c5fc5336f34ded026393db44916a664a5dc7e48027448f278b62ce902deda".unsafeDecodeHex)

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val options   = StateOptions(args)
    val dataDir   = options.dataDir()
    val blockHash = options.blockHash()
    val shardId   = options.shardId()
    val outputDir = options.outputDir()
    if (!Files.exists(outputDir)) {
      Files.createDirectory(outputDir)
    }

    val stateBalancesFile = outputDir.resolve("stateBalances.csv")

    implicit val tc = Async[IO]

    val task: IO[Unit] = for {
      stateBalances <- StateBalances.read(
                        shardId,
                        blockHash,
                        genesisVaultMapDepth,
                        dataDir
                      )
      _ = {
        val file = stateBalancesFile.toFile
        val bw   = new PrintWriter(file)
        stateBalances.foreach {
          case (key, balance) => bw.write(s"${Base16.encode(key.toByteArray)},${balance}\n")
        }
        bw.close()
      }
    } yield ()

    import cats.effect.unsafe.implicits.global
    task.unsafeRunSync
  }
}
