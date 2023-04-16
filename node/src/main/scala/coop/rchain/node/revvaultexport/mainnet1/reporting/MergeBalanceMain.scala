package coop.rchain.node.revvaultexport.mainnet1.reporting

import cats.effect.{IO, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.syntax._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.models.syntax._
import coop.rchain.shared.{Base16, Log}
import coop.rchain.shared.syntax._
import org.rogach.scallop.ScallopConf

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.io._

/**
  * The `merge-balance-main` would generate a csv file and the format is like below
  *
  * ```
  * revAddress1HashedValue,revAddress,stateBalance1,transactionBalance1,adjustedStateBalance1,accountType
  * revAddress1HashedValue,revAddress,stateBalance1,transactionBalance1,adjustedStateBalance1,accountType
  * revAddress1HashedValue,revAddress,stateBalance1,transactionBalance1,adjustedStateBalance1,accountType
  * ...
  * ```
  *
  * This command would just merge `transaciontBalance` and `stateBalance` .
  * If `stateBalance` != `transactionBalance`, the script would check balance
  * by ["findOrCreate" rholang term](https://github.com/rchain/pyrchain/blob/master/rchain/vault.py#L18-L29)
  * to get the adjusted balance.
  *
  */
final case class MergeBalanceOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "merge-balance-main"
  val blockHash = opt[String](
    descr = s"Target block for generate the balances.",
    required = true
  )
  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val outputDir = opt[Path](
    descr = s"The merge result would generate in output directory with file name -> balance.csv",
    required = true
  )
  val stateBalanceFile = opt[String](
    descr = s"State balance file.",
    required = true
  )
  val transactionBalanceFile = opt[String](
    descr = s"Transaction balance file.",
    required = true
  )
  verify()
}

object MergeBalanceMain {
  final case class Account(
      hashedAddress: String,
      address: String,
      transactionBalance: Long,
      stateBalance: Long,
      adjustedStateBalance: Long,
      accountType: String
  )

  def getVaultMap(stateBalanceFile: String, transactionBalanceFile: String) = {

    val srcTransaction = Source.fromFile(transactionBalanceFile)
    val accountMap = srcTransaction
      .getLines()
      .map(_.split(","))
      .foldLeft(Map.empty[String, Account]) {
        case (m, a) =>
          m.updated(
            a(0),
            Account(
              hashedAddress = a(0),
              address = a(1),
              transactionBalance = a(2).toLong,
              accountType = a(3),
              stateBalance = 0,
              adjustedStateBalance = 0
            )
          )

      }
    val srcState = Source.fromFile(stateBalanceFile)
    srcState.getLines
      .map(_.split(","))
      .foldLeft(accountMap) {
        case (m, a) => {
          val stateBalance  = a(1).toLong
          val hashedAddress = a(0)
          val acc = m.getOrElse(
            hashedAddress,
            Account(
              hashedAddress = a(0),
              address = "unknown",
              transactionBalance = 0,
              accountType = "NormalVault",
              stateBalance = stateBalance,
              adjustedStateBalance = 0
            )
          )

          val updatedAcc =
            acc.copy(stateBalance = stateBalance)
          m.updated(a(0), updatedAcc)
        }
      }
  }

  def getBalanceRholang(address: String) =
    s"""new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh in {
                                            |  rl!(`rho:rchain:revVault`, *RevVaultCh) |
                                            |  for (@(_, RevVault) <- RevVaultCh) {
                                            |    @RevVault!("findOrCreate", "$address", *vaultCh) |
                                            |    for (@(true, vault) <- vaultCh) {
                                            |      @vault!("balance", *balanceCh) |
                                            |      for (@balance <- balanceCh) {
                                            |        return!(balance)
                                            |      }
                                            |    }
                                            |  }
                                            |}""".stripMargin
  def getBalanceFromRholang[F[_]: Sync: Span: Log](
      revAddress: String,
      runtime: RhoRuntime[F],
      stateHash: ByteString
  ) =
    for {
      result  <- runtime.playExploratoryDeploy(getBalanceRholang(revAddress), stateHash)
      balance = result.head.exprs.head.getGInt
      _       <- Log[F].info(s"Got balance ${balance} from ${revAddress}")
    } yield balance

  def main(args: Array[String]): Unit = {
    val options                = MergeBalanceOptions(args)
    val stateBalanceFile       = options.stateBalanceFile()
    val transactionBalanceFile = options.transactionBalanceFile()
    val dataDir                = options.dataDir()
    val blockHash              = options.blockHash()
    val outputDir              = options.outputDir()
    val mergeFile              = outputDir.resolve("mergeBalances.csv")

    implicit val log: Log[IO]                    = Log.log
    implicit val span: NoopSpan[IO]              = NoopSpan[IO]()
    implicit val metrics: Metrics.MetricsNOP[IO] = new Metrics.MetricsNOP[IO]()
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: Match[IO, BindPattern, ListParWithRandom] = matchListPar[IO]

    val task: IO[Vector[Account]] = for {
      accountMap        <- getVaultMap(stateBalanceFile, transactionBalanceFile).pure[IO]
      rnodeStoreManager <- RNodeKeyValueStoreManager[IO](dataDir)
      blockStore        <- BlockStore[IO](rnodeStoreManager)
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[IO, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes                   <- RhoRuntime.createRuntimes[IO](rSpacePlay, rSpaceReplay, true, Seq.empty, Par())
      (rhoRuntime, _)            = runtimes
      blockOpt                   <- blockStore.get1(blockHash.unsafeHexToByteString)
      block                      = blockOpt.get
      postStateHash              = block.postStateHash
      adjustedAccounts <- accountMap.toList.foldLeftM(Vector.empty[Account]) {
                           case (acc, (_, account)) =>
                             if (account.transactionBalance != account.stateBalance) for {
                               _ <- Log[IO].info(s"account is not correct ${account}")
                               balance <- if (account.address != "unknown")
                                           getBalanceFromRholang[IO](
                                             account.address,
                                             rhoRuntime,
                                             postStateHash
                                           )
                                         else 0L.pure[IO]
                               adjustAccount = account.copy(
                                 adjustedStateBalance = balance
                               )
                               _ <- Log[IO]
                                     .info(
                                       s"Should Before adjusted after ${adjustAccount}"
                                     )
                             } yield acc :+ adjustAccount
                             else {
                               val adjustAccount =
                                 account.copy(adjustedStateBalance = account.stateBalance)
                               acc :+ adjustAccount
                             }.pure[IO]
                         }
    } yield adjustedAccounts

    import cats.effect.unsafe.implicits.global
    val accountMap = task.unsafeRunSync

    val file = mergeFile.toFile
    val bw   = new PrintWriter(file)
    accountMap.foreach {
      case account => {
        bw.write(
          s"${account.hashedAddress},${account.address},${account.stateBalance},${account.transactionBalance},${account.adjustedStateBalance},${account.accountType}\n"
        )
      }

    }
    bw.close()

  }
}
