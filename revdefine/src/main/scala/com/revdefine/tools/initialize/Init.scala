package com.revdefine.tools.initialize

import cats.syntax.all._
import com.revdefine.MONGO_URI
import com.revdefine.node.store.MongoStore
import com.revdefine.node.web.account.Account.{Account, POS_ADDRESS}
import com.revdefine.node.web.transfer.Transfer.{Genesis, Transaction}
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.syntax.syntaxBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.node.web.Transaction.SCodec
import coop.rchain.shared.Log
import monix.eval.Task
import coop.rchain.models.syntax._
import coop.rchain.blockstorage.syntax._
import org.rogach.scallop.{ScallopConf, ScallopOption}
import scodec.codecs.utf8
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path
import scala.io.Source

final case class InitOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "initialize"

  val genesisBlockHash: ScallopOption[String] = opt[String](
    descr = "target block hash init from",
    required = true
  )

  val genesisWalletFile: ScallopOption[Path] = opt[Path](
    descr = "Genesis block wallet file.",
    required = true
  )

  val genesisBondsFile: ScallopOption[Path] = opt[Path](
    descr = "Genesis bonds file."
  )

  val dataDir: ScallopOption[Path] = opt[Path](
    descr = "data directory",
    required = true
  )

  verify()
}

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements"))
object Init {
  def main(args: Array[String]): Unit = {
    val options                 = InitOptions(args)
    val GenesisBlockHash        = options.genesisBlockHash()
    val GenesisBlockBalanceFile = options.genesisWalletFile()
    val targetGenesisBondsFile  = options.genesisBondsFile()
    val dataDir                 = options.dataDir()
    val totalBonds = {
      val f = Source.fromFile(targetGenesisBondsFile.toFile)
      val result = f.getLines().foldLeft(0L) {
        case (acc, line) =>
          line.split(" ") match {
            case Array(_, amount) => acc + amount.toLong
            case _                => throw new Exception("invalid genesis bonds file")
          }
      }
      f.close()
      result
    }
    val balance = {
      val balancef = Source.fromFile(GenesisBlockBalanceFile.toFile)
      val result = balancef
        .getLines()
        .foldLeft(Map.empty[String, Long])(
          (acc, s) =>
            s.split(",") match {
              case Array(addr, balance) => acc + ((addr, balance.toLong))
            }
        )
      balancef.close()
      result + ((POS_ADDRESS, totalBonds))
    }

    implicit val log: Log[Task] = Log.log
    val accounts                = balance.toList.map { case (s, v) => Account(s, v, List.empty) }
    (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      genesisBlock <- blockStore.getUnsafe(
                       GenesisBlockHash.unsafeHexToByteString
                     )
      genesisTransactions = {
        val transferDeploys = genesisBlock.body.deploys.drop(8)
        val f               = Source.fromFile(GenesisBlockBalanceFile.toFile)
        val result = f
          .getLines()
          .map { s =>
            s.split(",") match {
              case Array(revAddress, amount) =>
                //https://obs-prehf1.dev.rchain.coop/api/blocks/0/0
                val deploy = transferDeploys.find(p => p.deploy.data.term.contains(revAddress)).get
                Transaction(
                  toAddr = revAddress,
                  fromAddr = "Genesis",
                  amount = amount.toLong,
                  blockNumber = 0,
                  blockHash = GenesisBlockHash,
                  deployId = deploy.deploy.sig.toHexString,
                  timestamp = deploy.deploy.data.timestamp,
                  isFinalized = true,
                  isSucceeded = true,
                  transactionType = Genesis(),
                  reason = "empty"
                )
              case _ => throw new Exception("Invalid wallets file")
            }
          }
          .toList
        f.close
        result
      }
      mongoStore = MongoStore.createStore[Task](MONGO_URI)
      _          <- mongoStore.insertAccount(accounts)
      _          <- log.info("Start storing the transactions into mongo")
      _          <- mongoStore.insertTransaction(genesisTransactions)
      _          <- log.info("Done storing into mongo")
    } yield ()).runSyncUnsafe()
  }
}
