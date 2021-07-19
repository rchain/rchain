package com.revdefine.tools.initialize

import cats.syntax.all._
import com.google.protobuf.ByteString
import com.revdefine.MONGO_URI
import com.revdefine.node.store.MongoStore
import com.revdefine.tools.initialize.SpecialCases.{
  allSpecialCases,
  blockHashAt908400,
  blockNumberAt908400,
  emptyReason,
  timestampAt908400
}
import com.revdefine.node.web.account.Account.{
  perValidatorAddresses,
  Account,
  BURNED_ADDRESS,
  BURNED_TAG,
  COOP_MULTI_SIG_ADDRESS,
  COOP_MULTI_SIG_TAG,
  GENESIS_TAG,
  PER_VALIDATOR_TAG,
  POS_ADDRESS,
  POS_TAG
}
import coop.rchain.node.web.Transaction.SCodec
import com.revdefine.node.web.transfer.Transfer.{
  fromRnodeTransaction,
  CloseBlock,
  CustomType,
  DEFAULT_REASON,
  Genesis,
  MINT_FROM_ADDRESS,
  PreCharge,
  Refund,
  Slashing,
  Transaction,
  TransactionType,
  UserDeploy
}
import com.revdefine.origin.revvaultexport.RhoTrieTraverser
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.rogach.scallop.{ScallopConf, ScallopOption}
import scodec.codecs.utf8
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.syntax._
import coop.rchain.crypto.codec.Base16
import monix.execution.Scheduler.Implicits.global

import java.io.{File, PrintWriter}
import java.nio.file.Path
import scala.io.Source

final case class InitOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "initialize"

  val transactionFile: ScallopOption[Path] = opt[Path](
    descr = s"All transaction history before 908318.",
    required = true
  )

  val dataDir: ScallopOption[Path] = opt[Path](
    descr = "data directory",
    required = true
  )

  val targetBlockHash: ScallopOption[String] = opt[String](
    descr = "target block hash",
    required = true
  )

  verify()
}
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Init {
  def main(args: Array[String]): Unit = {

    val options          = InitOptions(args)
    val transactionFile  = options.transactionFile()
    val dataDir          = options.dataDir()
    val targetBlockHash  = options.targetBlockHash()
    val GenesisBlockHash = "986addc3dfa12b179eaa40e38d77aec3da0530b9ca2243271436a135055229dd"

    val posAdded = readBondsTotal("bonds-genesis.txt")

    val genesisWalletNoPos = readRevAddressFile("wallets-genesis.txt")

    val genesisAddresses = genesisWalletNoPos.keySet

    val genesisWallet = genesisWalletNoPos + ((POS_ADDRESS, posAdded))

    val (transactionWallets, transfers @ _) = {
      val s = Source.fromFile(transactionFile.toFile)
      val wallets = s.getLines().zipWithIndex.foldLeft((genesisWallet, Vector.empty[Transaction])) {
        case ((accounts, transfers), (line, i)) =>
          // there is a problem with split line `a,` and `a,b`
          // it would ends with match case with both Array(x) and Array(x, y)
          // modified line to ensure pattern match only one case
          val modifiedLine = if (line.endsWith(",")) line + DEFAULT_REASON else line
          modifiedLine.split(",") match {
            case Array(
                toAddr,
                fromAddr,
                amount,
                blockNumber,
                blockHash,
                deployId,
                timestamp,
                isSucceeded,
                isFinalized,
                transType,
                reason
                ) =>
              val tt: TransactionType =
                if (transType.contains("PreCharge")) PreCharge()
                else if (transType.contains("Refund")) Refund()
                else if (transType.contains("UserDeploy")) UserDeploy()
                else if (transType.contains("CloseBlock")) CloseBlock()
                else if (transType.contains("SlashingDeploy")) Slashing()
                else throw new Exception(s"invalid transaction type $transType")
              val transfer = Transaction(
                toAddr = toAddr,
                fromAddr = fromAddr,
                amount = amount.toLong,
                blockNumber = blockNumber.toLong,
                blockHash = blockHash,
                deployId = deployId,
                timestamp = timestamp.toLong,
                isFinalized = isFinalized.toBoolean,
                isSucceeded = isSucceeded.toBoolean,
                transactionType = tt,
                reason = reason
              )
              (updateWallet(accounts, transfer), transfers :+ transfer)
            case _ =>
              println(
                s"WARNING, transfer history is not right format in $i, $line" +
                  s"f it is last line, it would be ok"
              )
              (accounts, transfers)
          }
      }
      s.close()
      wallets
    }

    val wallets908300 = readRevAddressFile("wallets_908300.txt")
    val wallets908400 = readRevAddressFile("wallets-908400-hf1.txt")
    println("checking transaction wallet and 908300 wallet")
    check(transactionWallets, wallets908300)
    check(wallets908300, transactionWallets)

    val bonds908400 = readBondsTotal("bonds-hf1.txt")

    val additionalMint = Transaction(
      transactionType = CustomType("Mistakenly mint at hard fork."),
      fromAddr = MINT_FROM_ADDRESS,
      toAddr = POS_ADDRESS,
      amount = bonds908400,
      blockHash = blockHashAt908400,
      blockNumber = blockNumberAt908400,
      deployId = blockHashAt908400,
      timestamp = timestampAt908400,
      isFinalized = true,
      isSucceeded = true,
      reason = emptyReason
    )

    val transactionWalletAfterHuman = allSpecialCases.foldLeft(transactionWallets) {
      case (accounts, transfer) =>
        updateWallet(accounts, transfer)
    }
    println("checking transaction wallet and 908400 wallet")
    check(transactionWalletAfterHuman, wallets908400)
    check(wallets908400, transactionWalletAfterHuman)

    implicit val log: Log[Task] = Log.log
    val task = (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      transactionStore <- rnodeStoreManager.database(
                           "transaction",
                           utf8,
                           SCodec.transactionResponseCodec
                         )
      genesisBlock <- blockStore.getUnsafe(
                       ByteString.copyFrom(Base16.unsafeDecode(GenesisBlockHash))
                     )
      genesisTransactions = {
        val transferDeploys = genesisBlock.body.deploys.slice(8, 82)
        Source
          .fromResource("wallets-genesis.txt")
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
                  deployId = Base16.encode(deploy.deploy.sig.toByteArray),
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
      }
      targetBlock     <- blockStore.getUnsafe(ByteString.copyFrom(Base16.unsafeDecode(targetBlockHash)))
      transactionsMap <- transactionStore.toMap
      allNewTransactions <- transactionsMap.toList.flatTraverse {
                             case (blockHash, transaction) =>
                               for {
                                 block <- blockStore.getUnsafe(
                                           ByteString.copyFrom(Base16.unsafeDecode(blockHash))
                                         )
                                 transactions = transaction.data.map(
                                   fromRnodeTransaction(_, block, isFinalized = true)
                                 )
                               } yield transactions
                           }
      sortedTransactions = allNewTransactions
        .filter(t => t.blockNumber <= targetBlock.body.state.blockNumber)
        .sortBy(_.blockNumber)

      latestWallet = (Vector(additionalMint) ++ sortedTransactions).foldLeft(
        transactionWalletAfterHuman
      ) { case (accounts, transfer) => updateWallet(accounts, transfer) }
      accounts = latestWallet.toList.map {
        case (addr, amount) =>
          val isGenesis =
            if (genesisAddresses.contains(addr)) List(GENESIS_TAG) else List.empty[String]
          val isBurned =
            if (BURNED_ADDRESS == addr) List(BURNED_TAG) else List.empty[String]
          val isPos = if (POS_ADDRESS == addr) List(POS_TAG) else List.empty[String]
          val isCoopMulti =
            if (COOP_MULTI_SIG_ADDRESS == addr) List(COOP_MULTI_SIG_TAG) else List.empty[String]
          val isPerValidator =
            if (perValidatorAddresses.contains(addr)) List(PER_VALIDATOR_TAG)
            else List.empty[String]
          Account(
            address = addr,
            balance = amount,
            tags = isGenesis ++ isPos ++ isCoopMulti ++ isPerValidator ++ isBurned
          )
      }
      allTransfers = genesisTransactions ++ transfers ++ allSpecialCases ++ Vector(additionalMint) ++ sortedTransactions
      _            = writeTransactionBalance(accounts, dataDir.resolve("balance.txt"))
      _            <- log.info("Start storing the accounts into mongo")
      mongoStore   = MongoStore.createStore[Task](MONGO_URI)
      _            <- mongoStore.insertAccount(accounts)
      _            <- log.info("Start storing the transactions into mongo")
      _            <- mongoStore.insertTransaction(allTransfers.toList)
      _            <- log.info("Done storing into mongo")
    } yield latestWallet).runSyncUnsafe()
  }

  def writeTransactionBalance(accounts: List[Account], path: Path): Unit = {
    val file = path.toFile
    val bw   = new PrintWriter(file)
    accounts.foreach { account =>
      bw.write(
        s"${Base16.encode(RhoTrieTraverser.keccakParString(account.address).drop(2))},${account.address},${account.balance}\n"
      )
    }
    bw.close()
  }

  def check(left: Map[String, Long], right: Map[String, Long]): Unit = left.foreach {
    case (addr, amount) =>
      val rightValue = right.getOrElse(addr, 0L)
      if (rightValue != amount)
        println(s"$addr is not correct, right: $rightValue, left: $amount")
  }

  def updateWallet(accounts: Map[String, Long], transfer: Transaction): Map[String, Long] =
    if (transfer.isSucceeded && transfer.isFinalized) {
      val toAccount    = accounts.getOrElse(transfer.toAddr, 0L)
      val newToAccount = toAccount + transfer.amount
      val newAccount   = accounts + ((transfer.toAddr, newToAccount))
      if (transfer.fromAddr == MINT_FROM_ADDRESS) {
        newAccount
      } else {
        val fromAccount    = newAccount.getOrElse(transfer.fromAddr, 0L)
        val newFromAccount = fromAccount - transfer.amount
        newAccount + ((transfer.fromAddr, newFromAccount))
      }

    } else accounts

  def readRevAddressFile(source: String): Map[String, Long] =
    Source
      .fromResource(source)
      .getLines()
      .foldLeft(Map.empty[String, Long]) {
        case (acc, s) =>
          s.split(",") match {
            case Array(revAddress, amount) =>
              acc + ((revAddress, amount.toLong))
            case _ => throw new Exception("Invalid wallets file")
          }
      }

  def readBondsTotal(source: String): Long = Source.fromResource(source).getLines().foldLeft(0L) {
    case (acc, line) =>
      line.split(" ") match {
        case Array(_, amount) => acc + amount.toLong
        case _                => throw new Exception("invalid genesis bonds file")
      }
  }

}
