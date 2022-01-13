package com.revdefine.tools

import cats.syntax.all._
import com.revdefine.node.store.MongoStore.createStore
import com.revdefine.syntax.all.mongoFindObservableSyntax
import coop.rchain.models.syntax._
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom}
import coop.rchain.rspace.Match
import coop.rchain.shared.{Base16, Log}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.rogach.scallop.ScallopConf
import coop.rchain.node.revvaultexport.{RhoTrieTraverser, StateBalances}
import coop.rchain.node.revvaultexport.mainnet1.StateBalanceMain.{
  genesisVaultMapDepth,
  genesisVaultMapPar
}

import java.io.PrintWriter
import java.nio.file.Path

final case class CheckBalanceOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "replay"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )

  val mongoUri = opt[String](
    descr = "mongo uri",
    required = true
  )
  verify()
}

object CheckBalance {
  def main(args: Array[String]): Unit = {
    val options  = CheckBalanceOptions(args)
    val dataDir  = options.dataDir()
    val mongoUri = options.mongoUri()

    import coop.rchain.rholang.interpreter.storage._
    implicit val span                                           = NoopSpan[Task]()
    implicit val log: Log[Task]                                 = Log.log
    implicit val metrics                                        = new Metrics.MetricsNOP[Task]()
    implicit val m: Match[Task, BindPattern, ListParWithRandom] = matchListPar[Task]

    val stateBalancesFile = dataDir.resolve("stateBalances.csv")
    val mongoBalanceFile  = dataDir.resolve("mongoBalances.csv")

    val task: Task[Unit] = for {
      _        <- log.info(s"${dataDir}")
      mongo    = createStore[Task](mongoUri)
      lt       <- mongo.lastFinalizedTransaction
      accounts <- mongo.accountCollection.find().liftToF[Task]
      _ = {
        val file = mongoBalanceFile.toFile
        val bw   = new PrintWriter(file)
        accounts.foreach { account =>
          bw.write(
            s"${Base16.encode(RhoTrieTraverser.keccakParString(account.address).drop(2))},${account.address},${account.balance}\n"
          )
        }
        bw.close()
      }
      stateBalances <- StateBalances.read[Task](
                        lt.blockHash,
                        genesisVaultMapDepth,
                        genesisVaultMapPar,
                        dataDir
                      )
      _ = {
        val file = stateBalancesFile.toFile
        val bw   = new PrintWriter(file)
        stateBalances.foreach {
          case (key, balance) => bw.write(s"${key.toHexString},${balance}\n")
        }
        bw.close()
      }
    } yield ()
    task.runSyncUnsafe()
  }
}
