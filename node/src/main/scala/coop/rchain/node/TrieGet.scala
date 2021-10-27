package coop.rchain.node

import cats.syntax.all._
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom}
import coop.rchain.rspace.Match
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.{HistoryStoreInstances, ValuePointer}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.rogach.scallop.ScallopConf
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path

final case class TrieGetOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "state-balance-main"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val trieHash = opt[String](
    descr = s"State hash 1",
    required = true
  )

  verify()

}

object TrieGet {
  def main(args: Array[String]): Unit = {
    val options  = TrieGetOptions(args)
    val dataDir  = options.dataDir()
    val trieHash = options.trieHash()

    implicit val log: Log[Task] = Log.log

    (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir, false)
      rspaceStore       <- rnodeStoreManager.rSpaceStores
      hs                = HistoryStoreInstances.historyStore(rspaceStore.history)
      t                 <- hs.get(Blake2b256Hash.fromHex(trieHash))
      _                 = println(t)
    } yield ()).runSyncUnsafe()
  }
}
