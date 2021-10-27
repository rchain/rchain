package coop.rchain.node

import cats.syntax.all._
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rspace.Match
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.{ColdStoreInstances, ContinuationsLeaf, DataLeaf, HistoryStoreInstances, JoinsLeaf, ValuePointer}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.rogach.scallop.ScallopConf
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.syntax._
import coop.rchain.rspace.serializers.ScodecSerialize.{decodeContinuations, decodeDatums, decodeJoins}
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path

final case class ColdStoreOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "state-balance-main"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val targetHash = opt[String](
    descr = s"State hash 1",
    required = true
  )

  verify()

}

object GetColdStore {
  def main(args: Array[String]): Unit = {
    val options    = ColdStoreOptions(args)
    val dataDir    = options.dataDir()
    val targetHash = options.targetHash()

    implicit val log: Log[Task] = Log.log
    import coop.rchain.rholang.interpreter.storage._

    (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir, false)
      rspaceStore       <- rnodeStoreManager.rSpaceStores
      cold              = ColdStoreInstances.coldStore[Task](rspaceStore.cold)
      itemOpt           <- cold.get(Blake2b256Hash.fromHex(targetHash))
      item              = itemOpt.get
      decoded = item match {
        case JoinsLeaf(bytes) => decodeJoins[Par](bytes)
        case DataLeaf(bytes) =>decodeDatums[ListParWithRandom](bytes)
        case ContinuationsLeaf(bytes) => decodeContinuations[BindPattern, TaggedContinuation](bytes)
      }
      _ = println(decoded)
    } yield ()).runSyncUnsafe()
  }
}
