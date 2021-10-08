package coop.rchain.node

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagRepresentation}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.syntax.syntaxBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import org.lmdbjava._
import scodec.bits.ByteVector
import monix.execution.Scheduler.Implicits.global

import java.io.PrintWriter
import java.nio.file.Path
import scala.collection.JavaConverters._
object StateHash {
  val errorStateHash = "4016004a1ded0740252de9374200f8294a85b7793c9506adaa29f79f6948ad09"
  val targetStateHash = "286e8596f1421033385fca794b4fa79ff975329f13fe73918166d5bda2f8e285"
  val targetBlockHash = "23f3c734b15f2aa71428f8125dfafea5acf3107544eb0acf4840edf6988bc505"

  def main(args: Array[String]): Unit = {
    implicit val log = Log.log[Task]
    implicit val sp = Span.noop[Task]
    implicit val m = new MetricsNOP[Task]

    val task : Task[Unit] = for{
      rnodeStoreManager      <- RNodeKeyValueStoreManager[Task](Path.of("/rchain/node29/rnode"), false)
      runtimeManagerWithHistory <- {
        rnodeStoreManager.rSpaceStores(false).flatMap(RuntimeManager.createWithHistory[Task])
      }
      (runtimeManager, _) = runtimeManagerWithHistory
      blockStore <- KeyValueBlockStore(rnodeStoreManager)
      blockOpt <- blockStore.get(ByteString.copyFrom(Base16.unsafeDecode(targetBlockHash)))
      b = blockOpt.get
      blockDagStorage <- BlockDagKeyValueStorage.create[Task](rnodeStoreManager)
      dag <- blockDagStorage.getRepresentation
      replay = {
        implicit val bs = blockStore
        for {
          replayResult <- InterpreterUtil.replayBlock(b.body.state.preStateHash, b, dag, runtimeManager)
          _ <- replayResult match {
            case Left(e) => log.info(s"replay failed ${e}")
            case Right(g) => log.info(s"replay succeeded ${g}")
          }
        } yield ()
      }
      r = fs2.Stream.repeatEval(replay)
      _<- r.compile.toList

    } yield ()
    task.runSyncUnsafe()
  }

}
