package coop.rchain.blockstorage.finality

import java.nio.file.Path

import cats.implicits._
import cats.effect.Sync
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.syntax._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.models.blockImplicits._
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, EitherValues, FlatSpecLike, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class LastFinalizedFileStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  implicit val raiseIOError: RaiseIOError[Task] = IOError.raiseIOErrorThroughSync[Task]

  def withLfbStorageFile[R](f: Path => Task[R]): R =
    Sync[Task].bracket {
      createTemporaryFile[Task]("lfb-storage-file", "")
    } { tmpFile =>
      f(tmpFile)
    } { _ =>
      Task.unit
    }.unsafeRunSync

  def withLfbStorage[R](f: LastFinalizedStorage[Task] => Task[R]): R =
    withLfbStorageFile { lfbStorageFile =>
      LastFinalizedFileStorage.make[Task](lfbStorageFile) >>= f
    }

  "Last finalized file storage" should "return genesis on empty state" in {
    forAll(blockElementGen()) { genesis =>
      withLfbStorage {
        _.getOrElse(genesis.blockHash) shouldBeF genesis.blockHash
      }
    }
  }

  it should "be able to restore saved last finalized block on restart" in {
    forAll(blockElementGen(), blockHashGen) { (genesis, blockHash) =>
      withLfbStorageFile { lfbStorageFile =>
        for {
          lfbStorage1 <- LastFinalizedFileStorage.make[Task](lfbStorageFile)
          _           <- lfbStorage1.put(blockHash)
          lfbStorage2 <- LastFinalizedFileStorage.make[Task](lfbStorageFile)
          _           <- lfbStorage2.getOrElse(genesis.blockHash) shouldBeF blockHash
        } yield ()
      }
    }
  }
}
