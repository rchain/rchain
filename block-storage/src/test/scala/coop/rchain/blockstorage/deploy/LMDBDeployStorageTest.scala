package coop.rchain.blockstorage.deploy

import java.nio.file.Path

import cats.effect.{Resource, Sync}
import cats.syntax.functor._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.models.blockImplicits.signedDeployDataGen
import monix.eval.Task
import monix.execution.Scheduler
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class LMDBDeployStorageTest
    extends FlatSpecLike
    with Matchers
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {

  val scheduler = Scheduler.fixedPool("lmdb-deploy-storage-test-scheduler", 4)

  implicit val raiseIOError: RaiseIOError[Task] = IOError.raiseIOErrorThroughSync[Task]

  def withStorageLocation[R](f: Path => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      createTemporaryDirectory[Task]("lmdb-deploy-storage")
    } { storagePath =>
      f(storagePath)
    } { storagePath =>
      // We have to delete directory files recursively as there is no easy way to delete
      // a non-empty folder
      deleteRecursively[Task](storagePath).void
    }
    testProgram.unsafeRunSync(scheduler)
  }

  def createDeployStorage(storagePath: Path): Resource[Task, DeployStorage[Task]] =
    LMDBDeployStorage.make[Task](LMDBDeployStorage.Config(storagePath, 1024L * 1024L * 1024L))

  "LMDB deploy storage" should "be able to restore deploys after restart" in {
    forAll(Gen.listOf(signedDeployDataGen), minSize(0), sizeRange(10)) { deploys =>
      withStorageLocation { dbPath =>
        for {
          _ <- createDeployStorage(dbPath).use { deployStorage1 =>
                deployStorage1.add(deploys)
              }
          result <- createDeployStorage(dbPath).use { deployStorage2 =>
                     deployStorage2.getUnfinalized
                   }
        } yield result shouldBe deploys.toSet
      }
    }
  }
}
