package coop.rchain.blockstorage.deploy

import java.nio.file.Path

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.functor._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.models.blockImplicits.signedDeployDataGen
import doobie.Transactor
import monix.eval.Task
import monix.execution.Scheduler
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.Location
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SQLiteDeployStorageTest
    extends FlatSpecLike
    with Matchers
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {

  val scheduler = Scheduler.fixedPool("sqlite-deploy-storage-test-scheduler", 4)

  implicit val raiseIOError: RaiseIOError[Task] = IOError.raiseIOErrorThroughSync[Task]

  def withDBLocation[R](f: Path => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      createTemporaryFile[Task]("sqlite-deploy-storage", "test")
    } { dbPath =>
      f(dbPath)
    } { dbPath =>
      deleteIfExists[Task](dbPath).void
    }
    testProgram.unsafeRunSync(scheduler)
  }

  def createDeployStorage(dbPath: Path): Task[DeployStorage[Task]] = {
    val dataSource = s"jdbc:sqlite:$dbPath"
    val conf =
      Flyway
        .configure()
        .dataSource(dataSource, "", "")
        .locations(new Location("classpath:/db/migration"))
    val flyway = conf.load()
    flyway.migrate()
    val transactor = Transactor.fromDriverManager[Task]("org.sqlite.JDBC", dataSource)
    SQLiteDeployStorage.make[Task](transactor)
  }

  "SQLiteDeployStorage" should "be able to restore deploys after restart" in {
    forAll(Gen.listOf(signedDeployDataGen), minSize(0), sizeRange(10)) { deploys =>
      withDBLocation { dbPath =>
        for {
          deployStorage1 <- createDeployStorage(dbPath)
          _              <- deployStorage1.put(deploys)
          deployStorage2 <- createDeployStorage(dbPath)
          result         <- deployStorage2.getUnfinalized
        } yield result shouldBe deploys.toSet
      }
    }
  }
}
