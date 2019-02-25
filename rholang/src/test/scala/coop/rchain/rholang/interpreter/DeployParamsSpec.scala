package coop.rchain.rholang.interpreter

import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoIStore
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._
import coop.rchain.shared.StoreType
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{fixture, Assertion, Matchers, Outcome}

import scala.concurrent.Await
import scala.concurrent.duration._

class DeployParamsSpec extends fixture.FlatSpec with Matchers {
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

  override protected def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt")
    val size      = 1024L * 1024 * 10
    (for {
      runtime <- Runtime.create[Task, Task.Par](dbDir, size, StoreType.LMDB)
      _       <- runtime.reducer.setPhlo(Cost(Integer.MAX_VALUE))
      outcome = try {
        test(runtime)
      } finally {
        runtime.close()
        dbDir.recursivelyDelete
      }
    } yield (outcome)).unsafeRunSync
  }

  def assertStoreContains(
      store: RhoIStore[Task],
      ackChannel: Par,
      data: ListParWithRandom
  ): Assertion = {
    val datum = store.toMap(List(ackChannel)).data.head
    assert(datum.a.pars == data.pars)
    assert(datum.a.randomState == data.randomState)
    assert(!datum.persist)
  }

  override type FixtureParam = Runtime[Task]

  "rho:deploy:params" should "return the parameters that are set." in { runtime =>
    implicit val rand     = Blake2b512Random(Array.empty[Byte])
    implicit val emptyEnv = Env[Par]()
    val ackChannel: Par   = GPrivate(ByteString.copyFrom(rand.next()))
    val empty: Par        = GByteArray(ByteString.copyFrom(new Array[Byte](32)))
    val phloRate: Par     = GInt(98765)
    val timestamp: Par    = GInt(1234567890)
    val send              = Send(Runtime.FixedChannels.GET_DEPLOY_PARAMS, List(ackChannel))
    val shortLeashParams  = runtime.shortLeashParams
    val task = for {
      _ <- shortLeashParams.setParams(empty, phloRate, empty, timestamp)
      _ <- runtime.reducer.eval(send)
    } yield ()
    Await.result(task.runToFuture, 3.seconds)
    assertStoreContains(
      runtime.space.store,
      ackChannel,
      ListParWithRandom(List(empty, phloRate, empty, timestamp), rand)
    )
  }
}
