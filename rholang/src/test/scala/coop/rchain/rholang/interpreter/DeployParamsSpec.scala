package coop.rchain.rholang.interpreter

import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{fixture, Assertion, Matchers, Outcome}

import scala.concurrent.Await
import scala.concurrent.duration._

class DeployParamsSpec extends fixture.FlatSpec with Matchers {
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  protected override def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt-")
    val size      = 1024L * 1024 * 10
    (for {
      sar     <- Runtime.setupRSpace[Task](dbDir, size)
      runtime <- Runtime.createWithEmptyCost[Task](sar)
      _       <- runtime.cost.set(Cost.UNSAFE_MAX)
      outcome = try {
        test(runtime)
      } finally {
        runtime.close()
        dbDir.recursivelyDelete
      }
    } yield (outcome)).unsafeRunSync
  }

  def assertStoreContains(
      space: RhoISpace[Task],
      ackChannel: Par,
      data: ListParWithRandom
  ): Task[Assertion] =
    for {
      spaceMap <- space.toMap
      datum    = spaceMap(List(ackChannel)).data.head
    } yield {
      assert(datum.a.pars == data.pars)
      assert(datum.a.randomState == data.randomState)
      assert(!datum.persist)
    }

  override type FixtureParam = Runtime[Task]

  "rho:deploy:params" should "return the parameters that are set." in { runtime =>
    implicit val rand     = Blake2b512Random(Array.empty[Byte])
    implicit val emptyEnv = Env[Par]()
    val ackChannel: Par   = GPrivate(ByteString.copyFrom(rand.next()))
    val deployerPk: Par   = GByteArray(ByteString.copyFrom(rand.next()))
    val send              = Send(Runtime.FixedChannels.GET_DEPLOY_PARAMS, List(ackChannel))
    (for {
      _ <- runtime.deployParametersRef.set(DeployParameters(deployerPk))
      _ <- runtime.reducer.eval(send)
      _ <- assertStoreContains(
            runtime.space,
            ackChannel,
            ListParWithRandom(List(deployerPk), rand)
          )
    } yield ()).runSyncUnsafe(3.seconds)

  }
}
