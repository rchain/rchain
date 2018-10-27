package coop.rchain.rholang.interpreter

import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoIStore
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.shared.PathOps._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{fixture, Assertion, Matchers, Outcome}
import scala.concurrent.Await
import scala.concurrent.duration._

class DeployParamsSpec extends fixture.FlatSpec with Matchers {
  override protected def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt")
    val size      = 1024L * 1024 * 10
    val runtime   = Runtime.create(dbDir, size)
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)

    try {
      test(runtime)
    } finally {
      runtime.close()
      dbDir.recursivelyDelete
    }
  }

  def assertStoreContains(store: RhoIStore, ackChannel: Par, data: ListParWithRandom): Assertion = {
    val datum = store.toMap(List(ackChannel)).data.head
    assert(datum.a.pars == data.pars)
    assert(datum.a.randomState == data.randomState)
    assert(!datum.persist)
  }

  override type FixtureParam = Runtime

  "rho:deploy:params" should "return the parameters that are set." in { runtime =>
    implicit val rand     = Blake2b512Random(Array.empty[Byte])
    implicit val emptyEnv = Env[Par]()
    val ackChannel: Par   = GPrivate(ByteString.copyFrom(rand.next()))
    val empty: Par        = GByteArray(ByteString.copyFrom(new Array[Byte](32)))

    val send             = Send(Runtime.FixedChannels.GET_DEPLOY_PARAMS, List(ackChannel))
    val shortLeashParams = runtime.shortLeashParams
    val task = for {
      _ <- shortLeashParams.codeHash.set(empty)
      _ <- shortLeashParams.phloRate.set(GInt(98765))
      _ <- shortLeashParams.userId.set(empty)
      _ <- shortLeashParams.timestamp.set(GInt(1234567890))
      _ <- runtime.reducer.eval(send)
    } yield ()
    Await.result(task.runAsync, 3.seconds)
    assertStoreContains(
      runtime.space.store,
      ackChannel,
      ListParWithRandom(List(empty, GInt(98765), empty, GInt(1234567890)), rand)
    )
  }
}
