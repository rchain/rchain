package coop.rchain.casper.genesis.contracts

import java.nio.file.Paths

import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.TestRuntime
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.unittest.RhoSpecContract
import coop.rchain.shared.StoreType.InMem
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

object TestUtil {

  val rhoSpecDeploy: DeployData =
    DeployData(
      user = ProtoUtil.stringToByteString(
        "4ae94eb0b2d7df529f7ae68863221d5adda402fc54303a3d90a8a7a279326828"
      ),
      timestamp = 1539808849271L,
      term = RhoSpecContract.code,
      phloLimit = accounting.MAX_VALUE
    )

  def runtime(
      extraServices: Seq[SystemProcess.Definition[Task]] = Seq.empty
  )(implicit scheduler: Scheduler): Runtime[Task] = {
    implicit val log: Log[Task]            = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    for {
      runtime <- TestRuntime.create[Task, Task.Par](extraServices)
      _       <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
    } yield (runtime)
  }.unsafeRunSync

  def evalDeploy(deploy: DeployData, runtime: Runtime[Task])(
      implicit scheduler: Scheduler
  ): Unit = {
    runtime.reducer.setPhlo(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    val term = mkTerm(deploy.term).right.get
    runtime.reducer.inj(term).unsafeRunSync
  }

  def evalTerm(
      term: Par,
      runtime: Runtime[Task]
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): Unit = {
    runtime.reducer.setPhlo(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    runtime.reducer.inj(term).unsafeRunSync
  }

  def eval(
      code: String,
      runtime: Runtime[Task]
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): Unit =
    mkTerm(code) match {
      case Right(term) => evalTerm(term, runtime)
      case Left(ex)    => throw ex
    }

  def runTestsWithDeploys(
      tests: CompiledRholangSource,
      otherLibs: Seq[DeployData],
      runtime: Runtime[Task]
  )(
      implicit scheduler: Scheduler
  ): Unit = {
    val rand = Blake2b512Random(128)
    evalDeploy(StandardDeploys.listOps, runtime)(implicitly)
    evalDeploy(rhoSpecDeploy, runtime)(implicitly)
    otherLibs.foreach(evalDeploy(_, runtime))
    eval(tests.code, runtime)(implicitly, rand.splitShort(1))
  }
}
