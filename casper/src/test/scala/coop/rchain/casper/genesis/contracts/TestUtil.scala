package coop.rchain.casper.genesis.contracts

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
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
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

import cats.implicits._

import scala.concurrent.duration._

object TestUtil {

  private val rhoSpecDeploy: DeployData =
    DeployData(
      deployer = ProtoUtil.stringToByteString(
        "4ae94eb0b2d7df529f7ae68863221d5adda402fc54303a3d90a8a7a279326828"
      ),
      timestamp = 1539808849271L,
      term = CompiledRholangSource("RhoSpecContract.rho").code,
      phloLimit = accounting.MAX_VALUE
    )

  def runtime[F[_]: Concurrent: ContextShift, G[_]](
      extraServices: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler, parallel: Parallel[F, G]): F[Runtime[F]] = {
    implicit val log: Log[F]            = new Log.NOPLog[F]
    implicit val metricsEff: Metrics[F] = new metrics.Metrics.MetricsNOP[F]
    for {
      runtime <- TestRuntime.create[F, G](extraServices)
      _       <- Runtime.injectEmptyRegistryRoot[F](runtime.space, runtime.replaySpace)
    } yield runtime
  }

  private def evalDeploy(deploy: DeployData, runtime: Runtime[Task])(
      implicit scheduler: Scheduler
  ): Unit = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    val term = mkTerm(deploy.term).right.get
    evalTerm(term, runtime)
  }

  private def evalTerm(
      term: Par,
      runtime: Runtime[Task]
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): Unit =
    (for {
      _ <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)
      _ <- runtime.reducer.phlo
    } yield ()).runSyncUnsafe(30.seconds)

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
      additionalSystemProcesses: Seq[SystemProcess.Definition[Task]]
  )(
      implicit scheduler: Scheduler
  ): Unit = {
    val runtime =
      TestUtil.runtime[Task, Task.Par](additionalSystemProcesses).runSyncUnsafe(5.seconds)
    evalDeploy(StandardDeploys.listOps, runtime)(implicitly)
    evalDeploy(rhoSpecDeploy, runtime)(implicitly)
    otherLibs.foreach(evalDeploy(_, runtime))
    val rand = Blake2b512Random(128)
    eval(tests.code, runtime)(implicitly, rand.splitShort(1))
  }
}
