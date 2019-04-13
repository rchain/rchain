package coop.rchain.casper.genesis.contracts

import cats.{FlatMap, Parallel}
import cats.effect.{Concurrent, ContextShift, Sync}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.{ParBuilder, Runtime, TestRuntime, accounting}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.shared.Log
import cats.implicits._
import coop.rchain.casper.util.rholang.RuntimeManager

import scala.concurrent.ExecutionContext

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
  )(implicit context: ExecutionContext, parallel: Parallel[F, G]): F[Runtime[F]] = {
    implicit val log: Log[F]            = new Log.NOPLog[F]
    implicit val metricsEff: Metrics[F] = new metrics.Metrics.MetricsNOP[F]
    for {
      runtime <- TestRuntime.create[F, G](extraServices)
      _       <- Runtime.injectEmptyRegistryRoot[F](runtime.space, runtime.replaySpace)
    } yield runtime
  }

  def setupRuntime[F[_]: Concurrent: ContextShift, G[_]: Parallel[F, ?[_]]](
      genesisSetup: RuntimeManager[F] => F[BlockMessage],
      otherLibs: Seq[DeployData],
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]]
  )(
      implicit context: ExecutionContext
  ): F[Runtime[F]] =
    for {
      runtime        <- TestUtil.runtime[F, G](additionalSystemProcesses)
      runtimeManager <- RuntimeManager.fromRuntime(runtime)

      _ <- genesisSetup(runtimeManager)

      _ <- evalDeploy(rhoSpecDeploy, runtime)
      _ <- otherLibs.toList.traverse(evalDeploy(_, runtime))

      // reset the deployParams.userId before executing the test
      // otherwise it'd execute as the deployer of last deployed contract
      _ <- runtime.shortLeashParams.updateParams(old => old.copy(userId = Par()))
    } yield (runtime)

  private def evalDeploy[F[_]: Sync](
      deploy: DeployData,
      runtime: Runtime[F]
  )(
      implicit context: ExecutionContext
  ): F[Unit] = {
    val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    eval(deploy.term, runtime)(implicitly, implicitly, rand)
  }

  def eval[F[_]: Sync](
      code: String,
      runtime: Runtime[F]
  )(implicit context: ExecutionContext, rand: Blake2b512Random): F[Unit] =
    ParBuilder[F].buildNormalizedTerm(code) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: Runtime[F]
  )(implicit context: ExecutionContext, rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)
      _ <- runtime.reducer.phlo
    } yield ()

}
