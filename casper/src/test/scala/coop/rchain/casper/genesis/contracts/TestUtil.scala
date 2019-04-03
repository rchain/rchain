package coop.rchain.casper.genesis.contracts

import java.nio.file.Files

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{FlatMap, Parallel, Traverse}
import com.google.protobuf.ByteString
import coop.rchain.casper.HashSetCasperTest.createBonds
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rholang.interpreter.{accounting, ParBuilder, Runtime, TestRuntime}
import coop.rchain.shared.{Log, StoreType}
import monix.execution.Scheduler

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

  def runTestsWithDeploys[F[_]: Concurrent: ContextShift, G[_]: Parallel[F, ?[_]]](
      tests: CompiledRholangSource,
      genesisSetup: RuntimeManager[F] => F[BlockMessage],
      otherLibs: Seq[DeployData],
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]]
  )(
      implicit scheduler: Scheduler
  ): F[Unit] =
    for {
      runtime        <- TestUtil.runtime[F, G](additionalSystemProcesses)
      runtimeManager <- RuntimeManager.fromRuntime(runtime)

      _ <- genesisSetup(runtimeManager)

      _ <- evalDeploy(rhoSpecDeploy, runtime)
      _ <- otherLibs.toList.traverse(evalDeploy(_, runtime))

      // reset the deployParams.userId before executing the test
      // otherwise it'd execute as the deployer of last deployed contract
      _ <- runtime.shortLeashParams.updateParams(old => old.copy(userId = Par()))

      rand = Blake2b512Random(128)
      _    <- eval(tests.code, runtime)(implicitly, implicitly, rand.splitShort(1))
    } yield ()

  private def evalDeploy[F[_]: Sync](
      deploy: DeployData,
      runtime: Runtime[F]
  )(
      implicit scheduler: Scheduler
  ): F[Unit] = {
    val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    eval(deploy.term, runtime)(implicitly, implicitly, rand)
  }

  def eval[F[_]: Sync](
      code: String,
      runtime: Runtime[F]
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): F[Unit] =
    ParBuilder[F].buildNormalizedTerm(code) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: Runtime[F]
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)
      _ <- runtime.reducer.phlo
    } yield ()

  def defaultTerms(
      genesisPk: PublicKey,
      vaults: Seq[Vault],
      revSupply: Long,
      minimumBond: Long,
      maximumBond: Long
  ): Seq[DeployData] =
    Seq(
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.PoS,
      StandardDeploys.authKey,
      StandardDeploys.revVault,
      StandardDeploys.testRev(genesisPk, vaults, revSupply)
    )

  def buildGenesis[F[_]: Concurrent: ContextShift, G[_]: Parallel[F, ?[_]]](
      implicit scheduler: Scheduler
  ): F[BlockMessage] = {

    implicit val log: Log.NOPLog[F]     = new Log.NOPLog[F]
    implicit val metricsEff: Metrics[F] = new metrics.Metrics.MetricsNOP[F]

    val storageDirectory  = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long = 1024L * 1024

    for {
      runtime        <- Runtime.createWithEmptyCost(storageDirectory, storageSize, StoreType.LMDB)
      runtimeManager <- RuntimeManager.fromRuntime(runtime)
      genesis        <- TestUtil.defaultGenesisSetup(runtimeManager)
      _              <- runtime.close()
    } yield genesis
  }

  def defaultGenesisSetup[F[_]: Concurrent](runtimeManager: RuntimeManager[F]): F[BlockMessage] = {

    val (_, genesisPk) = Ed25519.newKeyPair

    val (_, validatorPks) = Seq.fill(4)(Ed25519.newKeyPair).unzip
    val bonds             = createBonds(validatorPks)
    val validators        = bonds.map { case (pk, stake) => Validator(pk, stake) }.toSeq

    val vaults = Traverse[List]
      .traverse(validatorPks.toList)(RevAddress.fromPublicKey)
      .get
      .map(Vault(_, 1000L))
    val minimumBond = 0L
    val maximumBond = Long.MaxValue
    val revSupply   = Long.MaxValue

    val bondDeploys =
      validators.map { validator =>
        DeployData(
          deployer = ByteString.copyFrom(validator.pk.bytes),
          term = validator.code,
          timestamp = System.currentTimeMillis(),
          phloLimit = Long.MaxValue
        )
      }

    val defaultDeploys =
      defaultTerms(
        genesisPk,
        vaults,
        revSupply,
        minimumBond,
        maximumBond
      ) ++ bondDeploys

    Genesis.withContracts(
      defaultDeploys,
      Genesis.withoutContracts(bonds, 1, 1, "TESTING-shard"),
      runtimeManager.emptyStateHash,
      runtimeManager
    )
  }
}
