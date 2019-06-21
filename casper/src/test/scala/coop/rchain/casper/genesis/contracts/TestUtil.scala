package coop.rchain.casper.genesis.contracts

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{FlatMap, Parallel}
import coop.rchain.casper.MultiParentCasperTestUtil.createBonds
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rholang.interpreter.{ParBuilderUtil, Runtime, accounting}

object TestUtil {

  private val rhoSpecDeploy: DeployData =
    DeployData(
      deployer = ProtoUtil.stringToByteString(
        "0401f5d998c9be9b1a753771920c6e968def63fe95b20c71a163a7f7311b6131ac65a49f796b5947fa9d94b0542895e7b7ebe8b91eefcbc5c7604aaf281922ccac"
      ),
      timestamp = 1559158671800L,
      term = CompiledRholangSource("RhoSpecContract.rho").code,
      phloLimit = accounting.MAX_VALUE
    )

  def setupRuntime[F[_]: Concurrent: ContextShift: Metrics, G[_]: Parallel[F, ?[_]]](
      runtime: Runtime[F],
      genesisSetup: RuntimeManager[F] => F[BlockMessage],
      otherLibs: Seq[DeployData]
  ): F[Runtime[F]] =
    for {
      runtimeManager <- RuntimeManager.fromRuntime(runtime)
      _              <- genesisSetup(runtimeManager)
      _              <- evalDeploy(rhoSpecDeploy, runtime)
      _              <- otherLibs.toList.traverse(evalDeploy(_, runtime))
      // reset the deployParams.userId before executing the test
      // otherwise it'd execute as the deployer of last deployed contract
      _ <- runtime.deployParametersRef.update(_.copy(userId = Par()))
    } yield (runtime)

  // TODO: Have this function take an additional "Genesis" argument.
  def genesisSetup[F[_]: Concurrent](runtimeManager: RuntimeManager[F], additionalDeploys: Seq[DeployData] = Seq.empty[DeployData]): F[BlockMessage] = {

    val (_, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
    val bonds             = createBonds(validatorPks)

    Genesis.createGenesisBlock(
      runtimeManager,
      Genesis(
        shardId = "RhoSpec-shard",
        timestamp = 1L,
        proofOfStake = ProofOfStake(
          minimumBond = 0L,
          maximumBond = Long.MaxValue,
          validators = bonds.map(Validator.tupled).toSeq
        ),
        genesisPk = Secp256k1.newKeyPair._2,
        vaults = Vault(
          RevAddress.fromPublicKey(Secp256k1.toPublic(ConstructDeploy.defaultSec)).get,
          900000
        ) :: bonds.toList.map {
          case (pk, stake) =>
            RevAddress.fromPublicKey(pk).map(Vault(_, stake))
        }.flattenOption,
        supply = Long.MaxValue
      ),
      additionalDeploys
    )
  }

  private def evalDeploy[F[_]: Sync](
      deploy: DeployData,
      runtime: Runtime[F]
  ): F[Unit] = {
    val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    eval(deploy.term, runtime)(implicitly, rand)
  }

  def eval[F[_]: Sync](
      code: String,
      runtime: Runtime[F]
  )(implicit rand: Blake2b512Random): F[Unit] =
    ParBuilderUtil.buildNormalizedTerm(code) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: Runtime[F]
  )(implicit rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)
      _ <- runtime.reducer.phlo
    } yield ()
}
