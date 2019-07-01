package coop.rchain.casper

import java.nio.file.Files

import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.mutable

object MultiParentCasperTestUtil {

  def validateBlockStore[R](
      node: HashSetCasperTestNode[Effect]
  )(f: BlockStore[Effect] => Effect[R])(implicit metrics: Metrics[Effect], log: Log[Effect]) =
    for {
      bs     <- BlockDagStorageTestFixture.createBlockStorage[Effect](node.blockStoreDir)
      result <- f(bs)
      _      <- bs.close()
    } yield result

  def createBonds(validators: Iterable[PublicKey]): Map[PublicKey, Long] =
    validators.zipWithIndex.map { case (v, i) => v -> (2L * i.toLong + 1L) }.toMap

  def createGenesis(): BlockMessage =
    buildGenesis(buildGenesisParameters()).genesisBlock

  val defaultValidatorKeyPairs                   = (1 to 4).map(_ => Secp256k1.newKeyPair)
  val (defaultValidatorSks, defaultValidatorPks) = defaultValidatorKeyPairs.unzip

  def buildGenesisParameters(
      bondsFunction: Iterable[PublicKey] => Map[PublicKey, Long] = createBonds
  ): (Iterable[(PrivateKey, PublicKey)], Genesis) =
    buildGenesisParameters(defaultValidatorKeyPairs, bondsFunction(defaultValidatorPks))

  def buildGenesisParameters(
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)],
      bonds: Map[PublicKey, Long]
  ): (Iterable[(PrivateKey, PublicKey)], Genesis) =
    (
      validatorKeyPairs,
      Genesis(
        shardId = "MultiParentCasperSpec",
        timestamp = 0L,
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
      )
    )

  def buildGenesis(parameters: (Iterable[(PrivateKey, PublicKey)], Genesis)): GenesisContext = {
    val (validavalidatorKeyPairs, genesisParameters) = parameters
    val storageDirectory                             = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long                            = 3024L * 1024 * 10
    implicit val log: Log.NOPLog[Task]               = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task]           = new metrics.Metrics.MetricsNOP[Task]
    (for {
      activeRuntime <- Runtime
                        .createWithEmptyCost[Task, Task.Par](
                          storageDirectory,
                          storageSize
                        )
      runtimeManager <- RuntimeManager.fromRuntime[Task](activeRuntime)
      genesis <- Genesis.createGenesisBlock(
                  runtimeManager,
                  genesisParameters
                )
      _ <- activeRuntime.close()
    } yield GenesisContext(genesis, validavalidatorKeyPairs)).unsafeRunSync
  }

  case class GenesisContext(
      genesisBlock: BlockMessage,
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)]
  ) {
    def validatorSks: Iterable[PrivateKey] = validatorKeyPairs.map(_._1)
    def validatorPks: Iterable[PublicKey]  = validatorKeyPairs.map(_._2)
  }
}
