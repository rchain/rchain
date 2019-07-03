package coop.rchain.casper

import java.nio.file.{Files, Path}

import cats.implicits._
import coop.rchain.blockstorage.{BlockDagFileStorage, BlockStore}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode.{makeBlockDagFileStorageConfig, Effect}
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
  val defaultGenesisPk                           = Secp256k1.newKeyPair._2

  def buildGenesisParameters(
      bondsFunction: Iterable[PublicKey] => Map[PublicKey, Long] = createBonds
  ): GenesisParameters =
    buildGenesisParameters(defaultValidatorKeyPairs, bondsFunction(defaultValidatorPks))

  def buildGenesisParameters(
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)],
      bonds: Map[PublicKey, Long]
  ): GenesisParameters =
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
        genesisPk = defaultGenesisPk,
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

  type GenesisParameters = (Iterable[(PrivateKey, PublicKey)], Genesis)

  private val genesisCache: mutable.HashMap[GenesisParameters, GenesisContext] =
    mutable.HashMap.empty

  private var cacheAccesses = 0
  private var cacheMisses   = 0

  def buildGenesis(parameters: GenesisParameters): GenesisContext =
    genesisCache.synchronized {
      cacheAccesses += 1
      genesisCache.getOrElseUpdate(parameters, doBuildGenesis(parameters))
    }

  private def doBuildGenesis(
      parameters: GenesisParameters
  ): GenesisContext = {
    cacheMisses += 1
    println(
      f"""Genesis block cache miss, building a new genesis.
         |Cache misses: $cacheMisses / $cacheAccesses (${cacheMisses.toDouble / cacheAccesses}%1.2f) cache accesses.
       """.stripMargin
    )

    val (validavalidatorKeyPairs, genesisParameters) = parameters
    val storageDirectory                             = Files.createTempDirectory(s"hash-set-casper-test-genesis-")
    val storageSize: Long                            = 3024L * 1024 * 10
    implicit val log: Log.NOPLog[Task]               = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task]           = new metrics.Metrics.MetricsNOP[Task]

    (for {
      rspaceDir      <- Task.delay(Files.createDirectory(storageDirectory.resolve("rspace")))
      activeRuntime  <- Runtime.createWithEmptyCost[Task, Task.Par](rspaceDir, storageSize)
      runtimeManager <- RuntimeManager.fromRuntime[Task](activeRuntime)
      genesis        <- Genesis.createGenesisBlock(runtimeManager, genesisParameters)
      _              <- activeRuntime.close()

      blockStoreDir <- Task.delay(Files.createDirectory(storageDirectory.resolve("block-store")))
      blockStore    <- BlockDagStorageTestFixture.createBlockStorage[Task](blockStoreDir)
      _             <- blockStore.put(genesis.blockHash, genesis)

      blockDagDir <- Task.delay(Files.createDirectory(storageDirectory.resolve("block-dag-store")))
      blockDagStorage <- BlockDagFileStorage.create[Task](
                          makeBlockDagFileStorageConfig(blockDagDir)
                        )
      _ <- blockDagStorage.insert(genesis, genesis, invalid = false)
    } yield GenesisContext(genesis, validavalidatorKeyPairs, storageDirectory)).unsafeRunSync
  }

  case class GenesisContext(
      genesisBlock: BlockMessage,
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)],
      storageDirectory: Path
  ) {
    def validatorSks: Iterable[PrivateKey] = validatorKeyPairs.map(_._1)
    def validatorPks: Iterable[PublicKey]  = validatorKeyPairs.map(_._2)
  }
}
