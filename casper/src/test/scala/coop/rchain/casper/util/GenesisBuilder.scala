package coop.rchain.casper.util

import java.nio.file.{Files, Path}

import cats.implicits._
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.BlockDagStorageTestFixture
import coop.rchain.casper.protocol._
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.ConstructDeploy.{defaultPub, defaultPub2}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log
import monix.eval.Task

import scala.collection.mutable

object GenesisBuilder {

  def createBonds(validators: Iterable[PublicKey]): Map[PublicKey, Long] =
    validators.zipWithIndex.map { case (v, i) => v -> (2L * i.toLong + 1L) }.toMap

  def createGenesis(): BlockMessage =
    buildGenesis().genesisBlock

  val defaultValidatorKeyPairs: Seq[(PrivateKey, PublicKey)] =
    (1 to 4).map(_ => Secp256k1.newKeyPair)
  val (defaultValidatorSks, defaultValidatorPks) = defaultValidatorKeyPairs.unzip

  def buildGenesisParameters(
      bondsFunction: Iterable[PublicKey] => Map[PublicKey, Long] = createBonds,
      validatorsNum: Int = 4
  ): GenesisParameters = {
    val validatorKeyPairs = (1 to validatorsNum).map(_ => Secp256k1.newKeyPair)
    buildGenesisParameters(
      validatorKeyPairs,
      bondsFunction(validatorKeyPairs.map(_._2))
    )
  }

  def buildGenesisParameters(
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)],
      bonds: Map[PublicKey, Long]
  ): GenesisParameters = {
    val genesisVaults: Seq[(PrivateKey, PublicKey)] =
      (1 to validatorKeyPairs.size).map(_ => Secp256k1.newKeyPair)
    (
      validatorKeyPairs,
      genesisVaults,
      Genesis(
        shardId = "root",
        timestamp = 0L,
        proofOfStake = ProofOfStake(
          minimumBond = 0L,
          maximumBond = Long.MaxValue,
          // Epoch length is set to large number to prevent trigger of epoch change
          // in PoS close block method, which causes block merge conflicts
          // - epoch change can be set as a parameter in Rholang tests (e.g. PoSSpec)
          epochLength = 10000,
          quarantineLength = 50000,
          numberOfActiveValidators = 100,
          validators = bonds.map(Validator.tupled).toSeq
        ),
        vaults = genesisVaults.toList.map(pair => predefinedVault(pair._2)) ++
          bonds.toList.map {
            case (pk, _) =>
              // Initial validator vaults contain 0 Rev
              RevAddress.fromPublicKey(pk).map(Vault(_, 0))
          }.flattenOption,
        supply = Long.MaxValue
      )
    )
  }

  private def predefinedVault(pub: PublicKey): Vault =
    Vault(RevAddress.fromPublicKey(pub).get, 10000000000000L)

  type GenesisParameters =
    (Iterable[(PrivateKey, PublicKey)], Iterable[(PrivateKey, PublicKey)], Genesis)

  private val genesisCache: mutable.HashMap[GenesisParameters, GenesisContext] =
    mutable.HashMap.empty

  private var cacheAccesses = 0
  private var cacheMisses   = 0

  def buildGenesis(
      parameters: GenesisParameters = buildGenesisParameters(),
      validatorsNum: Int = 4
  ): GenesisContext =
    genesisCache.synchronized {
      cacheAccesses += 1
      genesisCache.getOrElseUpdate(
        parameters,
        doBuildGenesis(buildGenesisParameters(validatorsNum = validatorsNum))
      )
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

    val (validavalidatorKeyPairs, genesisVaults, genesisParameters) = parameters
    val storageDirectory                                            = Files.createTempDirectory(s"hash-set-casper-test-genesis-")
    val storageSize: Long                                           = 1024L * 1024 * 1024
    implicit val log: Log.NOPLog[Task]                              = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task]                          = new metrics.Metrics.MetricsNOP[Task]
    implicit val spanEff                                            = NoopSpan[Task]()

    implicit val scheduler = monix.execution.Scheduler.Implicits.global

    (for {
      rspaceDir      <- Task.delay(Files.createDirectory(storageDirectory.resolve("rspace")))
      r              <- RhoRuntime.setupRSpace[Task](rspaceDir, storageSize)
      historyRepo    = r._3
      runtimes       <- RhoRuntime.createRuntimes[Task](rspaceDir, storageSize)
      runtimeManager <- RuntimeManager.fromRuntimes[Task](runtimes._1, runtimes._2, historyRepo)
      genesis        <- Genesis.createGenesisBlock(runtimeManager, genesisParameters)
      _              <- runtimes._1.close
      _              <- runtimes._2.close
      blockStoreDir  <- Task.delay(Files.createDirectory(storageDirectory.resolve("block-store")))
      blockStore     <- BlockDagStorageTestFixture.createBlockStorage[Task](blockStoreDir)
      _              <- blockStore.put(genesis.blockHash, genesis)

      blockDagDir <- Task.delay(Files.createDirectory(storageDirectory.resolve("block-dag-store")))

      storeManager <- RNodeKeyValueStoreManager[Task](blockDagDir)
      blockDagStorage <- {
        implicit val kvm = storeManager
        BlockDagKeyValueStorage.create[Task]
      }
      _ <- blockDagStorage.insert(genesis, invalid = false)
    } yield GenesisContext(genesis, validavalidatorKeyPairs, genesisVaults, storageDirectory)).unsafeRunSync
  }

  case class GenesisContext(
      genesisBlock: BlockMessage,
      validatorKeyPairs: Iterable[(PrivateKey, PublicKey)],
      genesisVaults: Iterable[(PrivateKey, PublicKey)],
      storageDirectory: Path
  ) {
    def validatorSks: Iterable[PrivateKey] = validatorKeyPairs.map(_._1)
    def validatorPks: Iterable[PublicKey]  = validatorKeyPairs.map(_._2)

    def genesisVaultsSks: Iterable[PrivateKey] = genesisVaults.map(_._1)
    def genesisVailtsPks: Iterable[PublicKey]  = genesisVaults.map(_._2)
  }
}
