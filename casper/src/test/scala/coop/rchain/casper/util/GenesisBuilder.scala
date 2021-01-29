package coop.rchain.casper.util

import cats.syntax.all._
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ConstructDeploy.{defaultPub, defaultPub2}
import coop.rchain.casper.util.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task

import java.nio.file.{Files, Path}
import scala.collection.mutable

object GenesisBuilder {

  def createBonds(validators: Iterable[PublicKey]): Map[PublicKey, Long] =
    validators.zipWithIndex.map { case (v, i) => v -> (2L * i.toLong + 1L) }.toMap

  def createGenesis(): BlockMessage =
    buildGenesis().genesisBlock

  val defaultValidatorKeyPairs                   = (1 to 4).map(_ => Secp256k1.newKeyPair)
  val (defaultValidatorSks, defaultValidatorPks) = defaultValidatorKeyPairs.unzip

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
        shardId = "root",
        timestamp = 0L,
        proofOfStake = ProofOfStake(
          minimumBond = 0L,
          maximumBond = Long.MaxValue,
          // Epoch length is set to large number to prevent trigger of epoch change
          // in PoS close block method, which causes block merge conflicts
          // - epoch change can be set as a parameter in Rholang tests (e.g. PoSSpec)
          epochLength = 1000,
          quarantineLength = 50000,
          numberOfActiveValidators = 100,
          validators = bonds.map(Validator.tupled).toSeq
        ),
        vaults = Seq(defaultPub, defaultPub2).map(predefinedVault) ++
          bonds.toList.map {
            case (pk, _) =>
              // Initial validator vaults contain 0 Rev
              RevAddress.fromPublicKey(pk).map(Vault(_, 0))
          }.flattenOption,
        supply = Long.MaxValue
      )
    )

  private def predefinedVault(pub: PublicKey): Vault =
    Vault(RevAddress.fromPublicKey(pub).get, 9000000)

  type GenesisParameters = (Iterable[(PrivateKey, PublicKey)], Genesis)

  private val genesisCache: mutable.HashMap[GenesisParameters, GenesisContext] =
    mutable.HashMap.empty

  private var cacheAccesses = 0
  private var cacheMisses   = 0

  def buildGenesis(parameters: GenesisParameters = buildGenesisParameters()): GenesisContext =
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
    implicit val log: Log.NOPLog[Task]               = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task]           = new metrics.Metrics.MetricsNOP[Task]
    implicit val spanEff                             = NoopSpan[Task]()

    implicit val scheduler = monix.execution.Scheduler.Implicits.global

    val genesisContext = for {
      kvsManager          <- mkTestRNodeStoreManager[Task](storageDirectory)
      store               <- kvsManager.rSpaceStores
      spaces              <- Runtime.setupRSpace[Task](store)
      (rspace, replay, _) = spaces
      runtime             <- Runtime.createWithEmptyCost((rspace, replay))
      runtimeManager      <- RuntimeManager.fromRuntime[Task](runtime)
      genesis             <- Genesis.createGenesisBlock(runtimeManager, genesisParameters)
      blockStore          <- KeyValueBlockStore[Task](kvsManager)
      _                   <- blockStore.put(genesis.blockHash, genesis)
      blockDagStorage     <- BlockDagKeyValueStorage.create[Task](kvsManager)
      _                   <- blockDagStorage.insert(genesis, invalid = false)
    } yield GenesisContext(genesis, validavalidatorKeyPairs, storageDirectory)

    genesisContext.unsafeRunSync
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
