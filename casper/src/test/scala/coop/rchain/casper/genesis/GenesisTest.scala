package coop.rchain.casper.genesis

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Registry, Validator}
import coop.rchain.casper.helper.BlockDagStorageFixture
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.{BlockRandomSeed, InterpreterUtil, Resources, RuntimeManager}
import coop.rchain.casper.util.{BondsParser, GenesisBuilder, VaultParser}
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rspace.syntax._
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.PrintWriter
import java.nio.file.{Files, Path}

class GenesisTest extends AnyFlatSpec with Matchers with EitherValues with BlockDagStorageFixture {
  import GenesisTest._

  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val span: Span[Task]          = NoopSpan[Task]()

  val validators = Seq(
    "299670c52849f1aa82e8dfe5be872c16b600bf09cc8983e04b903411358f2de6",
    "6bf1b2753501d02d386789506a6d93681d2299c6edfd4455f596b97bc5725968"
  ).zipWithIndex

  val walletAddresses = Seq(
    "0x20356b6fae3a94db5f01bdd45347faFad3dd18ef",
    "0x041e1eec23d118f0c4ffc814d4f415ac3ef3dcff"
  )

  def printBonds(bondsFile: String): Unit = {
    val pw = new PrintWriter(bondsFile)
    pw.println(
      validators
        .map {
          case (v, i) => s"$v $i"
        }
        .mkString("\n")
    )
    pw.close()
  }

  def printWallets(walletsFile: String): Unit = {
    val pw = new PrintWriter(walletsFile)
    val walletsContent =
      walletAddresses.zipWithIndex
        .map {
          case (v, i) => s"$v,$i,0"
        }
        .mkString("\n")
    pw.println(walletsContent)
    pw.close()
  }

  "Genesis.fromInputFiles" should "generate random validators when no bonds file is given" in taskTest(
    withGenResources[Task] {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task]
      ) =>
        for {
          _ <- fromInputFiles()(
                genesisPath,
                runtimeManager,
                implicitly[Concurrent[Task]],
                log
              )
          _ = log.warns.count(
            _.contains(
              "Creating file with random bonds"
            )
          ) should be(1)
        } yield log.infos.count(_.contains("Bond generated")) should be(autogenShardSize)
    }
  )

  it should "tell when bonds file does not exist" in taskTest(
    withGenResources[Task] {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task]
      ) =>
        val nonExistingPath = storageLocation.resolve("not/a/real/file").toString
        for {
          genesisAttempt <- fromInputFiles(maybeBondsPath = Some(nonExistingPath))(
                             genesisPath,
                             runtimeManager,
                             implicitly[Concurrent[Task]],
                             log
                           ).attempt
        } yield log.warns.exists(_.contains("BONDS FILE NOT FOUND"))
    }
  )

  it should "fail with error when bonds file cannot be parsed" in taskTest(
    withGenResources[Task] {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task]
      ) =>
        val badBondsFile = genesisPath.resolve("misformatted.txt").toString

        val pw = new PrintWriter(badBondsFile)
        pw.println("xzy 1\nabc 123 7")
        pw.close()

        for {
          genesisAttempt <- fromInputFiles(maybeBondsPath = Some(badBondsFile))(
                             genesisPath,
                             runtimeManager,
                             implicitly[Concurrent[Task]],
                             log
                           ).attempt
        } yield genesisAttempt.left.value.getMessage should include(
          "FAILED PARSING BONDS FILE"
        )
    }
  )

  it should "create a genesis block with the right bonds when a proper bonds file is given" in taskTest(
    withGenResources[Task] {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task]
      ) =>
        val bondsFile = genesisPath.resolve("givenBonds.txt").toString
        printBonds(bondsFile)

        for {
          genesis <- fromInputFiles(maybeBondsPath = Some(bondsFile))(
                      genesisPath,
                      runtimeManager,
                      implicitly[Concurrent[Task]],
                      log
                    )
          bonds = genesis.bonds.toList
          _     = log.infos.length should be(3)
          result = validators
            .map {
              case (v, i) => (v.unsafeHexToByteString, i.toLong)
            }
        } yield result.forall(bonds.contains) should be(true)
    }
  )

  it should "create a valid genesis block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      withGenResources[Task] {
        (
            runtimeManager: RuntimeManager[Task],
            genesisPath: Path,
            log: LogStub[Task]
        ) =>
          implicit val rm     = runtimeManager
          implicit val logEff = log
          for {
            genesis <- fromInputFiles()(
                        genesisPath,
                        runtimeManager,
                        implicitly[Concurrent[Task]],
                        log
                      )
            _         <- BlockStore[Task].put(genesis.blockHash, genesis)
            _         <- blockDagStorage.insertGenesis(genesis)
            postState <- InterpreterUtil.validateBlockCheckpointLegacy[Task](genesis)
          } yield postState.value shouldBe true
      }
  }

  it should "detect an existing bonds file in the default location" in taskTest(
    withGenResources[Task] {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task]
      ) =>
        val bondsFile = genesisPath.resolve("bonds.txt").toString
        printBonds(bondsFile)

        for {
          genesis <- fromInputFiles()(
                      genesisPath,
                      runtimeManager,
                      implicitly[Concurrent[Task]],
                      log
                    )
          bonds = genesis.bonds.toList
          _     = log.infos.length should be(3)
          result = validators
            .map {
              case (v, i) => (v.unsafeHexToByteString, i.toLong)
            }
        } yield result.forall(bonds.contains) should be(true)
    }
  )

  it should "parse the wallets file and create corresponding RevVault-s" ignore {}

}

object GenesisTest {
  def storageLocation  = Files.createTempDirectory(s"casper-genesis-test-runtime-")
  def genesisPath      = Files.createTempDirectory(s"casper-genesis-test-")
  val autogenShardSize = 5
  val rchainShardId    = "root"

  def fromInputFiles(
      maybeBondsPath: Option[String] = None,
      autogenShardSize: Int = autogenShardSize,
      maybeVaultsPath: Option[String] = None,
      minimumBond: Long = 1L,
      maximumBond: Long = Long.MaxValue,
      epochLength: Int = 10000,
      quarantineLength: Int = 50000,
      numberOfActiveValidators: Int = 100,
      shardId: String = rchainShardId,
      blockNumber: Long = 0
  )(
      implicit genesisPath: Path,
      runtimeManager: RuntimeManager[Task],
      c: Concurrent[Task],
      log: LogStub[Task]
  ): Task[BlockMessage] =
    for {
      vaults <- VaultParser.parse[Task](
                 maybeVaultsPath.getOrElse(genesisPath + "/wallets.txt")
               )
      bonds <- BondsParser.parse[Task](
                maybeBondsPath.getOrElse(genesisPath + "/bonds.txt"),
                autogenShardSize
              )
      // Initial set of validators
      validators = bonds.toSeq.map(Validator.tupled)
      validator  = ValidatorIdentity(Secp256k1.newKeyPair._1)
      genesisBlock <- createGenesisBlock(
                       validator,
                       Genesis(
                         sender = validator.publicKey,
                         shardId = shardId,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           epochLength = epochLength,
                           quarantineLength = quarantineLength,
                           numberOfActiveValidators = numberOfActiveValidators,
                           validators = validators,
                           posMultiSigPublicKeys = GenesisBuilder.defaultPosMultiSigPublicKeys,
                           posMultiSigQuorum = GenesisBuilder.defaultPosMultiSigPublicKeys.length - 1,
                           posVaultPubKey = GenesisBuilder.defaultPosVaultPubKeyHex
                         ),
                         registry = Registry(GenesisBuilder.defaultSystemContractPubKey),
                         vaults = vaults,
                         blockNumber = blockNumber
                       )
                     )
    } yield genesisBlock

  def withGenResources[F[_]: Concurrent: ContextShift: Parallel](
      body: (RuntimeManager[F], Path, LogStub[F]) => F[Unit]
  ): F[Unit] = {
    val storePath                        = storageLocation
    val gp                               = genesisPath
    implicit val noopMetrics: Metrics[F] = new metrics.Metrics.MetricsNOP[F]
    implicit val span: Span[F]           = NoopSpan[F]()
    implicit val log                     = new LogStub[F]

    for {
      kvsManager <- Resources.mkTestRNodeStoreManager[F](storePath)
      rStore     <- kvsManager.rSpaceStores
      mStore     <- RuntimeManager.mergeableStore(kvsManager)
      t          = RuntimeManager.noOpExecutionTracker
      runtimeManager <- RuntimeManager[F](
                         rStore,
                         mStore,
                         BlockRandomSeed.nonNegativeMergeableTagName(rchainShardId),
                         t
                       )
      result <- body(runtimeManager, genesisPath, log)
      _      <- Sync[F].delay { storePath.recursivelyDelete() }
      _      <- Sync[F].delay { gp.recursivelyDelete() }
    } yield result
  }

  def taskTest[R](f: Task[R]): R =
    f.runSyncUnsafe()
}
