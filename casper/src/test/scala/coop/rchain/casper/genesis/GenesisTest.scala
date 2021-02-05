package coop.rchain.casper.genesis

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.helper.BlockDagStorageFixture
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.casper.util.rholang.{InterpreterUtil, Resources, RuntimeManager}
import coop.rchain.casper.util.{BondsParser, ProtoUtil, VaultParser}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.PathOps.RichPath
import org.scalatest.{BeforeAndAfterEach, EitherValues, FlatSpec, Matchers}
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.{CasperShardConf, CasperSnapshot, OnChainCasperState}
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{EitherValues, FlatSpec, Matchers}

import java.io.PrintWriter
import java.nio.file.{Files, Path}

class GenesisTest extends FlatSpec with Matchers with EitherValues with BlockDagStorageFixture {
  import GenesisTest._

  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val span: Span[Task]          = NoopSpan[Task]()

  def mkCasperSnapshot[F[_]](dag: BlockDagRepresentation[F]) =
    CasperSnapshot(
      dag,
      ByteString.EMPTY,
      ByteString.EMPTY,
      IndexedSeq.empty,
      List.empty,
      Set.empty,
      Map.empty,
      Set.empty,
      0,
      Map.empty,
      OnChainCasperState(
        CasperShardConf(0, "", "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Map.empty,
        Seq.empty
      )
    )

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
    withGenResources {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        for {
          _ <- fromInputFiles()(runtimeManager, genesisPath, log, time)
          _ = log.warns.count(
            _.contains(
              "Bonds file was not specified and default bonds file does not exist. Falling back on generating random validators."
            )
          ) should be(1)
        } yield log.infos.count(_.contains("Created validator")) should be(autogenShardSize)
    }
  )

  it should "fail with error when bonds file does not exist" in taskTest(
    withGenResources {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        for {
          genesisAttempt <- fromInputFiles(maybeBondsPath = Some("not/a/real/file"))(
                             runtimeManager,
                             genesisPath,
                             log,
                             time
                           ).attempt
        } yield genesisAttempt.left.value.getMessage should be(
          "Specified bonds file not/a/real/file does not exist"
        )
    }
  )

  it should "fail with error when bonds file cannot be parsed" in taskTest(
    withGenResources {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        val badBondsFile = genesisPath.resolve("misformatted.txt").toString

        val pw = new PrintWriter(badBondsFile)
        pw.println("xzy 1\nabc 123 7")
        pw.close()

        for {
          genesisAttempt <- fromInputFiles(maybeBondsPath = Some(badBondsFile))(
                             runtimeManager,
                             genesisPath,
                             log,
                             time
                           ).attempt
        } yield genesisAttempt.left.value.getMessage should include(
          "misformatted.txt cannot be parsed"
        )
    }
  )

  it should "create a genesis block with the right bonds when a proper bonds file is given" in taskTest(
    withGenResources {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        val bondsFile = genesisPath.resolve("givenBonds.txt").toString
        printBonds(bondsFile)

        for {
          genesis <- fromInputFiles(maybeBondsPath = Some(bondsFile))(
                      runtimeManager,
                      genesisPath,
                      log,
                      time
                    )
          bonds = ProtoUtil.bonds(genesis)
          _     = log.infos.length should be(2)
          result = validators
            .map {
              case (v, i) => Bond(ByteString.copyFrom(Base16.unsafeDecode(v)), i.toLong)
            }
        } yield result.forall(bonds.contains(_)) should be(true)
    }
  )

  it should "create a valid genesis block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      withGenResources {
        (
            runtimeManager: RuntimeManager[Task],
            genesisPath: Path,
            log: LogStub[Task],
            time: LogicalTime[Task]
        ) =>
          implicit val logEff = log
          for {
            genesis <- fromInputFiles()(runtimeManager, genesisPath, log, time)
            _       <- BlockStore[Task].put(genesis.blockHash, genesis)
            dag     <- blockDagStorage.getRepresentation
            maybePostGenesisStateHash <- InterpreterUtil
                                          .validateBlockCheckpoint[Task](
                                            genesis,
                                            mkCasperSnapshot(dag),
                                            runtimeManager
                                          )
          } yield maybePostGenesisStateHash should matchPattern { case Right(Some(_)) => }
      }
  }

  it should "detect an existing bonds file in the default location" in taskTest(withGenResources {
    (
        runtimeManager: RuntimeManager[Task],
        genesisPath: Path,
        log: LogStub[Task],
        time: LogicalTime[Task]
    ) =>
      val bondsFile = genesisPath.resolve("bonds.txt").toString
      printBonds(bondsFile)

      for {
        genesis <- fromInputFiles()(runtimeManager, genesisPath, log, time)
        bonds   = ProtoUtil.bonds(genesis)
        _       = log.infos.length should be(3)
        result = validators
          .map {
            case (v, i) => Bond(ByteString.copyFrom(Base16.unsafeDecode(v)), i.toLong)
          }
      } yield result.forall(bonds.contains(_)) should be(true)
  })

  it should "parse the wallets file and create corresponding RevVault-s" ignore {}

}

object GenesisTest {
  def storageLocation  = Files.createTempDirectory(s"casper-genesis-test-runtime-")
  def genesisPath      = Files.createTempDirectory(s"casper-genesis-test-")
  val autogenShardSize = 5
  val rchainShardId    = "root"

  implicit val raiseIOError = IOError.raiseIOErrorThroughSync[Task]
  implicit val log          = new LogStub[Task]

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
      deployTimestamp: Option[Long] = Some(System.currentTimeMillis())
  )(
      implicit runtimeManager: RuntimeManager[Task],
      genesisPath: Path,
      log: LogStub[Task],
      time: LogicalTime[Task]
  ): Task[BlockMessage] =
    for {
      timestamp <- deployTimestamp.fold(Time[Task].currentMillis)(x => x.pure[Task])
      vaults    <- VaultParser.parse[Task](maybeVaultsPath, genesisPath.resolve("wallets.txt"))
      bonds <- BondsParser.parse[Task](
                maybeBondsPath,
                genesisPath.resolve("bonds.txt"),
                autogenShardSize,
                genesisPath
              )
      validators = bonds.toSeq.map(Validator.tupled)
      genesisBlock <- createGenesisBlock(
                       runtimeManager,
                       Genesis(
                         shardId = shardId,
                         timestamp = timestamp,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           epochLength = epochLength,
                           quarantineLength = quarantineLength,
                           numberOfActiveValidators = numberOfActiveValidators,
                           validators = validators
                         ),
                         vaults = vaults,
                         supply = Long.MaxValue
                       )
                     )
    } yield genesisBlock

  def withRawGenResources(
      body: (
          RhoHistoryRepository[Task],
          (RhoRuntime[Task], ReplayRhoRuntime[Task]),
          Path,
          LogicalTime[Task]
      ) => Task[Unit]
  ): Task[Unit] = {
    val storePath                           = storageLocation
    val gp                                  = genesisPath
    implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val span: Span[Task]           = NoopSpan[Task]()
    val time                                = new LogicalTime[Task]

    for {
      kvsManager                   <- Resources.mkTestRNodeStoreManager[Task](storePath)
      store                        <- kvsManager.rSpaceStores
      spaces                       <- RhoRuntime.createRuntimes[Task](store)
      (runtime, replayRuntime, hr) = spaces
      result                       <- body(hr, (runtime, replayRuntime), genesisPath, time)
      _                            <- Sync[Task].delay { storePath.recursivelyDelete() }
      _                            <- Sync[Task].delay { gp.recursivelyDelete() }
    } yield result
  }

  def withGenResources(
      body: (RuntimeManager[Task], Path, LogStub[Task], LogicalTime[Task]) => Task[Unit]
  )(implicit metrics: Metrics[Task], span: Span[Task]): Task[Unit] =
    withRawGenResources {
      implicit val log = new LogStub[Task]
      (
          historyRepo: RhoHistoryRepository[Task],
          runtimes: (RhoRuntime[Task], ReplayRhoRuntime[Task]),
          genesisPath: Path,
          time: LogicalTime[Task]
      ) =>
        RuntimeManager
          .fromRuntimes(runtimes._1, runtimes._2, historyRepo)
          .flatMap(body(_, genesisPath, log, time))
    }

  def taskTest[R](f: Task[R]): R =
    f.runSyncUnsafe()
}
