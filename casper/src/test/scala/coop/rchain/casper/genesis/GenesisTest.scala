package coop.rchain.casper.genesis

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.casper.helper.BlockDagStorageFixture
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.casper.util.rholang.{InterpreterUtil, Resources, RuntimeManager}
import coop.rchain.casper.util.{BondsParser, ProtoUtil, VaultParser}
import coop.rchain.casper.{CasperShardConf, CasperSnapshot, OnChainCasperState}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.models.syntax._
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared.{Base16, Time}
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
    withGenResources[Task] {
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
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        val nonExistingPath = storageLocation.resolve("not/a/real/file").toString
        for {
          genesisAttempt <- fromInputFiles(maybeBondsPath = Some(nonExistingPath))(
                             runtimeManager,
                             genesisPath,
                             log,
                             time
                           ).attempt
        } yield log.warns.exists(_.contains("BONDS FILE NOT FOUND"))
    }
  )

  it should "fail with error when bonds file cannot be parsed" in taskTest(
    withGenResources[Task] {
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
          "FAILED PARSING BONDS FILE"
        )
    }
  )

  it should "create a genesis block with the right bonds when a proper bonds file is given" in taskTest(
    withGenResources[Task] {
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
          _     = log.infos.length should be(3)
          result = validators
            .map {
              case (v, i) => Bond(v.unsafeToByteString, i.toLong)
            }
        } yield result.forall(bonds.contains(_)) should be(true)
    }
  )

  it should "create a valid genesis block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      withGenResources[Task] {
        (
            runtimeManager: RuntimeManager[Task],
            genesisPath: Path,
            log: LogStub[Task],
            time: LogicalTime[Task]
        ) =>
          implicit val logEff = log
          for {
            genesis <- fromInputFiles()(runtimeManager, genesisPath, log, time)
            _       <- blockDagStorage.insert(genesis, false, approved = true)
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

  it should "detect an existing bonds file in the default location" in taskTest(
    withGenResources[Task] {
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
              case (v, i) => Bond(v.unsafeToByteString, i.toLong)
            }
        } yield result.forall(bonds.contains(_)) should be(true)
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
      deployTimestamp: Option[Long] = Some(System.currentTimeMillis()),
      blockNumber: Long = 0
  )(
      implicit runtimeManager: RuntimeManager[Task],
      genesisPath: Path,
      log: LogStub[Task],
      time: LogicalTime[Task]
  ): Task[BlockMessage] =
    for {
      timestamp <- deployTimestamp.fold(Time[Task].currentMillis)(x => x.pure[Task])
      vaults <- VaultParser.parse[Task](
                 maybeVaultsPath.getOrElse(genesisPath + "/wallets.txt")
               )
      bonds <- BondsParser.parse[Task](
                maybeBondsPath.getOrElse(genesisPath + "/bonds.txt"),
                autogenShardSize
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
                         supply = Long.MaxValue,
                         blockNumber = blockNumber
                       )
                     )
    } yield genesisBlock

  def withGenResources[F[_]: Concurrent: ContextShift: Parallel](
      body: (RuntimeManager[F], Path, LogStub[F], LogicalTime[F]) => F[Unit]
  ): F[Unit] = {
    val storePath                        = storageLocation
    val gp                               = genesisPath
    implicit val noopMetrics: Metrics[F] = new metrics.Metrics.MetricsNOP[F]
    implicit val span: Span[F]           = NoopSpan[F]()
    val time                             = new LogicalTime[F]
    implicit val log                     = new LogStub[F]

    for {
      kvsManager     <- Resources.mkTestRNodeStoreManager[F](storePath)
      store          <- kvsManager.rSpaceStores
      runtimeManager <- RuntimeManager[F](store)
      result         <- body(runtimeManager, genesisPath, log, time)
      _              <- Sync[F].delay { storePath.recursivelyDelete() }
      _              <- Sync[F].delay { gp.recursivelyDelete() }
    } yield result
  }

  def taskTest[R](f: Task[R]): R =
    f.runSyncUnsafe()
}
