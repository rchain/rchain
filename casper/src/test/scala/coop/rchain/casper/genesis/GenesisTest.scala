package coop.rchain.casper.genesis

import java.io.PrintWriter
import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.BlockDag
import coop.rchain.casper.helper.BlockStoreFixture
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.shared.PathOps.RichPath
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import java.nio.file.Path

import cats.effect.ContextShift
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.shared.Language
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

class GenesisTest extends FlatSpec with Matchers with BlockStoreFixture {
  import GenesisTest._

  implicit val metrics = new MetricsNOP[Task]

  val validators = Seq(
    "299670c52849f1aa82e8dfe5be872c16b600bf09cc8983e04b903411358f2de6",
    "6bf1b2753501d02d386789506a6d93681d2299c6edfd4455f596b97bc5725968"
  ).zipWithIndex

  val walletAddresses = Seq(
    "0x20356b6fae3a94db5f01bdd45347faFad3dd18ef",
    "0x041e1eec23d118f0c4ffc814d4f415ac3ef3dcff"
  ).zipWithIndex

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
    pw.println(
      walletAddresses
        .map {
          case (v, i) => s"$v,$i,0"
        }
        .mkString("\n")
    )
    pw.close()
  }

  "Genesis.fromInputFiles" should "generate random validators when no bonds file is given" in withGenResources {
    (
        runtimeManager: RuntimeManager[Task],
        genesisPath: Path,
        log: LogStub[Task],
        time: LogicalTime[Task]
    ) =>
      val _ = fromInputFiles()(runtimeManager, genesisPath, log, time)

      log.warns.find(_.contains("bonds")) should be(None)
      log.infos.count(_.contains("Created validator")) should be(numValidators)

  }

  it should "generate random validators, with a warning, when bonds file does not exist" in withGenResources {
    (
        runtimeManager: RuntimeManager[Task],
        genesisPath: Path,
        log: LogStub[Task],
        time: LogicalTime[Task]
    ) =>
      val _ = fromInputFiles(maybeBondsPath = Some("not/a/real/file"))(
        runtimeManager,
        genesisPath,
        log,
        time
      )

      log.warns.count(_.contains("does not exist. Falling back on generating random validators.")) should be(
        1
      )
      log.infos.count(_.contains("Created validator")) should be(numValidators)
  }

  it should "generate random validators, with a warning, when bonds file cannot be parsed" in withGenResources {
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

      val _ =
        fromInputFiles(maybeBondsPath = Some(badBondsFile))(runtimeManager, genesisPath, log, time)

      log.warns.count(_.contains("cannot be parsed. Falling back on generating random validators.")) should be(
        1
      )
      log.infos.count(_.contains("Created validator")) should be(numValidators)
  }

  it should "create a genesis block with the right bonds when a proper bonds file is given" in withGenResources {
    (
        runtimeManager: RuntimeManager[Task],
        genesisPath: Path,
        log: LogStub[Task],
        time: LogicalTime[Task]
    ) =>
      val bondsFile = genesisPath.resolve("givenBonds.txt").toString
      printBonds(bondsFile)

      val genesis =
        fromInputFiles(maybeBondsPath = Some(bondsFile))(runtimeManager, genesisPath, log, time)
      val bonds = ProtoUtil.bonds(genesis)

      log.infos.isEmpty should be(true)
      validators
        .map {
          case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i.toLong)
        }
        .forall(
          bonds.contains(_)
        ) should be(true)
  }

  it should "create a valid genesis block" in withStore { implicit store =>
    withGenResources {
      (
          runtimeManager: RuntimeManager[Task],
          genesisPath: Path,
          log: LogStub[Task],
          time: LogicalTime[Task]
      ) =>
        implicit val logEff = log
        val genesis         = fromInputFiles()(runtimeManager, genesisPath, log, time)
        BlockStore[Task].put(genesis.blockHash, genesis).runSyncUnsafe(10.seconds)
        val blockDag = BlockDag.empty

        val maybePostGenesisStateHash = InterpreterUtil
          .validateBlockCheckpoint[Task](
            genesis,
            blockDag,
            runtimeManager
          )
          .runSyncUnsafe(10.seconds)

        maybePostGenesisStateHash should matchPattern { case Right(Some(_)) => }
    }
  }

  it should "detect an existing bonds file in the default location" in withGenResources {
    (
        runtimeManager: RuntimeManager[Task],
        genesisPath: Path,
        log: LogStub[Task],
        time: LogicalTime[Task]
    ) =>
      val bondsFile = genesisPath.resolve("bonds.txt").toString
      printBonds(bondsFile)

      val genesis = fromInputFiles()(runtimeManager, genesisPath, log, time)
      val bonds   = ProtoUtil.bonds(genesis)

      log.infos.length should be(1)
      validators
        .map {
          case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i.toLong)
        }
        .forall(
          bonds.contains(_)
        ) should be(true)
  }

  it should "parse the wallets file and include it in the genesis state" in withRawGenResources {
    (runtime: Runtime[Task], genesisPath: Path, log: LogStub[Task], time: LogicalTime[Task]) =>
      val walletsFile = genesisPath.resolve("wallets.txt").toString
      printWallets(walletsFile)

      val runtimeManager  = RuntimeManager.fromRuntime[Task](runtime).runSyncUnsafe(10.seconds)
      val _               = fromInputFiles()(runtimeManager, genesisPath, log, time)
      val storageContents = StoragePrinter.prettyPrint(runtime.space.store)

      walletAddresses.forall(storageContents contains _._1) should be(true)
  }

}

object GenesisTest {
  val storageSize     = 1024L * 1024
  def storageLocation = Files.createTempDirectory(s"casper-genesis-test-runtime")
  def genesisPath     = Files.createTempDirectory(s"casper-genesis-test")
  val numValidators   = 5
  val rchainShardId   = "rchain"

  def fromInputFiles(
      maybeBondsPath: Option[String] = None,
      numValidators: Int = numValidators,
      maybeWalletsPath: Option[String] = None,
      minimumBond: Long = 1L,
      maximumBond: Long = Long.MaxValue,
      faucet: Boolean = false,
      shardId: String = rchainShardId,
      deployTimestamp: Option[Long] = Some(System.currentTimeMillis())
  )(
      implicit runtimeManager: RuntimeManager[Task],
      genesisPath: Path,
      log: LogStub[Task],
      time: LogicalTime[Task]
  ): BlockMessage =
    Genesis
      .fromInputFiles[Task](
        maybeBondsPath,
        numValidators,
        genesisPath,
        maybeWalletsPath,
        minimumBond,
        maximumBond,
        faucet,
        runtimeManager,
        shardId,
        deployTimestamp
      )
      .runSyncUnsafe(10.seconds)

  def withRawGenResources(
      body: (Runtime[Task], Path, LogStub[Task], LogicalTime[Task]) => Unit
  ): Unit = {
    val storePath = storageLocation
    val runtime   = Runtime.create[Task, Task.Par](storePath, storageSize).runSyncUnsafe(10.seconds)
    val gp        = genesisPath
    val log       = new LogStub[Task]
    val time      = new LogicalTime[Task]

    body(runtime, genesisPath, log, time)

    runtime.close().runSyncUnsafe(10.seconds)
    Language.ignore(storePath.recursivelyDelete())
    Language.ignore(gp.recursivelyDelete())
  }

  def withGenResources(
      body: (RuntimeManager[Task], Path, LogStub[Task], LogicalTime[Task]) => Unit
  ): Unit =
    withRawGenResources {
      (runtime: Runtime[Task], genesisPath: Path, log: LogStub[Task], time: LogicalTime[Task]) =>
        val runtimeManager = RuntimeManager.fromRuntime[Task](runtime) runSyncUnsafe (1.second)
        body(runtimeManager, genesisPath, log, time)
    }
}
