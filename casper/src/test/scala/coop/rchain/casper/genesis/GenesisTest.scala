package coop.rchain.casper.genesis

import java.io.PrintWriter
import java.nio.file.Files

import cats.Id
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.BlockDag
import coop.rchain.casper.helper.BlockStoreFixture
import coop.rchain.casper.protocol.Bond
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class GenesisTest extends FlatSpec with Matchers with BeforeAndAfterEach with BlockStoreFixture {
  val storageSize     = 1024L * 1024
  def storageLocation = Files.createTempDirectory(s"casper-genesis-test-runtime")
  def genesisPath     = Files.createTempDirectory(s"casper-genesis-test")
  val numValidators   = 5
  val rchainShardId   = "rchain"
  implicit val log    = new LogStub[Id]
  implicit val time   = new LogicalTime[Id]

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

  override def beforeEach(): Unit =
    log.reset()

  "Genesis.fromInputFiles" should "generate random validators when no bonds file is given" in {
    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val _ = Genesis.fromInputFiles[Id](
      None,
      numValidators,
      genesisPath,
      None,
      runtimeManager,
      rchainShardId,
      Some(System.currentTimeMillis())
    )
    runtime.close()

    log.warns.find(_.contains("bonds")) should be(None)
    log.infos.count(_.contains("Created validator")) should be(numValidators)
  }

  it should "generate random validators, with a warning, when bonds file does not exist" in {
    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val _ = Genesis.fromInputFiles[Id](
      Some("not/a/real/file"),
      numValidators,
      genesisPath,
      None,
      runtimeManager,
      rchainShardId,
      Some(System.currentTimeMillis())
    )
    runtime.close()

    log.warns.count(_.contains("does not exist. Falling back on generating random validators.")) should be(
      1
    )
    log.infos.count(_.contains("Created validator")) should be(numValidators)
  }

  it should "generate random validators, with a warning, when bonds file cannot be parsed" in {
    val path         = genesisPath
    val badBondsFile = path.resolve("misformatted.txt").toString

    val pw = new PrintWriter(badBondsFile)
    pw.println("xzy 1\nabc 123 7")
    pw.close()

    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val _ =
      Genesis.fromInputFiles[Id](
        Some(badBondsFile),
        numValidators,
        path,
        None,
        runtimeManager,
        rchainShardId,
        Some(System.currentTimeMillis())
      )
    runtime.close()

    log.warns.count(_.contains("cannot be parsed. Falling back on generating random validators.")) should be(
      1
    )
    log.infos.count(_.contains("Created validator")) should be(numValidators)
  }

  it should "create a genesis block with the right bonds when a proper bonds file is given" in {
    val path      = genesisPath
    val bondsFile = path.resolve("givenBonds.txt").toString
    printBonds(bondsFile)

    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val genesis =
      Genesis.fromInputFiles[Id](
        Some(bondsFile),
        numValidators,
        path,
        None,
        runtimeManager,
        rchainShardId,
        Some(System.currentTimeMillis())
      )
    runtime.close()
    val bonds = ProtoUtil.bonds(genesis)

    log.infos.isEmpty should be(true)
    validators
      .map {
        case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i)
      }
      .forall(
        bonds.contains(_)
      ) should be(true)
  }

  it should "create a valid genesis block" in withStore { implicit store =>
    val activeRuntime  = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
    val emptyStateHash = runtimeManager.emptyStateHash

    val genesis = Genesis.fromInputFiles[Id](
      None,
      numValidators,
      genesisPath,
      None,
      runtimeManager,
      rchainShardId,
      Some(System.currentTimeMillis())
    )
    BlockStore[Id].put(genesis.blockHash, genesis)
    val blockDag = BlockDag.empty

    val (maybePostGenesisStateHash, _) = InterpreterUtil
      .validateBlockCheckpoint[Id](
        genesis,
        blockDag,
        Set[ByteString](emptyStateHash),
        runtimeManager
      )
    activeRuntime.close()

    maybePostGenesisStateHash should matchPattern { case Right(Some(_)) => }
  }

  it should "detect an existing bonds file in the default location" in {
    val path      = genesisPath
    val bondsFile = path.resolve("bonds.txt").toString
    printBonds(bondsFile)

    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val genesis = Genesis
      .fromInputFiles[Id](None, numValidators, path, None, runtimeManager, rchainShardId, None)
    runtime.close()
    val bonds = ProtoUtil.bonds(genesis)

    log.infos.length should be(1)
    validators
      .map {
        case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i)
      }
      .forall(
        bonds.contains(_)
      ) should be(true)
  }

  it should "parse the wallets file and include it in the genesis state" in {
    val path        = genesisPath
    val walletsFile = path.resolve("wallets.txt").toString
    printWallets(walletsFile)

    val runtime        = Runtime.create(storageLocation, storageSize)
    val runtimeManager = RuntimeManager.fromRuntime(runtime)
    val _ = Genesis
      .fromInputFiles[Id](None, numValidators, path, None, runtimeManager, rchainShardId, None)
    val storageContents = StoragePrinter.prettyPrint(runtime.space.store)
    runtime.close()

    walletAddresses.forall(storageContents contains _._1) should be(true)
  }

}
