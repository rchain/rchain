package coop.rchain.casper.api

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockStoreFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.Capture._
import coop.rchain.metrics.Metrics.MetricsNOP
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._
import scala.collection.immutable.HashMap

class BlockQueryResponseAPITest extends FlatSpec with Matchers with BlockStoreFixture {
  implicit val metrics = new MetricsNOP[Task]
  implicit val timeEff = new LogicalTime[Task]

  val secondBlockQuery = "1234"
  val badTestHashQuery = "No such a hash"

  val genesisHashString = "00000000"
  val version           = 1L

  def genesisBlock(genesisHashString: String, version: Long): BlockMessage = {
    val genesisHash = ProtoUtil.stringToByteString(genesisHashString)
    val blockNumber = 0L
    val timestamp   = 1527191663L
    val ps = RChainState()
      .withBlockNumber(blockNumber)
      .withBonds(Seq(Bond(ByteString.copyFromUtf8("random"), 1)))
    val body   = Body().withPostState(ps)
    val header = ProtoUtil.blockHeader(body, Seq.empty[ByteString], version, timestamp)
    BlockMessage().withBlockHash(genesisHash).withHeader(header).withBody(body)
  }
  val genesisBlock: BlockMessage = genesisBlock(genesisHashString, version)

  val secondHashString     = "123456789101112131415161718192"
  val blockHash: BlockHash = ProtoUtil.stringToByteString(secondHashString)
  val blockNumber          = 1L
  val timestamp            = 1527191665L
  val ps: RChainState      = RChainState().withBlockNumber(blockNumber)
  val deployCount          = 10
  val randomDeploys =
    (0 until deployCount).map(ProtoUtil.basicProcessedDeploy[Task](_).runSyncUnsafe(1.second))
  val body: Body                       = Body().withPostState(ps).withDeploys(randomDeploys)
  val parentsString                    = List(genesisHashString, "0000000001")
  val parentsHashList: List[BlockHash] = parentsString.map(ProtoUtil.stringToByteString)
  val header: Header                   = ProtoUtil.blockHeader(body, parentsHashList, version, timestamp)
  val secondBlockSenderString: String  = "3456789101112131415161718192"
  val secondBlockSender: ByteString    = ProtoUtil.stringToByteString(secondBlockSenderString)
  val shardId: String                  = "abcdefgh"
  val secondBlock: BlockMessage =
    BlockMessage()
      .withBlockHash(blockHash)
      .withHeader(header)
      .withBody(body)
      .withSender(secondBlockSender)
      .withShardId(shardId)

  val faultTolerance = -1f

  // TODO: Test tsCheckpoint:
  // we should be able to stub in a tuplespace dump but there is currently no way to do that.
  "showBlock" should "return successful block info response" in withStore[Task, Unit] {
    implicit blockStore =>
      val (
        logEff: LogStub[Task],
        casperRef: MultiParentCasperRef[Task],
        turanOracleEffect: SafetyOracle[Task]
      )     = effectsForSimpleCasperSetup(blockStore)
      val q = BlockQuery(hash = secondBlockQuery)
      val blockQueryResponse = BlockAPI
        .showBlock[Task](q)(
          Sync[Task],
          casperRef,
          logEff,
          turanOracleEffect,
          blockStore
        )
        .runSyncUnsafe(1.second)
      val blockInfo = blockQueryResponse.blockInfo.get
      blockQueryResponse.status should be("Success")
      blockInfo.blockHash should be(secondHashString)
      blockInfo.blockSize should be(secondBlock.serializedSize.toString)
      blockInfo.blockNumber should be(blockNumber)
      blockInfo.version should be(version)
      blockInfo.deployCount should be(deployCount)
      blockInfo.faultTolerance should be(faultTolerance)
      blockInfo.mainParentHash should be(genesisHashString)
      blockInfo.parentsHashList should be(parentsString)
      blockInfo.sender should be(secondBlockSenderString)
      blockInfo.shardId should be(shardId)
  }

  it should "return error when no block exists" in withStore[Task, Unit] { implicit blockStore =>
    val (
      logEff: LogStub[Task],
      casperRef: MultiParentCasperRef[Task],
      turanOracleEffect: SafetyOracle[Task]
    )     = emptyEffects(blockStore)
    val q = BlockQuery(hash = badTestHashQuery)
    val blockQueryResponse = BlockAPI
      .showBlock[Task](q)(
        Sync[Task],
        casperRef,
        logEff,
        turanOracleEffect,
        blockStore
      )
      .runSyncUnsafe(1.second)
    blockQueryResponse.status should be(
      s"Error: Failure to find block with hash ${badTestHashQuery}"
    )
  }

  "findBlockWithDeploy" should "return successful block info response" in withStore[Task, Unit] {
    implicit blockStore =>
      val (
        logEff: LogStub[Task],
        casperRef: MultiParentCasperRef[Task],
        turanOracleEffect: SafetyOracle[Task]
      )             = effectsForSimpleCasperSetup(blockStore)
      val user      = ByteString.EMPTY
      val timestamp = 1L
      val blockQueryResponse = BlockAPI
        .findBlockWithDeploy[Task](user, timestamp)(
          Sync[Task],
          casperRef,
          logEff,
          turanOracleEffect,
          blockStore
        )
        .runSyncUnsafe(1.second)
      val blockInfo = blockQueryResponse.blockInfo.get
      blockQueryResponse.status should be("Success")
      blockInfo.blockHash should be(secondHashString)
      blockInfo.blockSize should be(secondBlock.serializedSize.toString)
      blockInfo.blockNumber should be(blockNumber)
      blockInfo.version should be(version)
      blockInfo.deployCount should be(deployCount)
      blockInfo.faultTolerance should be(faultTolerance)
      blockInfo.mainParentHash should be(genesisHashString)
      blockInfo.parentsHashList should be(parentsString)
      blockInfo.sender should be(secondBlockSenderString)
      blockInfo.shardId should be(shardId)
  }

  it should "return error when no block matching query exists" in withStore[Task, Unit] {
    implicit blockStore =>
      val (
        logEff: LogStub[Task],
        casperRef: MultiParentCasperRef[Task],
        turanOracleEffect: SafetyOracle[Task]
      )             = emptyEffects(blockStore)
      val user      = ByteString.EMPTY
      val timestamp = 0L
      val blockQueryResponse = BlockAPI
        .findBlockWithDeploy[Task](user, timestamp)(
          Sync[Task],
          casperRef,
          logEff,
          turanOracleEffect,
          blockStore
        )
        .runSyncUnsafe(1.second)
      blockQueryResponse.status should be(
        s"Error: Failure to find block containing deploy signed by  with timestamp ${timestamp.toString}"
      )
  }

  private def effectsForSimpleCasperSetup(
      blockStore: BlockStore[Task]
  ): (LogStub[Task], MultiParentCasperRef[Task], SafetyOracle[Task]) = {
    val genesisHash = ProtoUtil.stringToByteString(genesisHashString)
    val secondHash  = ProtoUtil.stringToByteString(secondHashString)
    val dag = BlockDag.empty
      .copy(
        dataLookup = Map(
          genesisHash -> BlockMetadata
            .fromBlock(genesisBlock),
          secondHash -> BlockMetadata.fromBlock(secondBlock)
        ),
        topoSort = Vector(Vector(genesisHash), Vector(secondHash))
      )
    implicit val casperEffect = NoOpsCasperEffect(
      HashMap[BlockHash, BlockMessage](
        (genesisHash, genesisBlock),
        (secondHash, secondBlock)
      ),
      Vector(BlockMessage()),
      dag
    )(Sync[Task], blockStore).runSyncUnsafe(1.second)
    implicit val logEff = new LogStub[Task]()
    implicit val casperRef = {
      val tmp = MultiParentCasperRef.of[Task].runSyncUnsafe(1.second)
      tmp.set(casperEffect)
      tmp
    }
    implicit val turanOracleEffect: SafetyOracle[Task] =
      SafetyOracle.turanOracle[Task]
    (logEff, casperRef, turanOracleEffect)
  }

  private def emptyEffects(
      blockStore: BlockStore[Task]
  ): (LogStub[Task], MultiParentCasperRef[Task], SafetyOracle[Task]) = {
    implicit val casperEffect = NoOpsCasperEffect(
      HashMap[BlockHash, BlockMessage](
        (ProtoUtil.stringToByteString(genesisHashString), genesisBlock),
        (ProtoUtil.stringToByteString(secondHashString), secondBlock)
      )
    )(Sync[Task], blockStore).runSyncUnsafe(1.second)
    implicit val logEff = new LogStub[Task]()(Sync[Task])
    implicit val casperRef = {
      val tmp = MultiParentCasperRef.of[Task].runSyncUnsafe(1.second)
      tmp.set(casperEffect)
      tmp
    }
    implicit val turanOracleEffect: SafetyOracle[Task] =
      SafetyOracle.turanOracle[Task]
    (logEff, casperRef, turanOracleEffect)
  }
}
