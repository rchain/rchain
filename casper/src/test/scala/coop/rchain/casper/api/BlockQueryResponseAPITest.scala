package coop.rchain.casper.api

import cats._
import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.blockstorage.{BlockDagFileStorage, BlockDagStorage, BlockMetadata, BlockStore}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockDagStorageFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.Capture._
import monix.eval.Task

import scala.collection.immutable.HashMap

class BlockQueryResponseAPITest extends FlatSpec with Matchers with BlockDagStorageFixture {
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
    val body   = Body().withState(ps)
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
    (0 until deployCount).toList
      .traverse(ProtoUtil.basicProcessedDeploy[Task])
      .unsafeRunSync(scheduler)
  val body: Body                       = Body().withState(ps).withDeploys(randomDeploys)
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
  "showBlock" should "return successful block info response" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                <- effectsForSimpleCasperSetup(blockStore, blockDagStorage)
        (logEff, casperRef, turanOracleEffect) = effects
        q                                      = BlockQuery(hash = secondBlockQuery)
        blockQueryResponse <- BlockAPI.showBlock[Task](q)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               turanOracleEffect,
                               blockStore
                             )
        blockInfo = blockQueryResponse.blockInfo.get
        _         = blockQueryResponse.status should be("Success")
        _         = blockInfo.blockHash should be(secondHashString)
        _         = blockInfo.blockSize should be(secondBlock.serializedSize.toString)
        _         = blockInfo.blockNumber should be(blockNumber)
        _         = blockInfo.version should be(version)
        _         = blockInfo.deployCount should be(deployCount)
        _         = blockInfo.faultTolerance should be(faultTolerance)
        _         = blockInfo.mainParentHash should be(genesisHashString)
        _         = blockInfo.parentsHashList should be(parentsString)
        _         = blockInfo.sender should be(secondBlockSenderString)
        result    = blockInfo.shardId should be(shardId)
      } yield result
  }

  it should "return error when no block exists" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                <- emptyEffects(blockStore, blockDagStorage)
        (logEff, casperRef, turanOracleEffect) = effects
        q                                      = BlockQuery(hash = badTestHashQuery)
        blockQueryResponse <- BlockAPI.showBlock[Task](q)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               turanOracleEffect,
                               blockStore
                             )
      } yield
        blockQueryResponse.status should be(
          s"Error: Failure to find block with hash ${badTestHashQuery}"
        )
  }

  "findBlockWithDeploy" should "return successful block info response" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                <- effectsForSimpleCasperSetup(blockStore, blockDagStorage)
        (logEff, casperRef, turanOracleEffect) = effects
        user                                   = ByteString.EMPTY
        timestamp                              = 1L
        blockQueryResponse <- BlockAPI.findBlockWithDeploy[Task](user, timestamp)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               turanOracleEffect,
                               blockStore
                             )
        blockInfo = blockQueryResponse.blockInfo.get
        _         = blockQueryResponse.status should be("Success")
        _         = blockInfo.blockHash should be(secondHashString)
        _         = blockInfo.blockSize should be(secondBlock.serializedSize.toString)
        _         = blockInfo.blockNumber should be(blockNumber)
        _         = blockInfo.version should be(version)
        _         = blockInfo.deployCount should be(deployCount)
        _         = blockInfo.faultTolerance should be(faultTolerance)
        _         = blockInfo.mainParentHash should be(genesisHashString)
        _         = blockInfo.parentsHashList should be(parentsString)
        _         = blockInfo.sender should be(secondBlockSenderString)
        result    = blockInfo.shardId should be(shardId)
      } yield result
  }

  it should "return error when no block matching query exists" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                <- emptyEffects(blockStore, blockDagStorage)
        (logEff, casperRef, turanOracleEffect) = effects
        user                                   = ByteString.EMPTY
        timestamp                              = 0L
        blockQueryResponse <- BlockAPI.findBlockWithDeploy[Task](user, timestamp)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               turanOracleEffect,
                               blockStore
                             )
      } yield
        blockQueryResponse.status should be(
          s"Error: Failure to find block containing deploy signed by  with timestamp ${timestamp.toString}"
        )
  }

  private def effectsForSimpleCasperSetup(
      blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[(LogStub[Task], MultiParentCasperRef[Task], SafetyOracle[Task])] =
    for {
      _ <- blockDagStorage.insert(genesisBlock)
      _ <- blockDagStorage.insert(secondBlock)
      casperEffect <- NoOpsCasperEffect[Task](
                       HashMap[BlockHash, BlockMessage](
                         (ProtoUtil.stringToByteString(genesisHashString), genesisBlock),
                         (ProtoUtil.stringToByteString(secondHashString), secondBlock)
                       )
                     )(Sync[Task], blockStore, blockDagStorage)
      logEff            = new LogStub[Task]()
      casperRef         <- MultiParentCasperRef.of[Task]
      _                 <- casperRef.set(casperEffect)
      turanOracleEffect = SafetyOracle.turanOracle[Task]
    } yield (logEff, casperRef, turanOracleEffect)

  private def emptyEffects(
      blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[(LogStub[Task], MultiParentCasperRef[Task], SafetyOracle[Task])] =
    for {
      casperEffect <- NoOpsCasperEffect(
                       HashMap[BlockHash, BlockMessage](
                         (ProtoUtil.stringToByteString(genesisHashString), genesisBlock),
                         (ProtoUtil.stringToByteString(secondHashString), secondBlock)
                       )
                     )(Sync[Task], blockStore, blockDagStorage)
      logEff            = new LogStub[Task]()
      casperRef         <- MultiParentCasperRef.of[Task]
      _                 <- casperRef.set(casperEffect)
      turanOracleEffect = SafetyOracle.turanOracle[Task]
    } yield (logEff, casperRef, turanOracleEffect)
}
