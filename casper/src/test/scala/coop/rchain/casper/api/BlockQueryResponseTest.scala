package coop.rchain.casper.api

import cats._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper._
import coop.rchain.casper.helper.BlockStoreFixture
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.p2p.EffectsTestInstances.LogStub
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.effect.implicits.syncId

class BlockQueryResponseTest extends FlatSpec with Matchers with BlockStoreFixture {
  val secondBlockQuery = "1234"
  val badTestHashQuery = "No such a hash"

  val genesisHashString = "00000000"
  val version           = 0L

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
  val randomDeploys: IndexedSeq[DeployCost] =
    (0 until deployCount).map(_ => DeployCost.defaultInstance)
  val body: Body                       = Body().withPostState(ps).withNewCode(randomDeploys)
  val parentsString                    = List(genesisHashString, "0000000001")
  val parentsHashList: List[BlockHash] = parentsString.map(ProtoUtil.stringToByteString)
  val header: Header                   = ProtoUtil.blockHeader(body, parentsHashList, version, timestamp)
  val secondBlockSenderString: String  = "3456789101112131415161718192"
  val secondBlockSender: ByteString    = ProtoUtil.stringToByteString(secondBlockSenderString)
  val secondBlock: BlockMessage =
    BlockMessage()
      .withBlockHash(blockHash)
      .withHeader(header)
      .withBody(body)
      .withSender(secondBlockSender)

  val faultTolerance = -1f

  def testCasper[F[_]: Monad: BlockStore]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[BlockStatus] = BlockStatus.valid.pure[F]
      def contains(b: BlockMessage): F[Boolean]     = false.pure[F]
      def deploy(r: Deploy): F[Unit]                = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def createBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag] =
        for {
          _ <- BlockStore[F].put(ProtoUtil.stringToByteString(genesisHashString), genesisBlock)
          _ <- BlockStore[F].put(ProtoUtil.stringToByteString(secondHashString), secondBlock)
        } yield BlockDag()
      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
      def lastFinalizedBlock: F[BlockMessage]                            = BlockMessage().pure[F]
      def storageContents(hash: BlockHash): F[String]                    = "".pure[F]
    }

  // TODO: Test tsCheckpoint:
  // we should be able to stub in a tuplespace dump but there is currently no way to do that.
  "getBlockQueryResponse" should "return successful block info response" in withStore {
    implicit blockStore =>
      implicit val casperEffect = testCasper[Id](syncId, blockStore)
      implicit val logEff       = new LogStub[Id]()(syncId)
      implicit val casperRef = {
        val tmp = MultiParentCasperRef.of[Id]
        tmp.set(casperEffect)
        tmp
      }
      implicit val turanOracleEffect: SafetyOracle[Id] =
        SafetyOracle.turanOracle[Id](syncId, blockStore)
      val q = BlockQuery(hash = secondBlockQuery)
      val blockQueryResponse = BlockAPI.getBlockQueryResponse[Id](q)(syncId,
                                                                     casperRef,
                                                                     logEff,
                                                                     turanOracleEffect,
                                                                     blockStore)
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
  }

  "getBlockQueryResponse" should "return error when no block exists" in withStore {
    implicit blockStore =>
      implicit val casperEffect = testCasper[Id](syncId, blockStore)
      implicit val logEff       = new LogStub[Id]()(syncId)
      implicit val casperRef = {
        val tmp = MultiParentCasperRef.of[Id]
        tmp.set(casperEffect)
        tmp
      }
      implicit val turanOracleEffect: SafetyOracle[Id] =
        SafetyOracle.turanOracle[Id](syncId, blockStore)
      val q = BlockQuery(hash = badTestHashQuery)
      val blockQueryResponse = BlockAPI.getBlockQueryResponse[Id](q)(syncId,
                                                                     casperRef,
                                                                     logEff,
                                                                     turanOracleEffect,
                                                                     blockStore)
      blockQueryResponse.status should be(
        s"Error: Failure to find block with hash ${badTestHashQuery}")
  }
}
