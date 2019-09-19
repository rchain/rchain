package coop.rchain.casper.api

import cats.effect.{Resource, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockDagStorageFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Cell
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.PrivateKey
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import monix.eval.Task
import org.scalatest._
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap
import coop.rchain.metrics.NoopSpan

class BlockQueryResponseAPITest
    extends FlatSpec
    with Matchers
    with Inside
    with BlockDagStorageFixture {
  implicit val timeEff = new LogicalTime[Task]
  implicit val spanEff = NoopSpan[Task]()
  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("block-query-response-api-test")

  private val (sk, _)  = ConstructDeploy.defaultKeyPair
  val secondBlockQuery = "1234"
  val badTestHashQuery = "No such a hash"

  val genesisHashString = "0000000000000000000000000000000000000000000000000000000000000000"
  val version           = 1L

  val senderString: String =
    "3456789101112131415161718192345678910111213141516171819261718192113456789101112131415161718192345678910111213141516171819261718192"
  val sender: ByteString = ProtoUtil.stringToByteString(senderString)
  val bondsValidator     = Bond(sender, 1)

  def genesisBlock(genesisHashString: String, version: Long): BlockMessage = {
    val genesisHash = ProtoUtil.stringToByteString(genesisHashString)
    val blockNumber = 0L
    val timestamp   = 1527191663L
    val ps          = Dummies.createRChainState(blockNumber = blockNumber, bonds = List(bondsValidator))
    val body        = Dummies.createBody(state = ps)
    val header      = ProtoUtil.blockHeader(body, Seq.empty[ByteString], version, timestamp)

    Dummies.createBlockMessage(blockHash = genesisHash, header = header, body = body)
  }
  val genesisBlock: BlockMessage = genesisBlock(genesisHashString, version)

  val secondHashString     = "1234567891011121314151617181921234567891011121314151617181928192"
  val blockHash: BlockHash = ProtoUtil.stringToByteString(secondHashString)
  val blockNumber          = 1L
  val timestamp            = 1527191665L
  val ps: RChainState =
    Dummies.createRChainState(blockNumber = blockNumber, bonds = List(bondsValidator))
  val deployCount = 10
  val randomDeploys =
    (0 until deployCount).toList
      .traverse(i => ConstructDeploy.basicProcessedDeploy[Task](i))
      .unsafeRunSync(scheduler)
  val body: Body                       = Dummies.createBody(state = ps, deploys = randomDeploys)
  val parentsString                    = List(genesisHashString, "0000000001")
  val parentsHashList: List[BlockHash] = parentsString.map(ProtoUtil.stringToByteString)
  val header: Header                   = ProtoUtil.blockHeader(body, parentsHashList, version, timestamp)
  val shardId: String                  = "abcdefgh"
  val secondBlock: BlockMessage =
    Dummies.createBlockMessage(
      blockHash = blockHash,
      header = header,
      body = body,
      justifications = List(Justification(bondsValidator.validator, genesisBlock.blockHash)),
      sender = sender,
      shardId = shardId
    )

  val faultTolerance = 1f

  val bondValidatorHashList: List[String] = List(bondsValidator).map(PrettyPrinter.buildString)

  val deployCostList: List[String] = randomDeploys.map(PrettyPrinter.buildString)
  // TODO: Test tsCheckpoint:
  // we should be able to stub in a tuplespace dump but there is currently no way to do that.
  "getBlock" should "return successful block info response" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                  <- effectsForSimpleCasperSetup(blockStore, blockDagStorage)
        spanEff                                  = NoopSpan[Task]()
        (logEff, engineCell, cliqueOracleEffect) = effects
        q                                        = BlockQuery(hash = secondBlockQuery)
        blockQueryResponse <- BlockAPI.getBlock[Task](q)(
                               Sync[Task],
                               engineCell,
                               logEff,
                               cliqueOracleEffect,
                               blockStore,
                               spanEff
                             )
        _ = inside(blockQueryResponse) {
          case Right(BlockQueryResponse(Some(blockInfo))) =>
            blockInfo.blockHash should be(secondHashString)
            blockInfo.blockSize should be(secondBlock.toProto.serializedSize.toString)
            blockInfo.blockNumber should be(blockNumber)
            blockInfo.version should be(version)
            blockInfo.deployCount should be(deployCount)
            blockInfo.faultTolerance should be(faultTolerance)
            blockInfo.mainParentHash should be(genesisHashString)
            blockInfo.parentsHashList should be(parentsString)
            blockInfo.sender should be(senderString)
            blockInfo.shardId should be(shardId)
            blockInfo.bondsValidatorList should be(bondValidatorHashList)
            blockInfo.deployCost should be(deployCostList)
        }
      } yield ()
  }

  it should "return error when no block exists" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                  <- emptyEffects(blockStore, blockDagStorage)
        spanEff                                  = NoopSpan[Task]()
        (logEff, engineCell, cliqueOracleEffect) = effects
        q                                        = BlockQuery(hash = badTestHashQuery)
        blockQueryResponse <- BlockAPI.getBlock[Task](q)(
                               Sync[Task],
                               engineCell,
                               logEff,
                               cliqueOracleEffect,
                               blockStore,
                               spanEff
                             )
        _ = inside(blockQueryResponse) {
          case Left(msg) =>
            msg should be(
              s"Error: Failure to find block with hash $badTestHashQuery"
            )
        }
      } yield ()
  }

  "findDeploy" should "return successful block info response when a block contains the deploy with given signature" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                 <- effectsForSimpleCasperSetup(blockStore, blockDagStorage)
        (logEff, casperRef, cliqueOracleEffect) = effects
        deployId = SignDeployment
          .sign(PrivateKey(sk.bytes), randomDeploys.head.deploy)
          .sig
        blockQueryResponse <- BlockAPI.findDeploy[Task](deployId)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               cliqueOracleEffect,
                               blockStore
                             )
        _ = inside(blockQueryResponse) {
          case Right(LightBlockQueryResponse(Some(blockInfo))) =>
            blockInfo.blockHash should be(secondHashString)
            blockInfo.blockSize should be(secondBlock.toProto.serializedSize.toString)
            blockInfo.blockNumber should be(blockNumber)
            blockInfo.version should be(version)
            blockInfo.deployCount should be(deployCount)
            blockInfo.faultTolerance should be(faultTolerance)
            blockInfo.mainParentHash should be(genesisHashString)
            blockInfo.parentsHashList should be(parentsString)
            blockInfo.sender should be(senderString)
        }
      } yield ()
  }

  it should "return an error when no block contains the deploy with the given signature" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        effects                                 <- emptyEffects(blockStore, blockDagStorage)
        (logEff, casperRef, cliqueOracleEffect) = effects
        deployId                                = ByteString.copyFromUtf8("asdfQwertyUiopxyzcbv")
        blockQueryResponse <- BlockAPI.findDeploy[Task](deployId)(
                               Sync[Task],
                               casperRef,
                               logEff,
                               cliqueOracleEffect,
                               blockStore
                             )
        _ = inside(blockQueryResponse) {
          case Left(msg) =>
            msg should be(
              s"Couldn't find block containing deploy with id: ${PrettyPrinter.buildStringNoLimit(deployId)}"
            )
        }
      } yield ()
  }

  private def effectsForSimpleCasperSetup(
      blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[(LogStub[Task], EngineCell[Task], SafetyOracle[Task])] =
    runtimeManagerResource.use { implicit runtimeManager =>
      for {
        _ <- blockDagStorage.insert(genesisBlock, genesisBlock, false)
        _ <- blockDagStorage.insert(secondBlock, genesisBlock, false)
        casperEffect <- NoOpsCasperEffect[Task](
                         HashMap[BlockHash, BlockMessage](
                           (ProtoUtil.stringToByteString(genesisHashString), genesisBlock),
                           (ProtoUtil.stringToByteString(secondHashString), secondBlock)
                         )
                       )(Sync[Task], blockStore, blockDagStorage, runtimeManager)
        logEff     = new LogStub[Task]()
        metricsEff = new Metrics.MetricsNOP[Task]
        engine     = new EngineWithCasper[Task](casperEffect)
        engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
        cliqueOracleEffect = SafetyOracle
          .cliqueOracle[Task](Sync[Task], logEff, metricsEff, spanEff)
      } yield (logEff, engineCell, cliqueOracleEffect)
    }

  private def emptyEffects(
      blockStore: BlockStore[Task],
      blockDagStorage: BlockDagStorage[Task]
  ): Task[(LogStub[Task], EngineCell[Task], SafetyOracle[Task])] =
    runtimeManagerResource.use { implicit runtimeManager =>
      for {
        casperEffect <- NoOpsCasperEffect(
                         HashMap[BlockHash, BlockMessage](
                           (ProtoUtil.stringToByteString(genesisHashString), genesisBlock),
                           (ProtoUtil.stringToByteString(secondHashString), secondBlock)
                         )
                       )(Sync[Task], blockStore, blockDagStorage, runtimeManager)
        logEff     = new LogStub[Task]()
        metricsEff = new Metrics.MetricsNOP[Task]
        engine     = new EngineWithCasper[Task](casperEffect)
        engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
        cliqueOracleEffect = SafetyOracle
          .cliqueOracle[Task](Sync[Task], logEff, metricsEff, spanEff)
      } yield (logEff, engineCell, cliqueOracleEffect)
    }
}
