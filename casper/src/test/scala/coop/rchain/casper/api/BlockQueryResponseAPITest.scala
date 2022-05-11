package coop.rchain.casper.api

import cats.effect.{Resource, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockQueryResponseAPITest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with BlockDagStorageFixture
    with BlockApiFixture {
  implicit val timeEff = new LogicalTime[Task]
  implicit val spanEff = NoopSpan[Task]()
  implicit val log     = Log.log[Task]

  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager[Task]("block-query-response-api-test")

  implicit val metricsEff           = new Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  val tooShortQuery    = "12345"
  val badTestHashQuery = "1234acd"
  val invalidHexQuery  = "No such a hash"

  val genesisBlock: BlockMessage = getRandomBlock(setJustifications = Seq().some)

  val deployCount = 10
  val randomDeploys =
    (0 until deployCount).toList
      .traverse(i => ConstructDeploy.basicProcessedDeploy[Task](i))
      .unsafeRunSync(scheduler)

  val senderString: String =
    "3456789101112131415161718192345678910111213141516171819261718192113456789101112131415161718192345678910111213141516171819261718192"
  val sender: ByteString = senderString.unsafeHexToByteString
  val bondsValidator     = (sender, 1L)

  val secondBlock: BlockMessage =
    getRandomBlock(
      setValidator = sender.some,
      setDeploys = randomDeploys.some,
      setJustifications = List(genesisBlock.blockHash).some,
      setBonds = Map(bondsValidator).some
    )

  val faultTolerance = -1f

  val deployCostList: List[String] = randomDeploys.map(PrettyPrinter.buildString)

  // TODO: Test tsCheckpoint:
  // we should be able to stub in a tuplespace dump but there is currently no way to do that.
  "getBlock" should "return successful block info response" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          _                  <- prepareDagStorage[Task]
          blockApi           <- createBlockApi[Task]("", 1)
          hash               = secondBlock.blockHash.toHexString
          blockQueryResponse <- blockApi.getBlock(hash)
          _ = inside(blockQueryResponse) {
            case Right(blockInfo) =>
              blockInfo.deploys should be(
                randomDeploys.map(_.toDeployInfo)
              )
              val b = blockInfo.blockInfo
              b.blockHash should be(secondBlock.blockHash.toHexString)
              b.sender should be(secondBlock.sender.toHexString)
              b.blockSize should be(secondBlock.toProto.serializedSize.toString)
              b.seqNum should be(secondBlock.toProto.seqNum)
              b.sig should be(secondBlock.sig.toHexString)
              b.sigAlgorithm should be(secondBlock.sigAlgorithm)
              b.shardId should be(secondBlock.toProto.shardId)
              b.version should be(secondBlock.version)
              b.blockNumber should be(secondBlock.blockNumber)
              b.preStateHash should be(
                secondBlock.preStateHash.toHexString
              )
              b.postStateHash should be(
                secondBlock.postStateHash.toHexString
              )
              b.bonds should be(secondBlock.bonds.map(ProtoUtil.bondToBondInfo))
              b.blockSize should be(secondBlock.toProto.serializedSize.toString)
              b.deployCount should be(secondBlock.state.deploys.length)
              b.faultTolerance should be(faultTolerance)
              b.justifications should be(secondBlock.justifications.map(_.toHexString))
          }
        } yield ()
      }
  }

  it should "return error when no block exists" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          blockApi           <- createBlockApi[Task]("", 1)
          hash               = badTestHashQuery
          blockQueryResponse <- blockApi.getBlock(hash)
          _ = inside(blockQueryResponse) {
            case Left(msg) =>
              msg should be(
                s"Error: Failure to find block with hash: $badTestHashQuery"
              )
          }
        } yield ()
      }
  }

  it should "return error when hash is invalid hex string" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          blockApi           <- createBlockApi[Task]("", 1)
          hash               = invalidHexQuery
          blockQueryResponse <- blockApi.getBlock(hash)
          _ = inside(blockQueryResponse) {
            case Left(msg) =>
              msg should be(
                s"Input hash value is not valid hex string: $invalidHexQuery"
              )
          }
        } yield ()
      }
  }

  it should "return error when hash is to short" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          blockApi           <- createBlockApi[Task]("", 1)
          hash               = tooShortQuery
          blockQueryResponse <- blockApi.getBlock(hash)
          _ = inside(blockQueryResponse) {
            case Left(msg) =>
              msg should be(
                s"Input hash value must be at least 6 characters: $tooShortQuery"
              )
          }
        } yield ()
      }
  }

  "findDeploy" should "return successful block info response when a block contains the deploy with given signature" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          _                  <- prepareDagStorage[Task]
          blockApi           <- createBlockApi[Task]("", 1)
          deployId           = randomDeploys.head.deploy.sig
          blockQueryResponse <- blockApi.findDeploy(deployId)
          _ = inside(blockQueryResponse) {
            case Right(blockInfo) =>
              blockInfo.blockHash should be(secondBlock.toProto.blockHash.toHexString)
              blockInfo.sender should be(secondBlock.toProto.sender.toHexString)
              blockInfo.blockSize should be(secondBlock.toProto.serializedSize.toString)
              blockInfo.seqNum should be(secondBlock.toProto.seqNum)
              blockInfo.sig should be(secondBlock.sig.toHexString)
              blockInfo.sigAlgorithm should be(secondBlock.sigAlgorithm)
              blockInfo.shardId should be(secondBlock.toProto.shardId)
              blockInfo.version should be(secondBlock.version)
              blockInfo.blockNumber should be(secondBlock.blockNumber)
              blockInfo.preStateHash should be(
                secondBlock.preStateHash.toHexString
              )
              blockInfo.postStateHash should be(
                secondBlock.postStateHash.toHexString
              )
              blockInfo.bonds should be(secondBlock.bonds.map(ProtoUtil.bondToBondInfo))
              blockInfo.blockSize should be(secondBlock.toProto.serializedSize.toString)
              blockInfo.deployCount should be(secondBlock.state.deploys.length)
              blockInfo.faultTolerance should be(faultTolerance)
              blockInfo.justifications should be(secondBlock.justifications.map(_.toHexString))
          }
        } yield ()
      }
  }

  it should "return an error when no block contains the deploy with the given signature" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          blockApi           <- createBlockApi[Task]("", 1)
          deployId           = ByteString.copyFromUtf8("asdfQwertyUiopxyzcbv")
          blockQueryResponse <- blockApi.findDeploy(deployId)
          _ = inside(blockQueryResponse) {
            case Left(msg) =>
              msg should be(
                s"Couldn't find block containing deploy with id: ${PrettyPrinter.buildStringNoLimit(deployId)}"
              )
          }
        } yield ()
      }
  }

  private def prepareDagStorage[F[_]: Sync: BlockDagStorage: BlockStore]: F[Unit] = {
    import coop.rchain.blockstorage.syntax._
    for {
      _ <- List(genesisBlock, secondBlock).traverse(BlockStore[F].put(_))
      _ <- BlockDagStorage[F].insert(genesisBlock, false, approved = true)
      _ <- BlockDagStorage[F].insert(secondBlock, false)
    } yield ()
  }
}
