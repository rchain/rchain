package coop.rchain.casper.api

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.NoopSpan
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
  implicit val timeEff: LogicalTime[Task] = new LogicalTime[Task]
  implicit val spanEff: NoopSpan[Task]    = NoopSpan[Task]()
  implicit val log: Log[Task]             = Log.log[Task]

  private val tooShortQuery    = "12345"
  private val badTestHashQuery = "1234acd"
  private val invalidHexQuery  = "No such a hash"

  private val genesisBlock: BlockMessage = getRandomBlock(setJustifications = Seq().some)

  private val deployCount = 10
  private val randomDeploys =
    (0 until deployCount).toList
      .traverse(i => ConstructDeploy.basicProcessedDeploy[Task](i))
      .runSyncUnsafe()

  private val senderString: String =
    "3456789101112131415161718192345678910111213141516171819261718192113456789101112131415161718192345678910111213141516171819261718192"
  private val sender: ByteString = senderString.unsafeHexToByteString
  private val bondsValidator     = (sender, 1L)

  private val secondBlock: BlockMessage =
    getRandomBlock(
      setValidator = sender.some,
      setDeploys = randomDeploys.some,
      setJustifications = List(genesisBlock.blockHash).some,
      setBonds = Map(bondsValidator).some
    )

  "getBlock" should "return successful block info response" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

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
          b.bonds should be(secondBlock.bonds.map(BlockApi.bondToBondInfo))
          b.blockSize should be(secondBlock.toProto.serializedSize.toString)
          b.deployCount should be(secondBlock.state.deploys.length)
          b.justifications should be(secondBlock.justifications.map(_.toHexString))
      }
    } yield ()
  }

  it should "return error when no block exists" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

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

  it should "return error when hash is invalid hex string" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

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

  it should "return error when hash is to short" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

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

  "findDeploy" should "return successful block info response when a block contains the deploy with given signature" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()
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
          blockInfo.bonds should be(secondBlock.bonds.map(BlockApi.bondToBondInfo))
          blockInfo.blockSize should be(secondBlock.toProto.serializedSize.toString)
          blockInfo.deployCount should be(secondBlock.state.deploys.length)
          blockInfo.justifications should be(secondBlock.justifications.map(_.toHexString))
      }
    } yield ()
  }

  it should "return an error when no block contains the deploy with the given signature" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

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

  private def prepareDagStorage[F[_]: Sync: BlockDagStorage: BlockStore]: F[Unit] = {
    import coop.rchain.blockstorage.syntax._
    for {
      _ <- List(genesisBlock, secondBlock).traverse(BlockStore[F].put(_))
      _ <- BlockDagStorage[F].insertLegacy(genesisBlock, invalid = false, approved = true)
      _ <- BlockDagStorage[F].insertLegacy(secondBlock, invalid = false)
    } yield ()
  }
}
