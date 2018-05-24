package coop.rchain.casper.api

import cats._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.{BlockDag, BlockGenerator, MultiParentCasper}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.Checkpoint
import coop.rchain.crypto.codec.Base16
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.{HashMap, HashSet}

class BlockAPITest extends FlatSpec with Matchers with BlockGenerator {
  val testHashQuery              = "1234"
  val badTestHashQuery           = "No such a hash"
  val testHash                   = ByteString.copyFrom(Base16.decode("12345678910111213141516171819"))
  val testHashEncoded            = Base16.encode(testHash.toByteArray)
  val testBlockNumber            = 47L
  val testParentsHashList        = Vector(ByteString.copyFrom(Base16.decode("testParent")))
  val testParentsHashListEncoded = testParentsHashList.map(b => Base16.encode(b.toByteArray))

  def testCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
      def contains(b: BlockMessage): F[Boolean] = false.pure[F]
      def deploy(r: Deploy): F[Unit]            = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def createBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag] = {
        val postState = RChainState().withBlockNumber(testBlockNumber)
        val body      = Body().withPostState(postState)
        val header    = Header().withParentsHashList(testParentsHashList)
        val testBlock = BlockMessage(blockHash = testHash, header = Some(header), body = Some(body))
        BlockDag(
          HashMap.empty[Int, BlockMessage],
          HashMap[BlockHash, BlockMessage](testHash -> testBlock),
          HashMap.empty[BlockHash, HashSet[BlockHash]],
          HashMap.empty[Validator, BlockHash],
          0
        ).pure[F]
      }
      def tsCheckpoint(hash: ByteString): F[Option[Checkpoint]] =
        Applicative[F].pure[Option[Checkpoint]](None)
    }
  implicit val casperEffect = testCasper[Id]

  // TODO: Test tsCheckpoint:
  // we should be able to stub in a tuplespace dump but there is currently no way to do that.
  "getBlockInfo" should "return successful block info response" in {
    val q                  = BlockQuery(hash = testHashQuery)
    val blockQueryResponse = BlockAPI.getBlockQueryResponse[Id](q)
    val blockInfo          = blockQueryResponse.blockInfo.get
    blockQueryResponse.status should be("Success")
    blockInfo.blockHash should be(testHashEncoded)
    blockInfo.blockNumber should be(testBlockNumber)
    blockInfo.parentsHashList should be(testParentsHashListEncoded)
  }

  "getBlockInfo" should "return error when no block exists" in {
    val q                  = BlockQuery(hash = badTestHashQuery)
    val blockQueryResponse = BlockAPI.getBlockQueryResponse[Id](q)
    blockQueryResponse.status should be(
      s"Error: Failure to find block with hash ${badTestHashQuery}")
  }
}
