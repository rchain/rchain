package coop.rchain.casper.api

import cats._
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper._
import coop.rchain.casper.helper.BlockGenerator
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.catscontrib.Catscontrib
import coop.rchain.p2p.EffectsTestInstances.LogStub
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.{HashMap, HashSet}

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseTest extends FlatSpec with Matchers with BlockGenerator {
  val initState = BlockDag()

  val v1     = ByteString.copyFromUtf8("Validator One")
  val v2     = ByteString.copyFromUtf8("Validator Two")
  val v3     = ByteString.copyFromUtf8("Validator Three")
  val v1Bond = Bond(v1, 25)
  val v2Bond = Bond(v2, 20)
  val v3Bond = Bond(v3, 15)
  val bonds  = Seq(v1Bond, v2Bond, v3Bond)
  val createChain =
    for {
      genesis <- createBlock[StateWithChain](Seq(), ByteString.EMPTY, bonds)
      b2 <- createBlock[StateWithChain](
             Seq(genesis.blockHash),
             v2,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash))
      b3 <- createBlock[StateWithChain](
             Seq(genesis.blockHash),
             v1,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash))
      b4 <- createBlock[StateWithChain](
             Seq(b2.blockHash),
             v3,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash))
      b5 <- createBlock[StateWithChain](
             Seq(b3.blockHash),
             v2,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash))
      b6 <- createBlock[StateWithChain](
             Seq(b4.blockHash),
             v1,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash))
      b7 <- createBlock[StateWithChain](
             Seq(b5.blockHash),
             v3,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash))
      b8 <- createBlock[StateWithChain](
             Seq(b6.blockHash),
             v2,
             bonds,
             HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash))
    } yield b8

  val chain: BlockDag = createChain.runS(initState)
  val genesis         = chain.idToBlocks(1)

  def testCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[Unit]                             = ().pure[F]
      def contains(b: BlockMessage): F[Boolean]                          = false.pure[F]
      def deploy(r: Deploy): F[Unit]                                     = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]]                         = Estimator.tips(chain, genesis).pure[F]
      def createBlock: F[Option[BlockMessage]]                           = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                                          = chain.pure[F]
      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
      def storageContents(hash: BlockHash): F[String]                    = "".pure[F]
    }
  implicit val casperEffect = testCasper[Id]
  implicit val logEff       = new LogStub[Id]
  implicit val constructorEffect =
    MultiParentCasperConstructor
      .successCasperConstructor[Id](ApprovedBlock.defaultInstance, casperEffect)
  implicit val turanOracleEffect: SafetyOracle[Id] = SafetyOracle.turanOracle[Id]

  "getBlocksResponse" should "return only blocks in the main chain" in {
    val blocksResponse = BlockAPI.getBlocksResponse[Id]
    val blocks         = blocksResponse.blocks
    blocksResponse.length should be(5)
    blocks.length should be(5)
  }
}
