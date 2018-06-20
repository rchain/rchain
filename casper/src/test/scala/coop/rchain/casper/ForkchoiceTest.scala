package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.implicits._
import cats.mtl.implicits._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.catscontrib.TaskContrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.{HashMap, HashSet}

class ForkchoiceTest extends FlatSpec with Matchers with BlockGenerator {
  type StateWithChain[A] = State[BlockDag, A]
  val initState = BlockDag()

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Estimator on Simple DAG" should "return the appropriate score map and forkchoice" in {
    val v1     = ByteString.copyFromUtf8("Validator One")
    val v2     = ByteString.copyFromUtf8("Validator Two")
    val v1Bond = Bond(v1, 2)
    val v2Bond = Bond(v2, 3)
    val bonds  = Seq(v1Bond, v2Bond)
    def createChain[F[_]: Monad: BlockDagState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[F](Seq(genesis.blockHash),
                             v2,
                             bonds,
                             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash))
        b3 <- createBlock[F](Seq(genesis.blockHash),
                             v1,
                             bonds,
                             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash))
        b4 <- createBlock[F](Seq(b2.blockHash),
                             v2,
                             bonds,
                             HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash))
        b5 <- createBlock[F](Seq(b2.blockHash),
                             v1,
                             bonds,
                             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash))
        _ <- createBlock[F](Seq(b4.blockHash),
                            v2,
                            bonds,
                            HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash))
        b7 <- createBlock[F](Seq(b4.blockHash),
                             v1,
                             bonds,
                             HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash))
        b8 <- createBlock[F](Seq(b7.blockHash),
                             v1,
                             bonds,
                             HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash))
      } yield b8

    val chain: BlockDag = createChain[StateWithChain].runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b6      = chain.idToBlocks(6)
    val b8      = chain.idToBlocks(8)

    val latestBlocks = HashMap[Validator, BlockHash](v1 -> b8.blockHash, v2 -> b6.blockHash)

    val forkchoice = Estimator.tips(chain.copy(latestMessages = latestBlocks), genesis)
    forkchoice.head should be(b6)
    forkchoice(1) should be(b8)
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Estimator on flipping forkchoice DAG" should "return the appropriate score map and forkchoice" in {
    val v1     = ByteString.copyFromUtf8("Validator One")
    val v2     = ByteString.copyFromUtf8("Validator Two")
    val v3     = ByteString.copyFromUtf8("Validator Three")
    val v1Bond = Bond(v1, 25)
    val v2Bond = Bond(v2, 20)
    val v3Bond = Bond(v3, 15)
    val bonds  = Seq(v1Bond, v2Bond, v3Bond)
    def createChain[F[_]: Monad: BlockDagState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[F](
               Seq(genesis.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash))
        b3 <- createBlock[F](
               Seq(genesis.blockHash),
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash))
        b4 <- createBlock[F](
               Seq(b2.blockHash),
               v3,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash))
        b5 <- createBlock[F](
               Seq(b3.blockHash),
               v2,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash))
        b6 <- createBlock[F](Seq(b4.blockHash),
                             v1,
                             bonds,
                             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash))
        b7 <- createBlock[F](Seq(b5.blockHash),
                             v3,
                             bonds,
                             HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash))
        b8 <- createBlock[F](Seq(b6.blockHash),
                             v2,
                             bonds,
                             HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash))
      } yield b8

    val chain: BlockDag = createChain[StateWithChain].runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b6      = chain.idToBlocks(6)
    val b7      = chain.idToBlocks(7)
    val b8      = chain.idToBlocks(8)

    val latestBlocks =
      HashMap[Validator, BlockHash](v1 -> b6.blockHash, v2 -> b8.blockHash, v3 -> b7.blockHash)

    val forkchoice = Estimator.tips(chain.copy(latestMessages = latestBlocks), genesis)
    forkchoice.head should be(b8)
    forkchoice(1) should be(b7)
  }
}
