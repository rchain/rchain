package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.internals._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.implicits._
import cats.mtl.implicits._

import coop.rchain.catscontrib.TaskContrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap

class CliqueOracleTest extends FlatSpec with Matchers with BlockGenerator {
  type StateWithChain[A] = State[Chain, A]

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Turan Oracle" should "detect finality as appropriate" in {
    val v1     = ByteString.copyFromUtf8("Validator One")
    val v2     = ByteString.copyFromUtf8("Validator Two")
    val v1Bond = Bond(v1, 2)
    val v2Bond = Bond(v2, 3)
    val bonds  = Seq(v1Bond, v2Bond)
    val createChain =
      for {
        genesis <- createBlock[StateWithChain](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[StateWithChain](Seq(genesis.blockHash),
                                          v2,
                                          bonds,
                                          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash))
        b3 <- createBlock[StateWithChain](Seq(genesis.blockHash),
                                          v1,
                                          bonds,
                                          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash))
        b4 <- createBlock[StateWithChain](Seq(b2.blockHash),
                                          v2,
                                          bonds,
                                          HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash))
        b5 <- createBlock[StateWithChain](Seq(b2.blockHash),
                                          v1,
                                          bonds,
                                          HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash))
        _ <- createBlock[StateWithChain](Seq(b4.blockHash),
                                         v2,
                                         bonds,
                                         HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash))
        b7 <- createBlock[StateWithChain](Seq(b4.blockHash),
                                          v1,
                                          bonds,
                                          HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash))
        b8 <- createBlock[StateWithChain](Seq(b7.blockHash),
                                          v1,
                                          bonds,
                                          HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash))
      } yield b8

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val chain: Chain = createChain.runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)
    val b6      = chain.idToBlocks(6)
    val b8      = chain.idToBlocks(8)

    val latestBlocks: collection.Map[ByteString, BlockMessage] =
      HashMap[ByteString, BlockMessage](v1 -> b8, v2 -> b6)

    implicit def turanOracleEffect: SafetyOracle[Task] =
      new TuranOracle(chain.hashToBlocks, latestBlocks)

    def runSafetyOracle[F[_]: Monad: SafetyOracle]: F[Unit] =
      for {
        isGenesisSafe          <- SafetyOracle[F].isSafe(genesis, 1)
        _                      = assert(isGenesisSafe)
        isB2Safe               <- SafetyOracle[F].isSafe(b2, 1)
        _                      = assert(isB2Safe)
        isB3Safe               <- SafetyOracle[F].isSafe(b3, 1)
        _                      = assert(!isB3Safe)
        isB4SafeConservatively <- SafetyOracle[F].isSafe(b4, 1)
        _                      = assert(!isB4SafeConservatively)
        isB4SafeLessConservatively <- SafetyOracle[F]
                                       .isSafe(b4, 0.2f) // Clique oracle would be safe
        _ = assert(!isB4SafeLessConservatively)
      } yield ()
    runSafetyOracle[Task].unsafeRunSync
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Turan Oracle" should "detect possible disagreements appropriately" in {
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

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val chain: Chain = createChain.runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)
    val b6      = chain.idToBlocks(6)
    val b7      = chain.idToBlocks(7)
    val b8      = chain.idToBlocks(8)

    val latestBlocks: collection.Map[ByteString, BlockMessage] =
      HashMap[ByteString, BlockMessage](v1 -> b6, v2 -> b8, v3 -> b7)

    implicit def turanOracleEffect: SafetyOracle[Task] =
      new TuranOracle(chain.hashToBlocks, latestBlocks)

    def runSafetyOracle[F[_]: Monad: SafetyOracle]: F[Unit] =
      for {
        isGenesisSafe <- SafetyOracle[F].isSafe(genesis, 0)
        _             = assert(isGenesisSafe)
        isB2Safe      <- SafetyOracle[F].isSafe(b2, 0)
        _             = assert(!isB2Safe)
        isB3Safe      <- SafetyOracle[F].isSafe(b3, 0)
        _             = assert(!isB3Safe)
        isB4Safe      <- SafetyOracle[F].isSafe(b4, 0)
        _             = assert(!isB4Safe)
      } yield ()
    runSafetyOracle[Task].unsafeRunSync
  }
}
