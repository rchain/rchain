package coop.rchain.casper.batch2

import coop.rchain.casper.Estimator
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{
  BlockDagStorageFixture,
  BlockGenerator,
  UnlimitedParentsEstimatorFixture
}
import coop.rchain.casper.protocol.Bond
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class EstimatorTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture
    with UnlimitedParentsEstimatorFixture {

  "Estimator on empty latestMessages" should "return the genesis regardless of DAG" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)
      for {
        genesis <- createGenesis[Task](bonds = bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b7.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
             )
        dag <- blockDagStorage.getRepresentation
        forkchoice <- Estimator[Task].tips(
                       dag,
                       genesis,
                       Map.empty[Validator, BlockHash]
                     )
      } yield forkchoice.tips.head should be(genesis.blockHash)
  }

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Estimator on Simple DAG" should "return the appropriate score map and forkchoice" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)
      for {
        genesis <- createGenesis[Task](bonds = bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b7.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
             )
        dag <- blockDagStorage.getRepresentation
        latestBlocks = HashMap[Validator, BlockHash](
          v1 -> b8.blockHash,
          v2 -> b6.blockHash
        )
        forkchoice <- Estimator[Task].tips(
                       dag,
                       genesis,
                       latestBlocks
                     )
        _      = forkchoice.tips.head should be(b6.blockHash)
        result = forkchoice.tips(1) should be(b8.blockHash)
      } yield result
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Estimator on flipping forkchoice DAG" should "return the appropriate score map and forkchoice" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v3     = generateValidator("Validator Three")
      val v1Bond = Bond(v1, 25)
      val v2Bond = Bond(v2, 20)
      val v3Bond = Bond(v3, 15)
      val bonds  = Seq(v1Bond, v2Bond, v3Bond)
      for {
        genesis <- createGenesis[Task](bonds = bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v3,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b3.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b5.blockHash),
               genesis,
               v3,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b6.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )
        dag <- blockDagStorage.getRepresentation
        latestBlocks = HashMap[Validator, BlockHash](
          v1 -> b6.blockHash,
          v2 -> b8.blockHash,
          v3 -> b7.blockHash
        )
        forkchoice <- Estimator[Task].tips(
                       dag,
                       genesis,
                       latestBlocks
                     )
        _      = forkchoice.tips.head should be(b8.blockHash)
        result = forkchoice.tips(1) should be(b7.blockHash)
      } yield result
  }

  /**
    * 5    4  3
    * |    | /
    * |    7
    * |   /
    * LCA
    * Main parent should be tip with weight 4
    * */
  "Estimator" should "return valid main parent" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v3     = generateValidator("Validator Three")
      val v1Bond = Bond(v1, 5)
      val v2Bond = Bond(v2, 4)
      val v3Bond = Bond(v3, 3)
      val bonds  = Seq(v1Bond, v2Bond, v3Bond)
      for {
        genesis <- createGenesis[Task](bonds = bonds)
        r7 <- createBlock[Task](
               parentsHashList = Seq(genesis.blockHash),
               genesis = genesis,
               creator = v2,
               bonds = bonds,
               justifications =
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        r4 <- createBlock[Task](
               parentsHashList = Seq(r7.blockHash),
               genesis = genesis,
               creator = v2,
               bonds = bonds,
               justifications =
                 HashMap(v1 -> genesis.blockHash, v2 -> r7.blockHash, v3 -> genesis.blockHash)
             )
        r3 <- createBlock[Task](
               parentsHashList = Seq(r7.blockHash),
               genesis = genesis,
               creator = v3,
               bonds = bonds,
               justifications =
                 HashMap(v1 -> genesis.blockHash, v2 -> r7.blockHash, v3 -> genesis.blockHash)
             )
        r5 <- createBlock[Task](
               parentsHashList = Seq(genesis.blockHash),
               genesis = genesis,
               creator = v1,
               bonds = bonds,
               justifications =
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )

        dag <- blockDagStorage.getRepresentation
        latestBlocks = HashMap[Validator, BlockHash](
          v1 -> r5.blockHash,
          v2 -> r4.blockHash,
          v3 -> r3.blockHash
        )
        forkchoice <- Estimator[Task].tips(
                       dag,
                       genesis,
                       latestBlocks
                     )
        _ = forkchoice.lca should be(genesis.blockHash)
        _ = forkchoice.tips.head should be(r4.blockHash)
        _ = forkchoice.tips.size should be(latestBlocks.size)
      } yield ()
  }
}
