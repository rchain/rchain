package coop.rchain.casper.api

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Async, IO}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagMessageState, DagRepresentation, Message}
import coop.rchain.casper.api.BlockApi.ApiErr
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.{BlockMessage, LightBlockInfo}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.GenesisBuilder.randomValidatorKeyPairs
import coop.rchain.casper.{PrettyPrinter, ValidatorIdentity}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class ExploratoryDeployAPITest
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {

  private val genesis = getRandomBlock()
  private val b1      = getRandomBlock()
  private val b2      = getRandomBlock()
  private val b3      = getRandomBlock()

  private def validator(index: Int) = keys(index)._2.bytes.toByteString

  private val keys     = randomValidatorKeyPairs.take(4).toList
  private val vGenesis = validator(0)
  private val v1       = validator(1)
  private val v2       = validator(2)
  private val v3       = validator(3)

  /*
   * DAG Looks like this:
   *           b3
   *           |
   *           b2
   *           |
   *           b1
   *           |
   *         genesis
   */
  it should "exploratoryDeploy get data from the read only node" in {
    implicit val log = mock[Log[IO]]
    implicit val sp  = mock[Span[IO]]

    implicit val bs = mock[BlockStore[IO]]
    bs.get(Seq(b2.blockHash)) returnsF Seq(b2.some)

    val blocks     = List(genesis, b1, b2, b3)
    val validators = List(vGenesis, v1, v2, v3)
    val bondsMap   = validators.map(_ -> 10L).toMap

    def toMessage(block: BlockMessage, sender: ByteString, senderSeq: Long) =
      Message[BlockHash, Validator](
        block.blockHash,
        blocks.indexOf(block).toLong,
        sender,
        senderSeq,
        bondsMap,
        blocks.get(blocks.indexOf(block) - 1L).map(b => Set(b.blockHash)).getOrElse(Set.empty),
        Set.empty,
        blocks.take(blocks.indexOf(block) + 1).map(_.blockHash).toSet
      )

    implicit val bds = mock[BlockDagStorage[IO]]
    bds.getRepresentation returnsF DagRepresentation(
      blocks
        .map(_.blockHash)
        .toSet,
      blocks.zipWithIndex.map {
        case (b, i) =>
          b.blockHash -> blocks
            .get(i + 1L)
            .map(nextBlock => Set(nextBlock.blockHash))
            .getOrElse(Set.empty)
      }.toMap,
      blocks.zipWithIndex.foldLeft(SortedMap.empty[Long, Set[BlockHash]]) {
        case (acc, (b, i)) => acc + (i.toLong -> Set(b.blockHash))
      },
      DagMessageState[BlockHash, Validator](
        Set(toMessage(b2, vGenesis, 2), toMessage(b3, v1, 0)),
        Map(
          genesis.blockHash -> toMessage(genesis, vGenesis, 0),
          b1.blockHash      -> toMessage(b1, vGenesis, 1),
          b2.blockHash      -> toMessage(b2, vGenesis, 2),
          b3.blockHash      -> toMessage(b3, v1, 0)
        )
      ),
      Map(
        Set.empty -> FringeData(
          Blake2b256Hash.create(Blake2b256.hash("".getBytes)),
          Set.empty,
          Set.empty,
          RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
          Set.empty,
          Set.empty,
          Set.empty
        )
      )
    )

    val term        = "new return in { for (@data <- @\"store\") {return!(data)}}"
    val storedData  = "data"
    implicit val rm = mock[RuntimeManager[IO]]
    rm.playExploratoryDeploy(term, *) returnsF List(Par(exprs = List(Expr(GString(storedData)))))

    for {
      result <- exploratoryDeploy[IO](term, b2.blockHash)
    } yield {
      result shouldBe 'right

      val (par, b) = result.value
      par match {
        case Seq(Par(_, _, _, Seq(expr), _, _, _, _, _, _)) =>
          expr match {
            case Expr(GString(data)) => data shouldBe storedData
            case _                   => fail("Could not get data from exploratory api")
          }
      }
      b.blockHash shouldBe PrettyPrinter.buildStringNoLimit(b2.blockHash)

      bs.get(Seq(b2.blockHash)) wasCalled once
      verifyNoMoreInteractions(bs)
      bds.getRepresentation wasCalled once
      rm.playExploratoryDeploy(term, *) wasCalled once
    }
  }

  it should "exploratoryDeploy return error on bonded validator" in {
    implicit val blockDagStorage = mock[BlockDagStorage[IO]]
    implicit val blockStore      = mock[BlockStore[IO]]
    implicit val runtimeManager  = mock[RuntimeManager[IO]]
    implicit val log             = mock[Log[IO]]
    implicit val sp              = mock[Span[IO]]

    for {
      result <- exploratoryDeploy[IO](
                 "new return in { return!(1) }",
                 ByteString.EMPTY,
                 ValidatorIdentity(keys.head._1).some
               )
    } yield {
      result shouldBe 'left
      result.left.value shouldBe "Exploratory deploy can only be executed on read-only RNode."

      verifyNoMoreInteractions(blockDagStorage)
      verifyNoMoreInteractions(blockStore)
      verifyNoMoreInteractions(runtimeManager)
    }
  }

  private def exploratoryDeploy[F[_]: Async: BlockStore: BlockDagStorage: RuntimeManager: Log: Span](
      term: String,
      block: BlockHash,
      validatorIdOpt: Option[ValidatorIdentity] = none
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]] =
    for {
      blockApi <- createBlockApi[F](genesis.shardId, 50, validatorIdOpt)
      res      <- blockApi.exploratoryDeploy(term, blockHash = block.toHexString.some)
    } yield res
}
