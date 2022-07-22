package coop.rchain.casper.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ConstructDeploy.{basicDeployData, sourceDeployNowF}
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.{PrettyPrinter, ValidatorIdentity}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExploratoryDeployAPITest
    extends AnyFlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {
  private val bonds                     = List(10L, 10L, 10L, 10L)
  private val validators                = randomValidatorKeyPairs.take(bonds.size).toList
  private val initialComputeBondsResult = validators.map(_._2.bytes.toByteString).zip(bonds).toMap
  private val gB                        = getRandomBlock(setBonds = initialComputeBondsResult.some)

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
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]

    val storedData = "data"
    for {
      produceDeploys0 <- basicDeployData[Task](0, shardId = gB.shardId)
      produceDeploys1 <- basicDeployData[Task](1, shardId = gB.shardId)
      putDataDeploy   <- sourceDeployNowF[Task](s"""@"store"!("$storedData")""", shardId = gB.shardId)

      _  = propagateBlock(validators.head, putDataDeploy)
      b2 = propagateBlock(validators.head, produceDeploys0)
      _  = propagateBlock(validators(1), produceDeploys1)

      readOnlyValidator = ValidatorIdentity(validators.last._1).some
      result <- exploratoryDeploy[Task](readOnlyValidator)(
                 "new return in { for (@data <- @\"store\") {return!(data)}}",
                 b2.blockHash
               )
      (par, b) = result.value
      _        = b.blockHash shouldBe PrettyPrinter.buildStringNoLimit(b2.blockHash)
      _ = par match {
        case Seq(Par(_, _, _, Seq(expr), _, _, _, _, _, _)) =>
          expr match {
            case Expr(GString(data)) => data shouldBe storedData
            case _                   => fail("Could not get data from exploretory api")
          }
      }
    } yield ()
  }

  it should "exploratoryDeploy return error on bonded validator" in {
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]

    for {
      produceDeploys <- basicDeployData[Task](0, shardId = gB.shardId)
      _              = propagateBlock(validators.head, produceDeploys)

      n1Validator = ValidatorIdentity(validators.head._1).some
      result <- exploratoryDeploy[Task](n1Validator)(
                 "new return in { return!(1) }",
                 ByteString.EMPTY
               )
      _ = result.left.value shouldBe "Exploratory deploy can only be executed on read-only RNode."
    } yield ()
  }

  private def createMocks[F[_]: Concurrent: Sync] = {
    val c   = Concurrent[F]
    val sp  = Mocks.createSpan[F]
    val log = Mocks.createLog[F]
    val bds = Mocks.createBlockDagStorage[F]
    val bs  = Mocks.createBlockStore[F]

    val rm = mock[RuntimeManager[F]]
    rm.computeBonds(*) returnsF initialComputeBondsResult

    (c, log, bds, bs, rm, sp)
  }

  private def propagateBlock(validator: (PrivateKey, PublicKey), deploy: Signed[DeployData]) =
    getRandomBlock(
      setValidator = validator._1.bytes.toByteString.some,
      setDeploys = Seq(ProcessedDeploy.empty(deploy)).some,
      setBonds = initialComputeBondsResult.some
    )

  private def exploratoryDeploy[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Span](
      validatorIdOpt: Option[ValidatorIdentity]
  )(term: String, block: BlockHash) =
    for {
      blockApi <- createBlockApi[F](gB.shardId, 50, validatorIdOpt)
      res      <- blockApi.exploratoryDeploy(term, blockHash = block.toHexString.some)
    } yield res
}
