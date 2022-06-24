package coop.rchain.casper.api

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture, BlockGenerator, TestNode}
import coop.rchain.casper.util.ConstructDeploy.{basicDeployData, sourceDeployNowF}
import coop.rchain.casper.util.GenesisBuilder.{buildGenesis, buildGenesisParametersFromBonds}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.models.syntax._
import coop.rchain.shared.scalatestcontrib.effectTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExploratoryDeployAPITest
    extends AnyFlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture {
  implicit val metricsEff = new Metrics.MetricsNOP[Task]
  val genesisParameters   = buildGenesisParametersFromBonds(List(10L, 10L, 10L, 10L))
  val genesisContext      = buildGenesis(genesisParameters)

  def exploratoryDeploy(node: TestNode[Task])(term: String, block: BlockHash) =
    for {
      blockApi <- createBlockApi[Task](node)
      res      <- blockApi.exploratoryDeploy(term, blockHash = block.toHexString.some)
    } yield res

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
  it should "exploratoryDeploy get data from the read only node" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3, withReadOnlySize = 1).use {
      case nodes @ n1 +: n2 +: _ +: readOnly +: Seq() =>
        val storedData = "data"
        for {
          produceDeploys <- (0 until 2).toList.traverse(
                             i =>
                               basicDeployData[Task](
                                 i,
                                 shardId = genesisContext.genesisBlock.shardId
                               )
                           )
          putDataDeploy <- sourceDeployNowF[Task](
                            s"""@"store"!("$storedData")""",
                            shardId = genesisContext.genesisBlock.shardId
                          )
          _  <- n1.propagateBlock(putDataDeploy)(nodes: _*)
          b2 <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
          _  <- n2.propagateBlock(produceDeploys(1))(nodes: _*)

          result <- exploratoryDeploy(readOnly)(
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
  }

  it should "exploratoryDeploy return error on bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 1).use {
      case nodes @ n1 +: Seq() =>
        for {
          produceDeploys <- (0 until 1).toList.traverse(
                             i =>
                               basicDeployData[Task](
                                 i,
                                 shardId = genesisContext.genesisBlock.shardId
                               )
                           )
          _ <- n1.propagateBlock(produceDeploys(0))(nodes: _*)

          result <- exploratoryDeploy(n1)("new return in { return!(1) }", ByteString.EMPTY)
          _      = result.left.value shouldBe "Exploratory deploy can only be executed on read-only RNode."

        } yield ()
    }
  }
}
