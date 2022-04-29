package coop.rchain.casper.api

import cats.syntax.all._
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.{BlockApiFixture, TestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

class ListeningNameAPITest extends FlatSpec with Matchers with Inside with BlockApiFixture {

  import GenesisBuilder._

  val genesis = buildGenesis()

  "getListeningNameDataResponse" should "work with unsorted channels" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      import node.t

      for {
        block <- ConstructDeploy.sourceDeployNowF(
                  "@{ 3 | 2 | 1 }!(0)",
                  shardId = this.genesis.genesisBlock.shardId
                ) >>= (node.addBlock(_))

        listeningName          = Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
        resultData             = Par().copy(exprs = Seq(Expr(GInt(0))))
        blockApi               <- createBlockApi(node)
        depth                  = node.apiMaxBlocksLimit
        listeningNameResponse1 <- blockApi.getListeningNameDataResponse(depth, listeningName)
        _ = inside(listeningNameResponse1) {
          case Right((blockResults, l)) =>
            val data1   = blockResults.map(_.postBlockData)
            val blocks1 = blockResults.map(_.block)
            data1 should be(List(List(resultData)))
            blocks1.length should be(1)
            l should be(1)
        }
      } yield ()
    }
  }

  it should "work across a chain" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      implicit val timeEff = new LogicalTime[Effect]
      for {
        deployDatas <- (0 to 7).toList
                        .traverse[Effect, Signed[DeployData]](
                          _ =>
                            ConstructDeploy
                              .basicDeployData[Effect](0, shardId = genesis.genesisBlock.shardId)
                        )

        block1 <- nodes(0).propagateBlock(deployDatas(0))(nodes: _*)

        listeningName          = Par().copy(exprs = Seq(Expr(GInt(0))))
        resultData             = Par().copy(exprs = Seq(Expr(GInt(0))))
        blockApi               <- createBlockApi(nodes(0))
        depth                  = nodes(0).apiMaxBlocksLimit
        listeningNameResponse1 <- blockApi.getListeningNameDataResponse(depth, listeningName)
        _ = inside(listeningNameResponse1) {
          case Right((blockResults, l)) =>
            val data1   = blockResults.map(_.postBlockData)
            val blocks1 = blockResults.map(_.block)
            data1 should be(List(List(resultData)))
            blocks1.length should be(1)
            l should be(1)
        }
        block2 <- nodes(1).propagateBlock(deployDatas(1))(nodes: _*)
        block3 <- nodes(2).propagateBlock(deployDatas(2))(nodes: _*)
        block4 <- nodes(0).propagateBlock(deployDatas(3))(nodes: _*)

        listeningNameResponse2 <- blockApi.getListeningNameDataResponse(depth, listeningName)
        _ = inside(listeningNameResponse2) {
          case Right((blockResults, l)) =>
            val data2   = blockResults.map(_.postBlockData)
            val blocks2 = blockResults.map(_.block)
            data2 should be(
              List(
                List(resultData, resultData, resultData, resultData),
                List(resultData, resultData, resultData),
                List(resultData, resultData),
                List(resultData)
              )
            )
            blocks2.length should be(4)
            l should be(4)
        }
        block5 <- nodes(1).propagateBlock(deployDatas(4))(nodes: _*)
        block6 <- nodes(2).propagateBlock(deployDatas(5))(nodes: _*)
        block7 <- nodes(0).propagateBlock(deployDatas(6))(nodes: _*)

        listeningNameResponse3 <- blockApi.getListeningNameDataResponse(depth, listeningName)
        _ = inside(listeningNameResponse3) {
          case Right((blockResults, l)) =>
            val data3   = blockResults.map(_.postBlockData)
            val blocks3 = blockResults.map(_.block)
            data3 should be(
              List(
                List(
                  resultData,
                  resultData,
                  resultData,
                  resultData,
                  resultData,
                  resultData,
                  resultData
                ),
                List(resultData, resultData, resultData, resultData, resultData, resultData),
                List(resultData, resultData, resultData, resultData, resultData),
                List(resultData, resultData, resultData, resultData),
                List(resultData, resultData, resultData),
                List(resultData, resultData),
                List(resultData)
              )
            )
            blocks3.length should be(7)
            l should be(7)
        }
        listeningNameResponse3UntilDepth <- blockApi.getListeningNameDataResponse(1, listeningName)
        _ = inside(listeningNameResponse3UntilDepth) {
          case Right((_, l)) => l should be(1)
        }
        listeningNameResponse3UntilDepth2 <- blockApi.getListeningNameDataResponse(2, listeningName)
        _ = inside(listeningNameResponse3UntilDepth2) {
          case Right((_, l)) => l should be(2)
        }
      } yield ()
    }
  }

  "getListeningNameContinuationResponse" should "work with unsorted channels" in {
    TestNode.standaloneEff(genesis).use { node =>
      def basicDeployData: Signed[DeployData] =
        ConstructDeploy.sourceDeployNow("for (@0 <- @{ 3 | 2 | 1 } & @1 <- @{ 2 | 1 }) { 0 }")

      for {
        block <- node.addBlock(basicDeployData)

        listeningNamesShuffled1 = List(
          Par().copy(exprs = Seq(Expr(GInt(1)), Expr(GInt(2)))),
          Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
        )
        desiredResult = WaitingContinuationInfo(
          List(
            BindPattern(Vector(Par().copy(exprs = Vector(Expr(GInt(1))))), None, 0),
            BindPattern(Vector(Par().copy(exprs = Vector(Expr(GInt(0))))), None, 0)
          ),
          Par().copy(exprs = Vector(Expr(GInt(0))))
        )
        blockApi <- createBlockApi(node)
        depth    = node.apiMaxBlocksLimit
        listeningNameResponse1 <- blockApi.getListeningNameContinuationResponse(
                                   depth,
                                   listeningNamesShuffled1
                                 )
        _ = inside(listeningNameResponse1) {
          case Right((blockResults, l)) =>
            val continuations1 = blockResults.map(_.postBlockContinuations)
            val blocks1        = blockResults.map(_.block)
            continuations1 should be(List(List(desiredResult)))
            blocks1.length should be(1)
            l should be(1)
        }
        listeningNamesShuffled2 = List(
          Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3)))),
          Par().copy(exprs = Seq(Expr(GInt(1)), Expr(GInt(2))))
        )
        listeningNameResponse2 <- blockApi.getListeningNameContinuationResponse(
                                   depth,
                                   listeningNamesShuffled2
                                 )
        _ = inside(listeningNameResponse2) {
          case Right((blockResults, l)) =>
            val continuations2 = blockResults.map(_.postBlockContinuations)
            val blocks2        = blockResults.map(_.block)
            continuations2 should be(List(List(desiredResult)))
            blocks2.length should be(1)
            l should be(1)
        }
      } yield ()
    }
  }
}
