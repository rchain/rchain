package coop.rchain.casper.api

import cats.implicits._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

class ListeningNameAPITest extends FlatSpec with Matchers with Inside {

  import GenesisBuilder._

  val genesis = buildGenesis()

  "getListeningNameDataResponse" should "work with unsorted channels" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._

      for {
        block <- ConstructDeploy.sourceDeployNowF("@{ 3 | 2 | 1 }!(0)") >>= (node.addBlock(_))

        listeningName = Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
        resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
        listeningNameResponse1 <- BlockAPI
                                   .getListeningNameDataResponse[Effect](
                                     Int.MaxValue,
                                     listeningName
                                   )
        _ = inside(listeningNameResponse1) {
          case Right(ListeningNameDataResponse(blockResults, l)) =>
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
    HashSetCasperTestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      implicit val nodeEngineCell             = nodes(0).engineCell
      implicit val nodeZeroSafetyOracleEffect = nodes(0).cliqueOracleEffect
      implicit val nodeZeroLogEffect          = nodes(0).logEff
      implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

      implicit val timeEff = new LogicalTime[Effect]
      for {
        deployDatas <- (0 to 7).toList
                        .traverse[Effect, DeployData](
                          _ => ConstructDeploy.basicDeployData[Effect](0)
                        )

        block1 <- nodes(0).addBlock(deployDatas(0))
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        listeningName = Par().copy(exprs = Seq(Expr(GInt(0))))
        resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
        listeningNameResponse1 <- BlockAPI.getListeningNameDataResponse[Effect](
                                   Int.MaxValue,
                                   listeningName
                                 )
        _ = inside(listeningNameResponse1) {
          case Right(ListeningNameDataResponse(blockResults, l)) =>
            val data1   = blockResults.map(_.postBlockData)
            val blocks1 = blockResults.map(_.block)
            data1 should be(List(List(resultData)))
            blocks1.length should be(1)
            l should be(1)
        }
        block2 <- nodes(1).addBlock(deployDatas(1))
        _      <- nodes(0).receive()
        _      <- nodes(2).receive()

        block3 <- nodes(2).addBlock(deployDatas(2))
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        block4 <- nodes(0).addBlock(deployDatas(3))
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        listeningNameResponse2 <- BlockAPI.getListeningNameDataResponse[Effect](
                                   Int.MaxValue,
                                   listeningName
                                 )
        _ = inside(listeningNameResponse2) {
          case Right(ListeningNameDataResponse(blockResults, l)) =>
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
        block5 <- nodes(1).addBlock(deployDatas(4))
        _      <- nodes(0).receive()
        _      <- nodes(2).receive()

        block6 <- nodes(2).addBlock(deployDatas(5))
        _      <- nodes(0).receive()
        _      <- nodes(1).receive()

        block7 <- nodes(0).addBlock(deployDatas(6))
        _      <- nodes(1).receive()
        _      <- nodes(2).receive()

        listeningNameResponse3 <- BlockAPI.getListeningNameDataResponse[Effect](
                                   Int.MaxValue,
                                   listeningName
                                 )
        _ = inside(listeningNameResponse3) {
          case Right(ListeningNameDataResponse(blockResults, l)) =>
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
        listeningNameResponse3UntilDepth <- BlockAPI
                                             .getListeningNameDataResponse[Effect](1, listeningName)
        _ = inside(listeningNameResponse3UntilDepth) {
          case Right(ListeningNameDataResponse(_, l)) => l should be(1)
        }
        listeningNameResponse3UntilDepth2 <- BlockAPI.getListeningNameDataResponse[Effect](
                                              2,
                                              listeningName
                                            )
        _ = inside(listeningNameResponse3UntilDepth2) {
          case Right(ListeningNameDataResponse(_, l)) => l should be(2)
        }
      } yield ()
    }
  }

  "getListeningNameContinuationResponse" should "work with unsorted channels" in {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._

      def basicDeployData: DeployData =
        ConstructDeploy.sourceDeployNow("for (@0 <- @{ 3 | 2 | 1 }; @1 <- @{ 2 | 1 }) { 0 }")

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
          Some(Par().copy(exprs = Vector(Expr(GInt(0)))))
        )
        listeningNameResponse1 <- BlockAPI.getListeningNameContinuationResponse[Effect](
                                   Int.MaxValue,
                                   listeningNamesShuffled1
                                 )
        _ = inside(listeningNameResponse1) {
          case Right(ListeningNameContinuationResponse(blockResults, l)) =>
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
        listeningNameResponse2 <- BlockAPI.getListeningNameContinuationResponse[Effect](
                                   Int.MaxValue,
                                   listeningNamesShuffled2
                                 )
        _ = inside(listeningNameResponse2) {
          case Right(ListeningNameContinuationResponse(blockResults, l)) =>
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
