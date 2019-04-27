package coop.rchain.casper.api

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Created
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

class ListeningNameAPITest extends FlatSpec with Matchers with Inside {

  import coop.rchain.casper.MultiParentCasperTestUtil._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val bonds                       = createBonds(validators)
  private val genesis                     = createGenesis(bonds)

  "getListeningNameDataResponse" should "work with unsorted channels" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      ConstructDeploy.sourceDeploy(
        source = "@{ 3 | 2 | 1 }!(0)",
        timestamp = timestamp,
        phlos = accounting.MAX_VALUE
      )
    }

    for {
      createBlockResult <- node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
      Created(block)    = createBlockResult
      _                 <- node.casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])

      listeningName = Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
      resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
      listeningNameResponse1 <- BlockAPI
                                 .getListeningNameDataResponse[Effect](Int.MaxValue, listeningName)
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

  it should "work across a chain" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis).flatMap { nodes =>
      implicit val nodeZeroCasperRef          = nodes(0).multiparentCasperRef
      implicit val nodeZeroSafetyOracleEffect = nodes(0).cliqueOracleEffect
      implicit val nodeZeroLogEffect          = nodes(0).logEff
      implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

      implicit val timeEff = new LogicalTime[Effect]
      for {
        deployDatas <- (0 to 7).toList
                        .traverse[Effect, DeployData](
                          _ => ConstructDeploy.basicDeployData[Effect](0)
                        )

        createBlock1Result <- nodes(0).casperEff
                               .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
        Created(block1) = createBlock1Result
        _               <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(1).receive()
        _               <- nodes(2).receive()

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
        createBlock2Result <- nodes(1).casperEff
                               .deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
        Created(block2) = createBlock2Result
        _               <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(0).receive()
        _               <- nodes(2).receive()

        createBlock3Result <- nodes(2).casperEff
                               .deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
        Created(block3) = createBlock3Result
        _               <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(0).receive()
        _               <- nodes(1).receive()

        createBlock4Result <- nodes(0).casperEff
                               .deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
        Created(block4) = createBlock4Result
        _               <- nodes(0).casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(1).receive()
        _               <- nodes(2).receive()

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
        createBlock5Result <- nodes(1).casperEff
                               .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
        Created(block5) = createBlock5Result
        _               <- nodes(1).casperEff.addBlock(block5, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(0).receive()
        _               <- nodes(2).receive()

        createBlock6Result <- nodes(2).casperEff
                               .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
        Created(block6) = createBlock6Result
        _               <- nodes(2).casperEff.addBlock(block6, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(0).receive()
        _               <- nodes(1).receive()

        createBlock7Result <- nodes(0).casperEff
                               .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
        Created(block7) = createBlock7Result
        _               <- nodes(0).casperEff.addBlock(block7, ignoreDoppelgangerCheck[Effect])
        _               <- nodes(1).receive()
        _               <- nodes(2).receive()

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
        _ = nodes.foreach(_.tearDown())
      } yield ()
    }
  }

  "getListeningNameContinuationResponse" should "work with unsorted channels" in {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withDeployer(ByteString.EMPTY)
        .withTimestamp(timestamp)
        .withTerm("for (@0 <- @{ 3 | 2 | 1 }; @1 <- @{ 2 | 1 }) { 0 }")
        .withPhloLimit(accounting.MAX_VALUE)
    }

    for {
      createBlockResult <- node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
      Created(block)    = createBlockResult
      _                 <- node.casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])

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
