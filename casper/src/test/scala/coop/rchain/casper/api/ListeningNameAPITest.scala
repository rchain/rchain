package coop.rchain.casper.api

import cats.Id
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{Created, HashSetCasperTest}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.Capture._

class ListeningNameAPITest extends FlatSpec with Matchers {

  import HashSetCasperTest._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val bonds                       = createBonds(validators)
  private val genesis                     = createGenesis(bonds)

  "getListeningNameDataResponse" should "work with unsorted channels" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withUser(ByteString.EMPTY)
        .withTimestamp(timestamp)
        .withTerm("@{ 3 | 2 | 1 }!(0)")
        .withPhloLimit(accounting.MAX_VALUE)
    }

    for {
      createBlockResult <- node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
      Created(block)    = createBlockResult
      _                 <- node.casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])

      listeningName = Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
      resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
      listeningNameResponse1 <- BlockAPI
                                 .getListeningNameDataResponse[Effect](Int.MaxValue, listeningName)
      data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
      blocks1 = listeningNameResponse1.blockResults.map(_.block)
      _       = data1 should be(List(List(resultData)))
      _       = blocks1.length should be(1)
      result  = listeningNameResponse1.length should be(1)
    } yield result
  }

  it should "work across a chain" in effectTest {
    HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis).flatMap { nodes =>
      implicit val nodeZeroCasperRef          = nodes(0).multiparentCasperRef
      implicit val nodeZeroSafetyOracleEffect = nodes(0).turanOracleEffect
      implicit val nodeZeroLogEffect          = nodes(0).logEff
      implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

      implicit val timeEff = new LogicalTime[Effect]
      for {
        deployDatas <- (0 to 7).toList
                        .traverse[Effect, DeployData](_ => ProtoUtil.basicDeployData[Effect](0))

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
        data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
        blocks1 = listeningNameResponse1.blockResults.map(_.block)
        _       = data1 should be(List(List(resultData)))
        _       = blocks1.length should be(1)
        _       = listeningNameResponse1.length should be(1)

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
        data2   = listeningNameResponse2.blockResults.map(_.postBlockData)
        blocks2 = listeningNameResponse2.blockResults.map(_.block)
        _ = data2 should be(
          List(
            List(resultData, resultData, resultData, resultData),
            List(resultData, resultData, resultData),
            List(resultData, resultData),
            List(resultData)
          )
        )
        _ = blocks2.length should be(4)
        _ = listeningNameResponse2.length should be(4)

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
        data3   = listeningNameResponse3.blockResults.map(_.postBlockData)
        blocks3 = listeningNameResponse3.blockResults.map(_.block)
        _ = data3 should be(
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
        _ = blocks3.length should be(7)
        _ = listeningNameResponse3.length should be(7)

        listeningNameResponse3UntilDepth <- BlockAPI
                                             .getListeningNameDataResponse[Effect](1, listeningName)
        _ = listeningNameResponse3UntilDepth.length should be(1)

        listeningNameResponse3UntilDepth2 <- BlockAPI.getListeningNameDataResponse[Effect](
                                              2,
                                              listeningName
                                            )
        result = listeningNameResponse3UntilDepth2.length should be(2)

        _ = nodes.foreach(_.tearDown())
      } yield result
    }
  }

  "getListeningNameContinuationResponse" should "work with unsorted channels" in {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withUser(ByteString.EMPTY)
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
      continuations1 = listeningNameResponse1.blockResults.map(_.postBlockContinuations)
      blocks1        = listeningNameResponse1.blockResults.map(_.block)
      _              = continuations1 should be(List(List(desiredResult)))
      _              = blocks1.length should be(1)
      _              = listeningNameResponse1.length should be(1)

      listeningNamesShuffled2 = List(
        Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3)))),
        Par().copy(exprs = Seq(Expr(GInt(1)), Expr(GInt(2))))
      )
      listeningNameResponse2 <- BlockAPI.getListeningNameContinuationResponse[Effect](
                                 Int.MaxValue,
                                 listeningNamesShuffled2
                               )
      continuations2 = listeningNameResponse2.blockResults.map(_.postBlockContinuations)
      blocks2        = listeningNameResponse2.blockResults.map(_.block)
      _              = continuations2 should be(List(List(desiredResult)))
      _              = blocks2.length should be(1)
      result         = listeningNameResponse2.length should be(1)
    } yield result
  }
}
