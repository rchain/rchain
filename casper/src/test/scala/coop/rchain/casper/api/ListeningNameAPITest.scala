package coop.rchain.casper.api

import cats.Id
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.{BlockStoreFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{Created, HashSetCasperTest}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.Capture._

class ListeningNameAPITest extends FlatSpec with Matchers with BlockStoreFixture {

  import HashSetCasperTest._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val bonds                       = createBonds(validators)
  private val genesis                     = createGenesis(bonds)

  "getListeningNameDataResponse" should "work with unsorted channels" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withUser(ByteString.EMPTY)
        .withTimestamp(timestamp)
        .withTerm("@{ 3 | 2 | 1 }!(0)")
        .withPhloLimit(accounting.MAX_VALUE)
    }

    val Created(block) = node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
    node.casperEff.addBlock(block)

    val listeningName =
      Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
    val resultData = Par().copy(exprs = Seq(Expr(GInt(0))))
    val listeningNameResponse1 =
      BlockAPI.getListeningNameDataResponse[Id](Int.MaxValue, listeningName)
    val data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
    val blocks1 = listeningNameResponse1.blockResults.map(_.block)
    data1 should be(List(List(resultData)))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)
  }

  it should "work across a chain" in {
    val nodes                               = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    implicit val nodeZeroCasperRef          = nodes(0).multiparentCasperRef
    implicit val nodeZeroSafetyOracleEffect = nodes(0).turanOracleEffect
    implicit val nodeZeroLogEffect          = nodes(0).logEff
    implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

    implicit val timeEff = new LogicalTime[Id]
    val deployDatas      = (0 to 7).map(_ => ProtoUtil.basicDeployData[Id](0))

    val Created(block1) = nodes(0).casperEff
      .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block1)
    nodes(1).receive()
    nodes(2).receive()

    val listeningName = Par().copy(exprs = Seq(Expr(GInt(0))))
    val resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
    val listeningNameResponse1 =
      BlockAPI.getListeningNameDataResponse[Id](Int.MaxValue, listeningName)
    val data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
    val blocks1 = listeningNameResponse1.blockResults.map(_.block)
    data1 should be(List(List(resultData)))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)

    val Created(block2) = nodes(1).casperEff
      .deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block2)
    nodes(0).receive()
    nodes(2).receive()

    val Created(block3) = nodes(2).casperEff
      .deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block3)
    nodes(0).receive()
    nodes(1).receive()

    val Created(block4) = nodes(0).casperEff
      .deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block4)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse2 =
      BlockAPI.getListeningNameDataResponse[Id](Int.MaxValue, listeningName)
    val data2   = listeningNameResponse2.blockResults.map(_.postBlockData)
    val blocks2 = listeningNameResponse2.blockResults.map(_.block)
    data2 should be(
      List(
        List(resultData, resultData, resultData, resultData),
        List(resultData, resultData, resultData),
        List(resultData, resultData),
        List(resultData)
      )
    )
    blocks2.length should be(4)
    listeningNameResponse2.length should be(4)

    val Created(block5) = nodes(1).casperEff
      .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block5)
    nodes(0).receive()
    nodes(2).receive()

    val Created(block6) = nodes(2).casperEff
      .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block6)
    nodes(0).receive()
    nodes(1).receive()

    val Created(block7) = nodes(0).casperEff
      .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block7)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse3 =
      BlockAPI.getListeningNameDataResponse[Id](Int.MaxValue, listeningName)
    val data3   = listeningNameResponse3.blockResults.map(_.postBlockData)
    val blocks3 = listeningNameResponse3.blockResults.map(_.block)
    data3 should be(
      List(
        List(resultData, resultData, resultData, resultData, resultData, resultData, resultData),
        List(resultData, resultData, resultData, resultData, resultData, resultData),
        List(resultData, resultData, resultData, resultData, resultData),
        List(resultData, resultData, resultData, resultData),
        List(resultData, resultData, resultData),
        List(resultData, resultData),
        List(resultData)
      )
    )
    blocks3.length should be(7)
    listeningNameResponse3.length should be(7)

    val listeningNameResponse3UntilDepth =
      BlockAPI.getListeningNameDataResponse[Id](1, listeningName)
    listeningNameResponse3UntilDepth.length should be(1)

    val listeningNameResponse3UntilDepth2 =
      BlockAPI.getListeningNameDataResponse[Id](2, listeningName)
    listeningNameResponse3UntilDepth2.length should be(2)

    nodes.foreach(_.tearDown())
  }

  "getListeningNameContinuationResponse" should "work with unsorted channels" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withUser(ByteString.EMPTY)
        .withTimestamp(timestamp)
        .withTerm("for (@0 <- @{ 3 | 2 | 1 }; @1 <- @{ 2 | 1 }) { 0 }")
        .withPhloLimit(accounting.MAX_VALUE)
    }

    val Created(block) = node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
    node.casperEff.addBlock(block)

    val listeningNamesShuffled1 =
      List(
        Par().copy(exprs = Seq(Expr(GInt(1)), Expr(GInt(2)))),
        Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))
      )
    val result = WaitingContinuationInfo(
      List(
        BindPattern(Vector(Par().copy(exprs = Vector(Expr(GInt(1))))), None, 0),
        BindPattern(Vector(Par().copy(exprs = Vector(Expr(GInt(0))))), None, 0)
      ),
      Some(Par().copy(exprs = Vector(Expr(GInt(0)))))
    )
    val listeningNameResponse1 =
      BlockAPI.getListeningNameContinuationResponse[Id](Int.MaxValue, listeningNamesShuffled1)
    val continuations1 = listeningNameResponse1.blockResults.map(_.postBlockContinuations)
    val blocks1        = listeningNameResponse1.blockResults.map(_.block)
    continuations1 should be(List(List(result)))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)

    val listeningNamesShuffled2 =
      List(
        Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3)))),
        Par().copy(exprs = Seq(Expr(GInt(1)), Expr(GInt(2))))
      )
    val listeningNameResponse2 =
      BlockAPI.getListeningNameContinuationResponse[Id](Int.MaxValue, listeningNamesShuffled2)
    val continuations2 = listeningNameResponse2.blockResults.map(_.postBlockContinuations)
    val blocks2        = listeningNameResponse2.blockResults.map(_.block)
    continuations2 should be(List(List(result)))
    blocks2.length should be(1)
    listeningNameResponse2.length should be(1)
  }
}
