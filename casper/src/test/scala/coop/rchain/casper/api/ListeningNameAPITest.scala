package coop.rchain.casper.api

import cats.Id
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.{BlockStoreFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.{Channel, Expr, Par}
import coop.rchain.casper.HashSetCasperTest

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.GInt
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable

class ListeningNameAPITest extends FlatSpec with Matchers with BlockStoreFixture {

  import HashSetCasperTest._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val genesis                     = createGenesis(validators)

  "getListeningNameResponse" should "work with unsorted channels" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    def basicDeployData: DeployData = {
      val timestamp = System.currentTimeMillis()
      DeployData()
        .withUser(ByteString.EMPTY)
        .withTimestamp(timestamp)
        .withTerm("@{ 3 | 2 | 1 }!(0)")
    }

    val Some(block) = node.casperEff.deploy(basicDeployData) *> node.casperEff.createBlock
    node.casperEff.addBlock(block)

    val listeningName =
      Channel(Quote(Par().copy(exprs = Seq(Expr(GInt(2)), Expr(GInt(1)), Expr(GInt(3))))))
    val resultData = Par().copy(exprs = Seq(Expr(GInt(0))))
    val listeningNameResponse1 =
      BlockAPI.getListeningNameResponse[Id](listeningName)
    val data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
    val blocks1 = listeningNameResponse1.blockResults.map(_.block)
    data1 should be(List(List(resultData)))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)
  }

  "getListeningNameResponse" should "work across a chain" in {
    val nodes                               = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    implicit val nodeZeroConstructor        = nodes(0).constructor
    implicit val nodeZeroSafetyOracleEffect = nodes(0).turanOracleEffect
    implicit val nodeZeroLogEffect          = nodes(0).logEff
    implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

    val deployDatas = (0 to 7).map(_ => ProtoUtil.basicDeployData(0))

    val Some(block1) = nodes(0).casperEff.deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block1)
    nodes(1).receive()
    nodes(2).receive()

    val listeningName = Channel(Quote(Par().copy(exprs = Seq(Expr(GInt(0))))))
    val resultData    = Par().copy(exprs = Seq(Expr(GInt(0))))
    val listeningNameResponse1 =
      BlockAPI.getListeningNameResponse[Id](listeningName)
    val data1   = listeningNameResponse1.blockResults.map(_.postBlockData)
    val blocks1 = listeningNameResponse1.blockResults.map(_.block)
    data1 should be(List(List(resultData)))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)

    val Some(block2) = nodes(1).casperEff.deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block2)
    nodes(0).receive()
    nodes(2).receive()

    val Some(block3) = nodes(2).casperEff.deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block3)
    nodes(0).receive()
    nodes(1).receive()

    val Some(block4) = nodes(0).casperEff.deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block4)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse2 =
      BlockAPI.getListeningNameResponse[Id](listeningName)
    val data2   = listeningNameResponse2.blockResults.map(_.postBlockData)
    val blocks2 = listeningNameResponse2.blockResults.map(_.block)
    data2 should be(
      List(List(resultData, resultData, resultData, resultData),
           List(resultData, resultData, resultData),
           List(resultData, resultData),
           List(resultData)))
    blocks2.length should be(4)
    listeningNameResponse2.length should be(4)

    val Some(block5) = nodes(1).casperEff.deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block5)
    nodes(0).receive()
    nodes(2).receive()

    val Some(block6) = nodes(2).casperEff.deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block6)
    nodes(0).receive()
    nodes(1).receive()

    val Some(block7) = nodes(0).casperEff.deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block7)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse3 =
      BlockAPI.getListeningNameResponse[Id](listeningName)
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
      ))
    blocks3.length should be(7)
    listeningNameResponse3.length should be(7)

    nodes.foreach(_.tearDown())
  }
}
