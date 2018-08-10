package coop.rchain.casper.api

import cats.{ApplicativeError, Id}
import cats.data.EitherT
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper.{
  BlockStoreFixture,
  BlockStoreTestFixture,
  CasperEffect,
  HashSetCasperTestNode
}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.transport
import coop.rchain.comm.transport.CommMessages.packet
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.{Channel, Expr, PCost, Par}
import coop.rchain.rholang.interpreter.Runtime
import java.nio.file.Files

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.HashSetCasperTest.createGenesis
import coop.rchain.casper.{
  HashSetCasperTest,
  MultiParentCasper,
  MultiParentCasperConstructor,
  SafetyOracle
}
import coop.rchain.casper.genesis.contracts.ProofOfStakeValidator
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.shared.PathOps.RichPath

import scala.collection.immutable

class ListeningNameAPITest extends FlatSpec with Matchers with BlockStoreFixture {

  import HashSetCasperTest._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val genesis                     = createGenesis(validators)

  "getListeningNameResponse" should "return listening names across a chain" in {
    val stake                 = 10
    val equalBonds            = validators.zipWithIndex.map { case (v, _) => v -> stake }.toMap
    val genesisWithEqualBonds = buildGenesis(equalBonds)
    val nodes                 = HashSetCasperTestNode.network(validatorKeys.take(3), genesisWithEqualBonds)
    implicit val nodeZeroConstructor = MultiParentCasperConstructor
      .successCasperConstructor[Id](
        ApprovedBlock(candidate = Some(ApprovedBlockCandidate(block = Some(genesis)))),
        nodes(0).casperEff)
    implicit val nodeZeroSafetyOracleEffect = nodes(0).turanOracleEffect
    implicit val nodeZeroLogEffect          = nodes(0).logEff
    implicit val nodeZeroBlockStoreEffect   = nodes(0).blockStore

    val deploys = (0 to 7).map(_ => ProtoUtil.basicDeploy(0))

    val Some(block1) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block1)
    nodes(1).receive()
    nodes(2).receive()

    val listeningName = Channel(Quote(Par().copy(exprs = Seq(Expr(GInt(0))))))
    val listeningNameResponse1 =
      BlockAPI.getListeningNameResponse[Id](ListeningNameQuery(Some(listeningName)))
    val data1   = listeningNameResponse1.blockResults.map(_.datum)
    val blocks1 = listeningNameResponse1.blockResults.map(_.block)
    data1 should be(List(List("@{0}")))
    blocks1.length should be(1)
    listeningNameResponse1.length should be(1)

    val Some(block2) = nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block2)
    nodes(0).receive()
    nodes(2).receive()

    val Some(block3) = nodes(2).casperEff.deploy(deploys(2)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block3)
    nodes(0).receive()
    nodes(1).receive()

    val Some(block4) = nodes(0).casperEff.deploy(deploys(3)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block4)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse2 =
      BlockAPI.getListeningNameResponse[Id](ListeningNameQuery(Some(listeningName)))
    val data2   = listeningNameResponse2.blockResults.map(_.datum)
    val blocks2 = listeningNameResponse2.blockResults.map(_.block)
    data2 should be(
      List(List("@{0}", "@{0}", "@{0}", "@{0}"),
           List("@{0}", "@{0}", "@{0}"),
           List("@{0}", "@{0}"),
           List("@{0}")))
    blocks2.length should be(4)
    listeningNameResponse2.length should be(4)

    val Some(block5) = nodes(1).casperEff.deploy(deploys(4)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block5)
    nodes(0).receive()
    nodes(2).receive()

    val Some(block6) = nodes(2).casperEff.deploy(deploys(5)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block6)
    nodes(0).receive()
    nodes(1).receive()

    val Some(block7) = nodes(0).casperEff.deploy(deploys(6)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block7)
    nodes(1).receive()
    nodes(2).receive()

    val listeningNameResponse3 =
      BlockAPI.getListeningNameResponse[Id](ListeningNameQuery(Some(listeningName)))
    val data3   = listeningNameResponse3.blockResults.map(_.datum)
    val blocks3 = listeningNameResponse3.blockResults.map(_.block)
    data3 should be(List(List("@{0}", "@{0}", "@{0}", "@{0}", "@{0}", "@{0}", "@{0}"), List("@{0}", "@{0}", "@{0}", "@{0}", "@{0}", "@{0}"), List("@{0}", "@{0}", "@{0}", "@{0}", "@{0}"), List("@{0}", "@{0}", "@{0}", "@{0}"), List("@{0}", "@{0}", "@{0}"), List("@{0}", "@{0}"), List("@{0}")))
    blocks3.length should be(7)
    listeningNameResponse3.length should be(7)

    nodes.foreach(_.tearDown())
  }
}
