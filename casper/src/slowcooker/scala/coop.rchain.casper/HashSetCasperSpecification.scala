package coop.rchain.casper

import scala.util.{Random, Try}
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.blocks.proposer.Created
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.crypto.PublicKey
import coop.rchain.models.BlockHash
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck._
import org.scalacheck.commands.Commands

case class RNode(idx: Int, name: String, deployed: Boolean)

object HashSetCasperProperties extends Properties("HashSetCasper") {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  property("network") = HashSetCasperSpecification.property()
}

object HashSetCasperPropertiesApp {
  def main(args: Array[String]): Unit =
    HashSetCasperSpecification.property().check(_.withMinSuccessfulTests(1))
}

class NodeBox(val node: TestNode[Effect], var lastBlock: Option[BlockMessage]) {
  def update(bm: Option[BlockMessage]): Unit = this.lastBlock = bm
}

object HashSetCasperActions {
  import GenesisBuilder._

  def context(amount: Int): GenesisContext = buildGenesis(amount)

  def deploy(
      node: TestNode[Effect],
      deployData: Signed[DeployData]
  ): Effect[Either[DeployError, DeployId]] =
    node.casperEff.deploy(deployData)

  def create(node: TestNode[Effect]): Task[BlockMessage] =
    for {
      createBlockResult1 <- node.proposeSync
      block              <- node.blockStore.getUnsafe(createBlockResult1)
    } yield block

  def add(
      node: TestNode[Effect],
      signed: BlockMessage
  ): Effect[Either[Throwable, ValidBlockProcessing]] =
    Sync[Effect].attempt(
      node.addBlock(signed)
    )

  def deployment(
      i: Int,
      ts: Long = System.currentTimeMillis()
  ): Signed[DeployData] =
    ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", ts, shardId = SHARD_ID)

  private val SHARD_ID = "root-shard"

  implicit class EffectOps[A](f: Effect[A]) {
    def result: A = f.unsafeRunSync
  }
}

object HashSetCasperSpecification extends Commands {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  import HashSetCasperActions._

  type Close[F[_]] = F[Unit]

  case class CloseableNodes(nodes: List[NodeBox], closeNodes: Close[Effect])

  override type State = List[RNode]
  override type Sut   = CloseableNodes // System Under Test

  override def canCreateNewSut(
      newState: State,
      initSuts: Traversable[State],
      runningSuts: Traversable[Sut]
  ): Boolean =
    true

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    val genesisContext = context(state.size)

    val nodesResource = TestNode
      .networkEff(
        genesisContext,
        networkSize = state.size,
        shardId = genesisContext.genesisBlock.shardId
      )
      .map(_.toList)

    print(":")

    val (nodes, releaseF) = nodesResource.allocated.result

    CloseableNodes(nodes.map(new NodeBox(_, None)), releaseF)
  }

  override def destroySut(sut: Sut): Unit = {
    print(".")
    sut.closeNodes.result
  }

  override def genInitialState: Gen[State] =
    for {
      count <- Gen.chooseNum(2, 4)
      ids   = 0 to count
      nodes <- Gen.sequence[List[RNode], RNode](ids.map(genRNode))
    } yield nodes

  def genRNode(i: Int): Gen[RNode] = Gen.const(RNode(i, s"validator-$i", deployed = false))

  override def genCommand(state: State): Gen[Command] =
    for {
      idx  <- Gen.chooseNum(0, state.size - 1)
      node = state(idx)
      command <- Gen.frequency(
                  (2, genDeployOrPropose(node)),
                  (1, genReceive(node))
                )
    } yield command

  def genDeployOrPropose(node: RNode): Gen[Command] =
    if (node.deployed) Gen.const(Propose(node))
    else
      Gen.frequency(
        (1, Gen.const(Deploy(node))),
        (1, Gen.const(DeployAndPropose(node)))
      )

  def genReceive(node: RNode): Gen[Command] = Gen.const(Receive(node))

  case class Deploy(node: RNode) extends Command {
    override type Result = List[String]

    override def run(sut: Sut): Result = {
      val validator    = sut.nodes(node.idx)
      val blockMessage = (deploy(validator.node, deployment(0)) >> create(validator.node)).result
      validator.update(Some(blockMessage))
      validator.node.logEff.errors ++ validator.node.logEff.warns
    }

    override def nextState(state: State): State =
      state.patch(node.idx, Seq(node.copy(deployed = true)), 1)

    override def preCondition(state: State): Boolean = !state(node.idx).deployed

    override def postCondition(state: State, result: Try[Result]): Prop =
      result.isSuccess && result.get.isEmpty

  }

  def isValidAndNoErrors(result: Try[(ValidBlockProcessing, List[String])]): Boolean =
    result.isSuccess && result.get._1.isRight && result.get._2.isEmpty

  case class Propose(node: RNode) extends Command {
    override type Result = (ValidBlockProcessing, List[String])

    override def run(sut: Sut): Result = {
      val validator = sut.nodes(node.idx)
      val result    = add(validator.node, validator.lastBlock.get).result.right.get
      validator.update(None)
      (result, validator.node.logEff.errors ++ validator.node.logEff.warns)
    }

    override def nextState(state: State): State =
      state.patch(node.idx, Seq(node.copy(deployed = false)), 1)

    override def preCondition(state: State): Boolean = state(node.idx).deployed

    override def postCondition(state: State, result: Try[Result]): Prop = isValidAndNoErrors(result)
  }

  case class DeployAndPropose(node: RNode) extends Command {
    override type Result = (ValidBlockProcessing, List[String])

    override def run(sut: Sut): Result = {
      val validator = sut.nodes(node.idx).node
      val result = (deploy(validator, deployment(0)) >> create(validator) >>= (
          b => add(validator, b)
      )).result.right.get
      (result, validator.logEff.errors ++ validator.logEff.warns)
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = !state(node.idx).deployed

    override def postCondition(state: State, result: Try[Result]): Prop = isValidAndNoErrors(result)
  }

  case class Receive(node: RNode) extends Command {
    override type Result = List[String]

    override def run(sut: Sut): List[String] = {
      val validator = sut.nodes(node.idx).node
      validator.handleReceive().result
      validator.logEff.errors ++ validator.logEff.warns
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result.isSuccess && result.get.isEmpty
  }
}
