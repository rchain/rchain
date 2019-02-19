package coop.rchain.casper

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.contracts.{Faucet, PreWallet}
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.CommError
import monix.eval.Task
import org.scalacheck.{Gen, Prop, Properties, Shrink}
import org.scalacheck.commands.Commands

import scala.collection.immutable
import scala.util.{Random, Try}

case class RNode(idx: Int, name: String, deployed: Boolean)

object HashSetCasperProperties extends Properties("HashSetCasper") {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  property("network") = HashSetCasperSpecification.property()
}

class NodeBox(val node: HashSetCasperTestNode[Effect], var lastBlock: Option[BlockMessage]) {
  def update(bm: Option[BlockMessage]): Unit = this.lastBlock = bm
}

object HashSetCasperActions {
  import HashSetCasperTest._

  type ValidatorKey = Array[Byte]

  def context(
      amount: Int,
      bondsGen: Seq[ValidatorKey] => Map[ValidatorKey, Long]
  ): (BlockMessage, immutable.IndexedSeq[ValidatorKey]) = {
    val (validatorKeys, validators) = (1 to amount).map(_ => Ed25519.newKeyPair).unzip
    val (_, ethPubKeys)             = (1 to amount).map(_ => Secp256k1.newKeyPair).unzip
    val ethAddresses =
      ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
    val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
    val bonds       = bondsGen(validators)
    val minimumBond = 100L
    val genesis =
      buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)
    (genesis, validatorKeys)
  }

  def deploy(
      node: HashSetCasperTestNode[Effect],
      deployData: DeployData
  ): Effect[Either[Throwable, Unit]] =
    node.casperEff.deploy(deployData)

  def create(node: HashSetCasperTestNode[Effect]): EitherT[Task, CommError, BlockMessage] =
    for {
      createBlockResult1    <- node.casperEff.createBlock
      Created(signedBlock1) = createBlockResult1
    } yield signedBlock1

  def add(
      node: HashSetCasperTestNode[Effect],
      signed: BlockMessage
  ): Effect[Either[Throwable, BlockStatus]] =
    Sync[Effect].attempt(
      node.casperEff.addBlock(signed, ignoreDoppelgangerCheck[Effect])
    )

  def deployment(i: Int, ts: Long = System.currentTimeMillis()): DeployData =
    ProtoUtil.sourceDeploy(s"new x in { x!(0) }", ts, accounting.MAX_VALUE)

  implicit class EffectOps[A](f: Effect[A]) {
    def result: A = f.value.unsafeRunSync.right.get
  }
}

object HashSetCasperSpecification extends Commands {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  import HashSetCasperActions._

  override type State = List[RNode]
  override type Sut   = List[NodeBox] // System Under Test

  override def canCreateNewSut(
      newState: State,
      initSuts: Traversable[State],
      runningSuts: Traversable[Sut]
  ): Boolean =
    true

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    val (genesis, validatorKeys) = context(state.size, validators => {
      val weights = Random.shuffle((1L to validators.size).toList)
      validators.zip(weights).toMap
    })

    val network = TestNetwork.empty[Effect]

    val nodes = HashSetCasperTestNode
      .networkEff(validatorKeys.take(state.size), genesis, testNetwork = network)
      .map(_.toList)

    nodes.result.map(new NodeBox(_, None))
  }

  override def destroySut(sut: Sut): Unit = sut.traverse_(_.node.tearDown()).result

  override def genInitialState: Gen[State] =
    for {
      count: Int <- Gen.chooseNum(2, 4)
      ids        = 0 to count
      nodes      <- Gen.sequence[List[RNode], RNode](ids.map(genRNode))
    } yield nodes

  def genRNode(i: Int): Gen[RNode] = Gen.const(RNode(i, s"validator-$i", deployed = false))

  override def genCommand(state: State): Gen[Command] =
    for {
      idx: Int <- Gen.chooseNum(0, state.size - 1)
      node     = state(idx)
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
      val validator    = sut(node.idx)
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

  def isValidAndNoErrors(result: Try[(BlockStatus, List[String])]): Boolean =
    result.isSuccess && result.get._1 == Valid && result.get._2.isEmpty

  case class Propose(node: RNode) extends Command {
    override type Result = (BlockStatus, List[String])

    override def run(sut: Sut): Result = {
      val validator = sut(node.idx)
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
    override type Result = (BlockStatus, List[String])

    override def run(sut: Sut): Result = {
      val validator = sut(node.idx).node
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
      val validator = sut(node.idx).node
      validator.receive().result
      validator.logEff.errors ++ validator.logEff.warns
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result.isSuccess && result.get.isEmpty
  }
}
