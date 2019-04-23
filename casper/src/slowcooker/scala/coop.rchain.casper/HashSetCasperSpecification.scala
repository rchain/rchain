package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Resource, Sync}
import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.CommError
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.eval.Task
import org.scalacheck._
import org.scalacheck.commands.Commands

import scala.collection.immutable
import scala.util.{Random, Try}

case class RNode(idx: Int, name: String, deployed: Boolean)

object HashSetCasperProperties extends Properties("HashSetCasper") {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  property("network") = HashSetCasperSpecification.property()
}

object HashSetCasperPropertiesApp {
  def main(args: Array[String]): Unit =
    HashSetCasperSpecification.property().check(_.withMinSuccessfulTests(1))
}

class NodeBox(val node: HashSetCasperTestNode[Effect], var lastBlock: Option[BlockMessage]) {
  def update(bm: Option[BlockMessage]): Unit = this.lastBlock = bm
}

object HashSetCasperActions {
  import MultiParentCasperTestUtil._

  def context(
      amount: Int,
      bondsGen: Seq[PublicKey] => Map[PublicKey, Long]
  ): (BlockMessage, immutable.IndexedSeq[PrivateKey]) = {
    val (validatorKeys, validators) = (1 to amount).map(_ => Ed25519.newKeyPair).unzip
    val (_, ethPubKeys)             = (1 to amount).map(_ => Secp256k1.newKeyPair).unzip
    val ethAddresses =
      ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
    val wallets = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
    val bonds   = bondsGen(validators)
    val genesis =
      buildGenesis(
        Genesis(
          shardId = "HashSetCasperSpecification",
          wallets = wallets,
          proofOfStake = ProofOfStake(
            minimumBond = 0L,
            maximumBond = Long.MaxValue,
            validators = bonds.toSeq.map(Validator.tupled)
          ),
          faucet = true,
          genesisPk = Ed25519.newKeyPair._2,
          timestamp = 0L,
          vaults = bonds.toList.map {
            case (pk, stake) =>
              RevAddress.fromPublicKey(pk).map(Vault(_, stake))
          }.flattenOption,
          supply = Long.MaxValue
        )
      )
    (genesis, validatorKeys)
  }

  def deploy(
      node: HashSetCasperTestNode[Effect],
      deployData: DeployData
  ): Effect[Either[DeployError, Unit]] =
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
    ConstructDeploy.sourceDeploy(s"new x in { x!(0) }", ts, accounting.MAX_VALUE)

  implicit class EffectOps[A](f: Effect[A]) {
    def result: A = f.value.unsafeRunSync.right.get
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
    val (genesis, validatorKeys) = context(state.size, validators => {
      val weights = Random.shuffle((1L to validators.size.toLong).toList)
      validators.zip(weights).toMap
    })

    val network = TestNetwork.empty[Effect]

    val nodesResource = HashSetCasperTestNode
      .networkEff(validatorKeys.take(state.size), genesis, testNetwork = network)
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
      ids        = 0 to count
      nodes      <- Gen.sequence[List[RNode], RNode](ids.map(genRNode))
    } yield nodes

  def genRNode(i: Int): Gen[RNode] = Gen.const(RNode(i, s"validator-$i", deployed = false))

  override def genCommand(state: State): Gen[Command] =
    for {
      idx <- Gen.chooseNum(0, state.size - 1)
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

  def isValidAndNoErrors(result: Try[(BlockStatus, List[String])]): Boolean =
    result.isSuccess && result.get._1 == Valid && result.get._2.isEmpty

  case class Propose(node: RNode) extends Command {
    override type Result = (BlockStatus, List[String])

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
    override type Result = (BlockStatus, List[String])

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
      validator.receive().result
      validator.logEff.errors ++ validator.logEff.warns
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result.isSuccess && result.get.isEmpty
  }
}
