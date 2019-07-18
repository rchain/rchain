package coop.rchain.casper.engine

import cats._
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift}
import cats.temp.par
import coop.rchain.blockstorage._
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.helper.BlockDagStorageTestFixture
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{GenesisBuilder, TestTime}
import coop.rchain.catscontrib.ApplicativeError_
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Cell
import monix.eval.Task
import monix.execution.Scheduler

object Setup {
  def apply() = new {
    implicit val log              = new LogStub[Task]
    implicit val eventLogStub     = new EventLogStub[Task]
    implicit val metrics          = new Metrics.MetricsNOP[Task]
    implicit val span: Span[Task] = NoopSpan[Task]()
    val networkId                 = "test"
    val scheduler                 = Scheduler.io("test")
    val runtimeDir                = BlockDagStorageTestFixture.blockStorageDir
    val activeRuntime =
      Runtime
        .createWithEmptyCost[Task](runtimeDir, 3024L * 1024)(
          ContextShift[Task],
          Concurrent[Task],
          log,
          metrics,
          span,
          par.Par[Task],
          scheduler
        )
        .unsafeRunSync(scheduler)

    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync(scheduler)

    val params @ (_, Genesis(shardId, timestamp, proofOfStake, genesisPk, vaults, supply)) =
      GenesisBuilder.buildGenesisParameters()

    val GenesisContext(genesisBlock, validatorKeyPairs, _) = GenesisBuilder.buildGenesis(params)

    val (validatorSk, validatorPk) = validatorKeyPairs.head
    val bonds                      = proofOfStake.validators.flatMap(Validator.unapply).toMap
    val requiredSigs               = 1

    val validatorId = ValidatorIdentity(validatorPk, validatorSk, "secp256k1")
    val bap = new BlockApproverProtocol(
      validatorId,
      timestamp,
      bonds,
      proofOfStake.minimumBond,
      proofOfStake.maximumBond,
      requiredSigs
    )
    val local: PeerNode = peerNode("src", 40400)

    implicit val nodeDiscovery = new NodeDiscoveryStub[Task]
    implicit val connectionsCell: ConnectionsCell[Task] =
      Cell.unsafe[Task, Connections](List(local))
    implicit val transportLayer = new TransportLayerStub[Task]
    implicit val rpConf         = createRPConfAsk[Task](local)
    implicit val time           = TestTime.instance
    implicit val errHandler =
      ApplicativeError_.applicativeError(new ApplicativeError[Task, CommError] {
        override def raiseError[A](e: CommError): Task[A] =
          Task.raiseError(new Exception(s"CommError: $e"))
        override def handleErrorWith[A](fa: Task[A])(f: CommError => Task[A]): Task[A] =
          fa.onErrorHandleWith(th => f(UnknownCommError(th.getMessage)))
        override def pure[A](x: A): Task[A]                           = Task.pure(x)
        override def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] = Applicative[Task].ap(ff)(fa)
      })
    implicit val lab =
      LastApprovedBlock.of[Task].unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val blockMap         = Ref.unsafe[Task, Map[BlockHash, BlockMessage]](Map.empty)
    implicit val approvedBlockRef = Ref.unsafe[Task, Option[ApprovedBlock]](None)
    implicit val blockStore       = InMemBlockStore.create[Task]
    implicit val blockDagStorage = InMemBlockDagStorage
      .create[Task]
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val casperRef = MultiParentCasperRef.unsafe[Task](None)
    implicit val safetyOracle = new SafetyOracle[Task] {
      override def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[Task],
          estimateBlockHash: BlockHash
      ): Task[Float] = Task.pure(1.0f)
    }
    implicit val lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[Task](0f)
  }
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
