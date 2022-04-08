package coop.rchain.casper.engine

import cats._
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagRepresentation}
import coop.rchain.blockstorage.deploy.KeyValueDeployStorage
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever.RequestState
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{GenesisBuilder, TestTime}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rspace.RSpace
import coop.rchain.rspace.state.instances.RSpaceStateManagerImpl
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Cell
import coop.rchain.store.InMemoryStoreManager
import fs2.concurrent.Queue
import monix.eval.Task
import monix.execution.Scheduler

object Setup {
  def apply() = new {

    implicit val errHandler =
      new ApplicativeError[Task, CommError] {
        override def raiseError[A](e: CommError): Task[A] =
          Task.raiseError(new Exception(s"CommError: $e"))
        override def handleErrorWith[A](fa: Task[A])(f: CommError => Task[A]): Task[A] =
          fa.onErrorHandleWith(th => f(UnknownCommError(th.getMessage)))
        override def pure[A](x: A): Task[A]                           = Task.pure(x)
        override def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] = Applicative[Task].ap(ff)(fa)
      }

    implicit val log              = new LogStub[Task]
    implicit val eventLogStub     = new EventLogStub[Task]
    implicit val metrics          = new Metrics.MetricsNOP[Task]
    implicit val span: Span[Task] = NoopSpan[Task]()
    implicit val scheduler        = Scheduler.Implicits.global
    import coop.rchain.rholang.interpreter.storage._
    implicit val m                     = matchListPar[Task]
    val params @ (_, _, genesisParams) = GenesisBuilder.buildGenesisParameters()
    val context                        = GenesisBuilder.buildGenesis(params)

    val networkId = "test"
    val spaceKVManager =
      mkTestRNodeStoreManager[Task](context.storageDirectory).runSyncUnsafe()
    val store = spaceKVManager.rSpaceStores.runSyncUnsafe()
    val spaces = RSpace
      .createWithReplay[Task, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
      .runSyncUnsafe()
    val (rspace, replay) = spaces
    val historyRepo      = rspace.historyRepo

    val (exporter, importer) = {
      (historyRepo.exporter.unsafeRunSync, historyRepo.importer.unsafeRunSync)
    }
    implicit val rspaceStateManager = RSpaceStateManagerImpl(exporter, importer)

    val mStore = RuntimeManager.mergeableStore(spaceKVManager).unsafeRunSync(scheduler)
    implicit val runtimeManager =
      RuntimeManager[Task](rspace, replay, historyRepo, mStore, Genesis.NonNegativeMergeableTagName)
        .unsafeRunSync(scheduler)

    val (validatorSk, validatorPk) = context.validatorKeyPairs.head
    val bonds                      = genesisParams.proofOfStake.validators.flatMap(Validator.unapply).toMap
    val requiredSigs               = 1
    val shardId                    = genesisParams.shardId
    val finalizationRate           = 1
    val deployTimestamp            = genesisParams.timestamp

    val genesis: BlockMessage = context.genesisBlock

    val validatorId = ValidatorIdentity(validatorPk, validatorSk, "secp256k1")
    val bap = BlockApproverProtocol
      .of[Task](
        validatorId,
        deployTimestamp,
        genesisParams.vaults,
        bonds,
        genesisParams.proofOfStake.minimumBond,
        genesisParams.proofOfStake.maximumBond,
        genesisParams.proofOfStake.epochLength,
        genesisParams.proofOfStake.quarantineLength,
        genesisParams.proofOfStake.numberOfActiveValidators,
        requiredSigs,
        genesisParams.proofOfStake.posMultiSigPublicKeys,
        genesisParams.proofOfStake.posMultiSigQuorum
      )
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)

    val local: PeerNode = peerNode("src", 40400)

    implicit val nodeDiscovery = new NodeDiscoveryStub[Task]
    implicit val connectionsCell: ConnectionsCell[Task] =
      Cell.unsafe[Task, Connections](List(local))
    implicit val transportLayer = new TransportLayerStub[Task]
    implicit val rpConf         = createRPConfAsk[Task](local)
    implicit val time           = TestTime.instance
    implicit val currentRequests: engine.BlockRetriever.RequestedBlocks[Task] =
      Ref.unsafe[Task, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
    implicit val commUtil = CommUtil.of[Task]
    implicit val lab =
      LastApprovedBlock.of[Task].unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    val kvm = InMemoryStoreManager[Task]()
    implicit val blockStore = {
      BlockStore[Task](kvm).unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    }
    implicit val approvedStore = {
      ApprovedStore[Task](kvm).unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    }
    implicit val blockDagStorage = BlockDagKeyValueStorage
      .create(kvm)
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val deployStorage = KeyValueDeployStorage[Task](kvm)
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)
    implicit val safetyOracle = new SafetyOracle[Task] {
      override def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[Task],
          estimateBlockHash: BlockHash
      ): Task[Float] = Task.pure(1.0f)
    }
    implicit val estimator                      = Estimator[Task](Estimator.UnlimitedParents, None)
    implicit val synchronyConstraintChecker     = SynchronyConstraintChecker[Task]
    implicit val lastFinalizedConstraintChecker = LastFinalizedHeightConstraintChecker[Task]
    implicit val blockRetriever                 = BlockRetriever.of[Task]

    implicit val casperBuffer = CasperBufferKeyValueStorage
      .create[Task](spaceKVManager)
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)

    implicit val blockProcessingQueue = Queue
      .unbounded[Task, (Casper[Task], BlockMessage)]
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)

    implicit val blockProcessingState = Ref
      .of[Task, Set[BlockHash]](Set.empty)
      .unsafeRunSync(monix.execution.Scheduler.Implicits.global)

    implicit val casperShardConf = CasperShardConf(
      -1,
      shardId,
      "",
      finalizationRate,
      Int.MaxValue,
      Int.MaxValue,
      0,
      Long.MaxValue,
      50,
      1,
      1,
      genesisParams.proofOfStake.minimumBond,
      genesisParams.proofOfStake.maximumBond,
      genesisParams.proofOfStake.epochLength,
      genesisParams.proofOfStake.quarantineLength,
      1
    )
  }
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
