package coop.rchain.rholang.interpreter

import cats.data.Chain
import cats.effect._
import cats.mtl.FunctorTell
import cats.syntax.all._
import cats.{Monad, Parallel}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoISpace, RhoReplayISpace}
import coop.rchain.rholang.interpreter.RholangAndScalaDispatcher.RhoDispatch
import coop.rchain.rholang.interpreter.SystemProcesses._
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost, CostAccounting, HasCost}
import coop.rchain.rholang.interpreter.registry.RegistryBootstrap
import coop.rchain.rholang.interpreter.storage.ChargingRSpace
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.util.unpackOption
import coop.rchain.rspace.{Match, _}
import coop.rchain.shared.Log

import scala.concurrent.ExecutionContext
import cats.effect.Ref

trait RhoRuntime[F[_]] extends HasCost[F] {

  /**
    * Parse the rholang term into [[coop.rchain.models.Par]] and execute it with provided initial phlo.
    *
    * This function would change the state in the runtime.
    * @param term The rholang contract which would run on the runtime
    * @param initialPhlo initial cost for the this evaluation. If the phlo is not enough,
    *                    [[coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError]] would return.
    * @param normalizerEnv additional env for Par when parsing term into Par
    * @param rand random seed for rholang execution
    * @return
    */
  def evaluate(
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par],
      rand: Blake2b512Random
  ): F[EvaluateResult]

  /**
    * The function would execute the par regardless setting cost which would possibly cause
    * [[coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError]]. Because of that, use this
    * function in some situation which is not cost sensitive.
    *
    * This function would change the state in the runtime.
    *
    * Ideally, this function should be removed or hack the runtime without cost accounting in the future .
    * @param par [[coop.rchain.models.Par]] for the execution
    * @param env additional env for execution
    * @param rand random seed for rholang execution
    * @return
    */
  def inj(par: Par, env: Env[Par] = Env[Par]())(
      implicit rand: Blake2b512Random
  ): F[Unit]

  /**
    * After some executions([[evaluate]]) on the runtime, you can create a soft checkpoint which is the changes
    * for the current state of the runtime. You can revert the changes by [[revertToSoftCheckpoint]]
    * @return
    */
  def createSoftCheckpoint
      : F[SoftCheckpoint[Par, BindPattern, ListParWithRandom, TaggedContinuation]]

  def revertToSoftCheckpoint(
      softCheckpoint: SoftCheckpoint[Par, BindPattern, ListParWithRandom, TaggedContinuation]
  ): F[Unit]

  /**
    * Create a checkpoint for the runtime. All the changes which happened in the runtime would persistent in the disk
    * and result in a new stateHash for the new state.
    * @return
    */
  def createCheckpoint: F[Checkpoint]

  /**
    * Reset the runtime to the specific state. Then you can operate some execution on the state.
    * @param root the target state hash to reset
    * @return
    */
  def reset(root: Blake2b256Hash): F[Unit]

  /**
    * Consume the result in the rspace.
    *
    * This function would change the state in the runtime.
    * @param channel target channel for the consume
    * @param pattern pattern for the consume
    * @return
    */
  def consumeResult(
      channel: Seq[Par],
      pattern: Seq[BindPattern]
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]]

  /**
    * get data directly from history repository
    *
    * This function would not change the state in the runtime
    */
  def getData(channel: Par): F[Seq[Datum[ListParWithRandom]]]

  def getJoins(channel: Par): F[Seq[Seq[Par]]]

  /**
    * get data directly from history repository
    *
    * This function would not change the state in the runtime
    */
  def getContinuation(
      channels: Seq[Par]
  ): F[Seq[WaitingContinuation[BindPattern, TaggedContinuation]]]

  /**
    * Set the runtime block data environment.
    */
  def setBlockData(blockData: BlockData): F[Unit]

  /**
    * Get the hot changes after some executions for the runtime.
    * Currently this is only for debug info mostly.
    */
  def getHotChanges: F[Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]]
}

trait ReplayRhoRuntime[F[_]] extends RhoRuntime[F] {
  def rig(log: trace.Log): F[Unit]

  def checkReplayData: F[Unit]
}

class RhoRuntimeImpl[F[_]: Sync: Span](
    val reducer: Reduce[F],
    val space: RhoISpace[F],
    val cost: _cost[F],
    val blockDataRef: Ref[F, BlockData],
    val mergeChs: Ref[F, Set[Par]]
) extends RhoRuntime[F] {
  private val emptyContinuation = TaggedContinuation()

  override def getHotChanges
      : F[Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]] = space.toMap

  override def inj(par: Par, env: Env[Par] = Env[Par]())(implicit rand: Blake2b512Random): F[Unit] =
    reducer.inj(par)

  override def consumeResult(
      channel: Seq[Par],
      pattern: Seq[BindPattern]
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
    space.consume(channel, pattern, emptyContinuation, persist = false).map(unpackOption)

  override def evaluate(
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Name],
      rand: Blake2b512Random
  ): F[EvaluateResult] = {
    implicit val c: _cost[F]       = cost
    implicit val m                 = mergeChs
    implicit val i: Interpreter[F] = Interpreter.newIntrepreter[F]
    Interpreter[F].injAttempt(
      reducer,
      term,
      initialPhlo,
      normalizerEnv
    )(rand)
  }

  override def reset(root: Blake2b256Hash): F[Unit] = space.reset(root)

  override def createCheckpoint: F[Checkpoint] = Span[F].withMarks("create-checkpoint") {
    space.createCheckpoint()
  }

  override def createSoftCheckpoint
      : F[SoftCheckpoint[Par, BindPattern, ListParWithRandom, TaggedContinuation]] =
    Span[F].withMarks("create-soft-heckpoint") {
      space.createSoftCheckpoint()
    }

  override def revertToSoftCheckpoint(
      softCheckpoint: SoftCheckpoint[Name, BindPattern, ListParWithRandom, TaggedContinuation]
  ): F[Unit] = space.revertToSoftCheckpoint(softCheckpoint)

  override def getData(channel: Par): F[Seq[Datum[ListParWithRandom]]] = space.getData(channel)

  override def getContinuation(
      channels: Seq[Name]
  ): F[Seq[WaitingContinuation[BindPattern, TaggedContinuation]]] =
    space.getWaitingContinuations(channels)

  override def getJoins(channel: Name): F[Seq[Seq[Name]]] = space.getJoins(channel)

  override def setBlockData(blockData: BlockData): F[Unit] = blockDataRef.set(blockData)
}

class ReplayRhoRuntimeImpl[F[_]: Sync: Span](
    override val reducer: Reduce[F],
    override val space: RhoReplayISpace[F],
    override val cost: _cost[F],
    // TODO: Runtime must be immutable. Block data and invalid blocks should be supplied when Runtime is created.
    //  This also means to unify all special names necessary to spawn a new Runtime.
    override val blockDataRef: Ref[F, BlockData],
    override val mergeChs: Ref[F, Set[Par]]
) extends RhoRuntimeImpl[F](reducer, space, cost, blockDataRef, mergeChs)
    with ReplayRhoRuntime[F] {
  override def checkReplayData: F[Unit] = space.checkReplayData()

  override def rig(log: trace.Log): F[Unit] = space.rig(log)
}

object ReplayRhoRuntime {
  def apply[F[_]: Sync: Span](
      reducer: Reduce[F],
      space: RhoReplayISpace[F],
      cost: _cost[F],
      blockDataRef: Ref[F, BlockData],
      mergeChs: Ref[F, Set[Par]]
  ) = new ReplayRhoRuntimeImpl[F](reducer, space, cost, blockDataRef, mergeChs)
}

object RhoRuntime {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(RholangMetricsSource, "runtime")
  private[this] val createReplayRuntime     = Metrics.Source(RuntimeMetricsSource, "create-replay")
  private[this] val createPlayRuntime       = Metrics.Source(RuntimeMetricsSource, "create-play")

  def apply[F[_]: Sync: Span](
      reducer: Reduce[F],
      space: RhoISpace[F],
      cost: _cost[F],
      blockDataRef: Ref[F, BlockData],
      mergeChs: Ref[F, Set[Par]]
  ) = new RhoRuntimeImpl[F](reducer, space, cost, blockDataRef, mergeChs)

  type RhoTuplespace[F[_]]   = TCPAK[F, Tuplespace]
  type RhoISpace[F[_]]       = TCPAK[F, ISpace]
  type RhoReplayISpace[F[_]] = TCPAK[F, IReplaySpace]
  type ISpaceAndReplay[F[_]] = (RhoISpace[F], RhoReplayISpace[F])

  type RhoHistoryRepository[F[_]] =
    HistoryRepository[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type TCPAK[M[_], F[_[_], _, _, _, _]] =
    F[M, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  def introduceSystemProcesses[F[_]: Sync: _cost: Span](
      spaces: List[RhoTuplespace[F]],
      processes: List[(Name, Arity, Remainder, BodyRef)]
  ): F[List[Option[(TaggedContinuation, Seq[ListParWithRandom])]]] =
    processes.flatMap {
      case (name, arity, remainder, ref) =>
        val channels = List(name)
        val patterns = List(
          BindPattern(
            (0 until arity).map[Par, Seq[Par]](i => EVar(FreeVar(i))),
            remainder,
            freeCount = arity
          )
        )
        val continuation = TaggedContinuation(ScalaBodyRef(ref))
        spaces.map(_.install(channels, patterns, continuation))
    }.sequence

  def stdSystemProcesses[F[_]]: Seq[Definition[F]] = Seq(
    Definition[F]("rho:io:stdout", FixedChannels.STDOUT, 1, BodyRefs.STDOUT, {
      ctx: ProcessContext[F] =>
        ctx.systemProcesses.stdOut
    }),
    Definition[F]("rho:io:stdoutAck", FixedChannels.STDOUT_ACK, 2, BodyRefs.STDOUT_ACK, {
      ctx: ProcessContext[F] =>
        ctx.systemProcesses.stdOutAck
    }),
    Definition[F]("rho:io:stderr", FixedChannels.STDERR, 1, BodyRefs.STDERR, {
      ctx: ProcessContext[F] =>
        ctx.systemProcesses.stdErr
    }),
    Definition[F]("rho:io:stderrAck", FixedChannels.STDERR_ACK, 2, BodyRefs.STDERR_ACK, {
      ctx: ProcessContext[F] =>
        ctx.systemProcesses.stdErrAck
    }),
    Definition[F](
      "rho:block:data",
      FixedChannels.GET_BLOCK_DATA,
      1,
      BodyRefs.GET_BLOCK_DATA, { ctx =>
        ctx.systemProcesses.getBlockData(ctx.blockData)
      }
    ),
    Definition[F](
      "rho:rev:address",
      FixedChannels.REV_ADDRESS,
      3,
      BodyRefs.REV_ADDRESS, { ctx =>
        ctx.systemProcesses.revAddress
      }
    ),
    Definition[F](
      "rho:rchain:deployerId:ops",
      FixedChannels.DEPLOYER_ID_OPS,
      3,
      BodyRefs.DEPLOYER_ID_OPS, { ctx =>
        ctx.systemProcesses.deployerIdOps
      }
    ),
    Definition[F](
      "rho:registry:ops",
      FixedChannels.REG_OPS,
      3,
      BodyRefs.REG_OPS, { ctx =>
        ctx.systemProcesses.registryOps
      }
    ),
    Definition[F](
      "sys:authToken:ops",
      FixedChannels.SYS_AUTHTOKEN_OPS,
      3,
      BodyRefs.SYS_AUTHTOKEN_OPS, { ctx =>
        ctx.systemProcesses.sysAuthTokenOps
      }
    )
  )

  def stdRhoCryptoProcesses[F[_]]: Seq[Definition[F]] = Seq(
    Definition[F](
      "rho:crypto:secp256k1Verify",
      FixedChannels.SECP256K1_VERIFY,
      4,
      BodyRefs.SECP256K1_VERIFY, { ctx =>
        ctx.systemProcesses.secp256k1Verify
      }
    ),
    Definition[F](
      "rho:crypto:blake2b256Hash",
      FixedChannels.BLAKE2B256_HASH,
      2,
      BodyRefs.BLAKE2B256_HASH, { ctx =>
        ctx.systemProcesses.blake2b256Hash
      }
    ),
    Definition[F](
      "rho:crypto:keccak256Hash",
      FixedChannels.KECCAK256_HASH,
      2,
      BodyRefs.KECCAK256_HASH, { ctx =>
        ctx.systemProcesses.keccak256Hash
      }
    ),
    Definition[F](
      "rho:crypto:sha256Hash",
      FixedChannels.SHA256_HASH,
      2,
      BodyRefs.SHA256_HASH, { ctx =>
        ctx.systemProcesses.sha256Hash
      }
    ),
    Definition[F](
      "rho:crypto:ed25519Verify",
      FixedChannels.ED25519_VERIFY,
      4,
      BodyRefs.ED25519_VERIFY, { ctx =>
        ctx.systemProcesses.ed25519Verify
      }
    )
  )

  def dispatchTableCreator[F[_]: Async: Span](
      space: RhoTuplespace[F],
      dispatcher: RhoDispatch[F],
      blockData: Ref[F, BlockData],
      extraSystemProcesses: Seq[Definition[F]]
  ): RhoDispatchMap[F] =
    (stdSystemProcesses[F] ++ stdRhoCryptoProcesses[F] ++ extraSystemProcesses)
      .map(_.toDispatchTable(ProcessContext(space, dispatcher, blockData)))
      .toMap

  val basicProcesses: Map[String, Par] = Map[String, Par](
    "rho:registry:lookup"          -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true),
    "rho:registry:insertArbitrary" -> Bundle(FixedChannels.REG_INSERT_RANDOM, writeFlag = true),
    "rho:registry:insertSigned:secp256k1" -> Bundle(
      FixedChannels.REG_INSERT_SIGNED,
      writeFlag = true
    )
  )

  def setupReducer[F[_]: Async: Parallel: _cost: Log: Metrics: Span](
      chargingRSpace: RhoTuplespace[F],
      blockDataRef: Ref[F, BlockData],
      extraSystemProcesses: Seq[Definition[F]],
      urnMap: Map[String, Par],
      mergeChs: Ref[F, Set[Par]],
      mergeableTagName: Par
  ): Reduce[F] = {
    lazy val replayDispatchTable: RhoDispatchMap[F] =
      dispatchTableCreator(chargingRSpace, replayDispatcher, blockDataRef, extraSystemProcesses)

    lazy val (replayDispatcher, replayReducer) =
      RholangAndScalaDispatcher(
        chargingRSpace,
        replayDispatchTable,
        urnMap,
        mergeChs,
        mergeableTagName
      )
    replayReducer
  }

  def setupMapsAndRefs[F[_]: Async](
      extraSystemProcesses: Seq[Definition[F]] = Seq.empty
  ): F[
    (Ref[F, BlockData], Map[String, Name], Seq[(Name, Arity, Remainder, BodyRef)])
  ] =
    for {
      blockDataRef <- Ref.of(BlockData.empty)
      urnMap = basicProcesses ++ (stdSystemProcesses[F] ++ stdRhoCryptoProcesses[F] ++ extraSystemProcesses)
        .map(_.toUrnMap)
      procDefs = (stdSystemProcesses[F] ++ stdRhoCryptoProcesses[F] ++ extraSystemProcesses)
        .map(_.toProcDefs)
    } yield (blockDataRef, urnMap, procDefs)

  def createRhoEnv[F[_]: Async: Parallel: _cost: Log: Metrics: Span](
      rspace: RhoISpace[F],
      mergeChs: Ref[F, Set[Par]],
      mergeableTagName: Par,
      extraSystemProcesses: Seq[Definition[F]] = Seq.empty
  ): F[(Reduce[F], Ref[F, BlockData])] =
    for {
      mapsAndRefs                      <- setupMapsAndRefs(extraSystemProcesses)
      (blockDataRef, urnMap, procDefs) = mapsAndRefs
      reducer = setupReducer(
        ChargingRSpace.chargingRSpace[F](rspace),
        blockDataRef,
        extraSystemProcesses,
        urnMap,
        mergeChs,
        mergeableTagName
      )
      res <- introduceSystemProcesses(rspace :: Nil, procDefs.toList)
      _   = assert(res.forall(_.isEmpty))
    } yield (reducer, blockDataRef)

  // This is from Nassim Taleb's "Skin in the Game"
  val bootstrapRand: Blake2b512Random = Blake2b512Random(
    ("Decentralization is based on the simple notion that it is easier to macrobull***t than microbull***t. " +
      "Decentralization reduces large structural asymmetries.")
      .getBytes()
  )

  def bootstrapRegistry[F[_]: Monad](runtime: RhoRuntime[F]): F[Unit] = {
    implicit val rand: Blake2b512Random = bootstrapRand
    for {
      cost <- runtime.cost.get
      _    <- runtime.cost.set(Cost.UNSAFE_MAX)
      _    <- runtime.inj(RegistryBootstrap.AST)
      _    <- runtime.cost.set(cost)
    } yield ()
  }

  private def createRuntime[F[_]: Async: Log: Metrics: Span: Parallel](
      rspace: RhoISpace[F],
      extraSystemProcesses: Seq[Definition[F]],
      initRegistry: Boolean,
      mergeableTagName: Par
  )(implicit costLog: FunctorTell[F, Chain[Cost]]): F[RhoRuntime[F]] =
    Span[F].trace(createPlayRuntime) {
      for {
        cost     <- CostAccounting.emptyCost[F]
        mergeChs <- Ref.of(Set[Par]())
        rhoEnv <- {
          implicit val c: _cost[F] = cost
          createRhoEnv(rspace, mergeChs, mergeableTagName, extraSystemProcesses)
        }
        (reducer, blockRef) = rhoEnv
        runtime             = new RhoRuntimeImpl[F](reducer, rspace, cost, blockRef, mergeChs)
        _ <- if (initRegistry) {
              bootstrapRegistry(runtime) >> runtime.createCheckpoint
            } else ().pure[F]
      } yield runtime
    }

  /**
    *
    * @param rspace the rspace which the runtime would operate on it
    * @param extraSystemProcesses extra system rholang processes exposed to the runtime
    *                             which you can execute function on it
    * @param initRegistry For a newly created rspace, you might need to bootstrap registry
    *                     in the runtime to use rholang registry normally. Actually this initRegistry
    *                     is not the only thing you need for rholang registry, after the bootstrap
    *                     registry, you still need to insert registry contract on the rspace.
    *                     For a exist rspace which bootstrap registry before, you can skip this.
    *                     For some test cases, you don't need the registry then you can skip this
    *                     init process which can be faster.
    * @param costLog currently only the testcases needs a special costLog for test information.
    *                Normally you can just
    *                use [[coop.rchain.rholang.interpreter.accounting.noOpCostLog]]
    * @return
    */
  def createRhoRuntime[F[_]: Async: Log: Metrics: Span: Parallel](
      rspace: RhoISpace[F],
      mergeableTagName: Par,
      initRegistry: Boolean = true,
      extraSystemProcesses: Seq[Definition[F]] = Seq.empty
  )(implicit costLog: FunctorTell[F, Chain[Cost]]): F[RhoRuntime[F]] =
    createRuntime[F](rspace, extraSystemProcesses, initRegistry, mergeableTagName)

  /**
    *
    * @param rspace the replay rspace which the runtime operate on it
    * @param extraSystemProcesses same as [[coop.rchain.rholang.interpreter.RhoRuntime.createRhoRuntime]]
    * @param initRegistry same as [[coop.rchain.rholang.interpreter.RhoRuntime.createRhoRuntime]]
    * @param costLog same as [[coop.rchain.rholang.interpreter.RhoRuntime.createRhoRuntime]]
    * @return
    */
  def createReplayRhoRuntime[F[_]: Async: Log: Metrics: Span: Parallel](
      rspace: RhoReplayISpace[F],
      mergeableTagName: Par,
      extraSystemProcesses: Seq[Definition[F]] = Seq.empty,
      initRegistry: Boolean = true
  )(implicit costLog: FunctorTell[F, Chain[Cost]]): F[ReplayRhoRuntime[F]] =
    Span[F].trace(createReplayRuntime) {
      for {
        cost     <- CostAccounting.emptyCost[F]
        mergeChs <- Ref.of(Set[Par]())
        rhoEnv <- {
          implicit val c: _cost[F] = cost
          createRhoEnv(rspace, mergeChs, mergeableTagName, extraSystemProcesses)
        }
        (reducer, blockRef) = rhoEnv
        runtime = new ReplayRhoRuntimeImpl[F](
          reducer,
          rspace,
          cost,
          blockRef,
          mergeChs
        )
        _ <- if (initRegistry) {
              bootstrapRegistry(runtime) >> runtime.createCheckpoint
            } else ().pure[F]
      } yield runtime
    }

  def createRuntimes[F[_]: Async: Parallel: Log: Metrics: Span](
      space: RhoISpace[F],
      replaySpace: RhoReplayISpace[F],
      initRegistry: Boolean,
      additionalSystemProcesses: Seq[Definition[F]],
      mergeableTagName: Par
  ): F[(RhoRuntime[F], ReplayRhoRuntime[F])] =
    for {
      rhoRuntime <- RhoRuntime.createRhoRuntime[F](
                     space,
                     mergeableTagName,
                     initRegistry,
                     additionalSystemProcesses
                   )
      replayRhoRuntime <- RhoRuntime.createReplayRhoRuntime[F](
                           replaySpace,
                           mergeableTagName,
                           additionalSystemProcesses,
                           initRegistry
                         )
    } yield (rhoRuntime, replayRhoRuntime)

  /*
   * Create from KeyValueStore's
   */

  def createRuntime[F[_]: Async: Parallel: Log: Metrics: Span](
      stores: RSpaceStore[F],
      mergeableTagName: Par,
      initRegistry: Boolean = false,
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty
  ): F[RhoRuntime[F]] = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    for {
      space <- RSpace
                .create[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                  stores
                )
      runtime <- createRhoRuntime[F](
                  space,
                  mergeableTagName,
                  initRegistry,
                  additionalSystemProcesses
                )
    } yield runtime
  }
}
