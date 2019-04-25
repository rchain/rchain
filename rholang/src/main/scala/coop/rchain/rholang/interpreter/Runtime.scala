package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats._
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.mtl.FunctorTell
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Metrics
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.accounting.{noOpCostLog, _}
import coop.rchain.rholang.interpreter.errors.SetupError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.shared.StoreType._
import coop.rchain.shared.{Log, StoreType}

import scala.concurrent.ExecutionContext

class Runtime[F[_]: Sync] private (
    val reducer: ChargingReducer[F],
    val replayReducer: ChargingReducer[F],
    val space: RhoISpace[F],
    val replaySpace: RhoReplayISpace[F],
    val errorLog: ErrorLog[F],
    val cost: _cost[F],
    val context: RhoContext[F],
    val deployParametersRef: Ref[F, DeployParameters],
    val blockTime: Runtime.BlockTime[F]
) {
  def readAndClearErrorVector(): F[Vector[Throwable]] = errorLog.readAndClearErrorVector()
  def close(): F[Unit] =
    for {
      _ <- space.close()
      _ <- replaySpace.close()
    } yield (context.close())
}

object Runtime {

  type RhoISpace[F[_]]       = TCPARK[F, ISpace]
  type RhoPureSpace[F[_]]    = TCPARK[F, PureRSpace]
  type RhoReplayISpace[F[_]] = TCPARK[F, IReplaySpace]

  type RhoIStore[F[_]]  = CPAK[F, IStore]
  type RhoContext[F[_]] = CPAK[F, Context]

  type RhoDispatch[F[_]]    = Dispatch[F, ListParWithRandom, TaggedContinuation]
  type RhoSysFunction[F[_]] = (Seq[ListParWithRandom], Int) => F[Unit]
  type RhoDispatchMap[F[_]] = Map[Long, RhoSysFunction[F]]

  type CPAK[M[_], F[_[_], _, _, _, _]] =
    F[M, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type TCPARK[M[_], F[_[_], _, _, _, _, _]] =
    F[
      M,
      Par,
      BindPattern,
      ListParWithRandom,
      ListParWithRandom,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type BodyRef   = Long

  class BlockTime[F[_]](val timestamp: Ref[F, Par]) {
    def setParams(timestamp: Par)(implicit F: Sync[F]): F[Unit] =
      for {
        _ <- this.timestamp.set(timestamp)
      } yield ()
  }

  object BlockTime {
    def apply[F[_]]()(implicit F: Sync[F]): F[BlockTime[F]] =
      for {
        timestamp <- Ref[F].of(Par())
      } yield new BlockTime[F](timestamp)

    def unsafe[F[_]]()(implicit F: Sync[F]): BlockTime[F] =
      new BlockTime(Ref.unsafe[F, Par](Par()))
  }

  object BodyRefs {
    val STDOUT: Long                       = 0L
    val STDOUT_ACK: Long                   = 1L
    val STDERR: Long                       = 2L
    val STDERR_ACK: Long                   = 3L
    val ED25519_VERIFY: Long               = 4L
    val SHA256_HASH: Long                  = 5L
    val KECCAK256_HASH: Long               = 6L
    val BLAKE2B256_HASH: Long              = 7L
    val SECP256K1_VERIFY: Long             = 9L
    val REG_LOOKUP: Long                   = 10L
    val REG_LOOKUP_CALLBACK: Long          = 11L
    val REG_INSERT: Long                   = 12L
    val REG_INSERT_CALLBACK: Long          = 13L
    val REG_DELETE: Long                   = 14L
    val REG_DELETE_ROOT_CALLBACK: Long     = 15L
    val REG_DELETE_CALLBACK: Long          = 16L
    val REG_PUBLIC_LOOKUP: Long            = 17L
    val REG_PUBLIC_REGISTER_RANDOM: Long   = 18L
    val REG_REGISTER_INSERT_CALLBACK: Long = 19L
    val REG_PUBLIC_REGISTER_SIGNED: Long   = 20L
    val REG_NONCE_INSERT_CALLBACK: Long    = 21L
    val GET_DEPLOY_PARAMS: Long            = 22L
    val GET_TIMESTAMP: Long                = 23L
    val REV_ADDRESS: Long                  = 24L
  }

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  object FixedChannels {
    val STDOUT: Par            = byteName(0)
    val STDOUT_ACK: Par        = byteName(1)
    val STDERR: Par            = byteName(2)
    val STDERR_ACK: Par        = byteName(3)
    val ED25519_VERIFY: Par    = GString("ed25519Verify")
    val SHA256_HASH: Par       = GString("sha256Hash")
    val KECCAK256_HASH: Par    = GString("keccak256Hash")
    val BLAKE2B256_HASH: Par   = GString("blake2b256Hash")
    val SECP256K1_VERIFY: Par  = GString("secp256k1Verify")
    val REG_LOOKUP: Par        = byteName(9)
    val REG_INSERT_RANDOM: Par = byteName(10)
    val REG_INSERT_SIGNED: Par = byteName(11)
    val GET_DEPLOY_PARAMS: Par = byteName(12)
    val GET_TIMESTAMP: Par     = byteName(13)
    val REV_ADDRESS: Par       = byteName(14)
  }

  private def introduceSystemProcesses[F[_]: Sync: _cost](
      space: RhoISpace[F],
      replaySpace: RhoISpace[F],
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
        List(
          space.install(channels, patterns, continuation)(matchListPar),
          replaySpace.install(channels, patterns, continuation)(matchListPar)
        )
    }.sequence

  object SystemProcess {
    final case class Context[F[_]: Concurrent](
        space: RhoISpace[F],
        dispatcher: RhoDispatch[F],
        registry: Registry[F],
        deployParametersRef: Ref[F, DeployParameters],
        blockTime: BlockTime[F]
    ) {
      val systemProcesses = SystemProcesses[F](dispatcher, space)
    }

    final case class Definition[F[_]](
        urn: String,
        fixedChannel: Name,
        arity: Arity,
        bodyRef: BodyRef,
        handler: Context[F] => (Seq[ListParWithRandom], Int) => F[Unit],
        remainder: Remainder = None
    ) {
      def toDispatchTable(
          context: SystemProcess.Context[F]
      ): (BodyRef, (Seq[ListParWithRandom], Arity) => F[Unit]) =
        bodyRef -> handler(context)

      def toUrnMap: (String, Par) = {
        val bundle: Par = Bundle(fixedChannel, writeFlag = true)
        urn -> bundle
      }

      def toProcDefs: (Name, Arity, Remainder, BodyRef) =
        (fixedChannel, arity, remainder, bodyRef)
    }
  }

  def stdSystemProcesses[F[_]]: Seq[SystemProcess.Definition[F]] = Seq(
    SystemProcess.Definition[F]("rho:io:stdout", FixedChannels.STDOUT, 1, BodyRefs.STDOUT, {
      ctx: SystemProcess.Context[F] =>
        ctx.systemProcesses.stdOut
    }),
    SystemProcess
      .Definition[F]("rho:io:stdoutAck", FixedChannels.STDOUT_ACK, 2, BodyRefs.STDOUT_ACK, {
        ctx: SystemProcess.Context[F] =>
          ctx.systemProcesses.stdOutAck
      }),
    SystemProcess.Definition[F]("rho:io:stderr", FixedChannels.STDERR, 1, BodyRefs.STDERR, {
      ctx: SystemProcess.Context[F] =>
        ctx.systemProcesses.stdErr
    }),
    SystemProcess
      .Definition[F]("rho:io:stderrAck", FixedChannels.STDERR_ACK, 2, BodyRefs.STDERR_ACK, {
        ctx: SystemProcess.Context[F] =>
          ctx.systemProcesses.stdErrAck
      }),
    SystemProcess.Definition[F](
      "rho:registry:insertArbitrary",
      FixedChannels.REG_INSERT_RANDOM,
      2,
      BodyRefs.REG_PUBLIC_REGISTER_RANDOM, { ctx =>
        ctx.registry.publicRegisterRandom
      }
    ),
    SystemProcess.Definition[F](
      "rho:registry:insertSigned:ed25519",
      FixedChannels.REG_INSERT_SIGNED,
      4,
      BodyRefs.REG_PUBLIC_REGISTER_SIGNED, { ctx =>
        ctx.registry.publicRegisterSigned
      }
    ),
    SystemProcess.Definition[F](
      "rho:deploy:params",
      FixedChannels.GET_DEPLOY_PARAMS,
      1,
      BodyRefs.GET_DEPLOY_PARAMS, { ctx =>
        ctx.systemProcesses.getDeployParams(ctx.deployParametersRef)
      }
    ),
    SystemProcess.Definition[F](
      "rho:block:timestamp",
      FixedChannels.GET_TIMESTAMP,
      1,
      BodyRefs.GET_TIMESTAMP, { ctx =>
        ctx.systemProcesses.blockTime(ctx.blockTime)
      }
    ),
    SystemProcess.Definition[F](
      "rho:rev:address",
      FixedChannels.REV_ADDRESS,
      3,
      BodyRefs.REV_ADDRESS, { ctx =>
        ctx.systemProcesses.validateRevAddress
      }
    )
  )

  def createWithEmptyCost[F[_]: ContextShift: Concurrent: Log: Metrics, M[_]](
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType,
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit
      P: Parallel[F, M],
      executionContext: ExecutionContext
  ): F[Runtime[F]] =
    (for {
      cost <- CostAccounting.emptyCost[F]
      runtime <- {
        implicit val c = cost
        create(dataDir, mapSize, storeType, extraSystemProcesses)
      }
    } yield (runtime))

  def create[F[_]: ContextShift: Concurrent: Log: Metrics, M[_]](
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType,
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit P: Parallel[F, M],
      executionContext: ExecutionContext,
      cost: _cost[F]
  ): F[Runtime[F]] = {
    val errorLog                               = new ErrorLog[F]()
    implicit val ft: FunctorTell[F, Throwable] = errorLog

    def dispatchTableCreator(
        space: RhoISpace[F],
        dispatcher: RhoDispatch[F],
        registry: Registry[F],
        deployParametersRef: Ref[F, DeployParameters],
        blockTime: BlockTime[F]
    ): RhoDispatchMap[F] = {
      val systemProcesses = SystemProcesses[F](dispatcher, space)
      import BodyRefs._
      Map(
        ED25519_VERIFY               -> systemProcesses.ed25519Verify,
        SHA256_HASH                  -> systemProcesses.sha256Hash,
        KECCAK256_HASH               -> systemProcesses.keccak256Hash,
        BLAKE2B256_HASH              -> systemProcesses.blake2b256Hash,
        SECP256K1_VERIFY             -> systemProcesses.secp256k1Verify,
        REG_LOOKUP                   -> (registry.lookup(_, _)),
        REG_LOOKUP_CALLBACK          -> (registry.lookupCallback(_, _)),
        REG_INSERT                   -> (registry.insert(_, _)),
        REG_INSERT_CALLBACK          -> (registry.insertCallback(_, _)),
        REG_REGISTER_INSERT_CALLBACK -> (registry.registerInsertCallback(_, _)),
        REG_DELETE                   -> (registry.delete(_, _)),
        REG_DELETE_ROOT_CALLBACK     -> (registry.deleteRootCallback(_, _)),
        REG_DELETE_CALLBACK          -> (registry.deleteCallback(_, _)),
        REG_PUBLIC_LOOKUP            -> (registry.publicLookup(_, _)),
        REG_NONCE_INSERT_CALLBACK    -> (registry.nonceInsertCallback(_, _))
      ) ++
        (stdSystemProcesses[F] ++ extraSystemProcesses)
          .map(
            _.toDispatchTable(
              SystemProcess.Context(space, dispatcher, registry, deployParametersRef, blockTime)
            )
          )
    }

    val urnMap: Map[String, Par] = Map[String, Par](
      "rho:registry:lookup" -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true)
    ) ++ (stdSystemProcesses[F] ++ extraSystemProcesses).map(_.toUrnMap)

    val blockTime = BlockTime.unsafe[F]()

    val procDefs: List[(Name, Arity, Remainder, BodyRef)] = {
      import BodyRefs._
      List(
        (FixedChannels.ED25519_VERIFY, 4, None, ED25519_VERIFY),
        (FixedChannels.SHA256_HASH, 2, None, SHA256_HASH),
        (FixedChannels.KECCAK256_HASH, 2, None, KECCAK256_HASH),
        (FixedChannels.BLAKE2B256_HASH, 2, None, BLAKE2B256_HASH),
        (FixedChannels.SECP256K1_VERIFY, 4, None, SECP256K1_VERIFY),
        (FixedChannels.REG_LOOKUP, 2, None, REG_PUBLIC_LOOKUP)
      ) ++ (stdSystemProcesses[F] ++ extraSystemProcesses).map(_.toProcDefs)
    }

    for {
      setup                         <- setupRSpace[F](dataDir, mapSize, storeType)
      deployParametersRef           <- Ref.of(DeployParameters.empty)
      (context, space, replaySpace) = setup
      (reducer, replayReducer) = {
        lazy val replayDispatchTable: RhoDispatchMap[F] =
          dispatchTableCreator(
            replaySpace,
            replayDispatcher,
            replayRegistry,
            deployParametersRef,
            blockTime
          )

        lazy val dispatchTable: RhoDispatchMap[F] =
          dispatchTableCreator(space, dispatcher, registry, deployParametersRef, blockTime)

        lazy val (dispatcher, reducer, registry) =
          RholangAndScalaDispatcher.create(space, dispatchTable, urnMap)

        lazy val (replayDispatcher, replayReducer, replayRegistry) =
          RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable, urnMap)
        (reducer, replayReducer)
      }
      res <- introduceSystemProcesses(space, replaySpace, procDefs)
    } yield {
      assert(res.forall(_.isEmpty))
      new Runtime[F](
        reducer,
        replayReducer,
        space,
        replaySpace,
        errorLog,
        cost,
        context,
        deployParametersRef,
        blockTime
      )
    }
  }

  def injectEmptyRegistryRoot[F[_]](space: RhoISpace[F], replaySpace: RhoReplayISpace[F])(
      implicit F: Concurrent[F]
  ): F[Unit] = {
    // This random value stays dead in the tuplespace, so we can have some fun.
    // This is from Jeremy Bentham's "Defence of Usury"
    val rand = Blake2b512Random(
      ("there can be no such thing as usury: " +
        "for what rate of interest is there that can naturally be more proper than another?")
        .getBytes()
    )

    for {
      cost <- CostAccounting.initialCost[F](Cost.UNSAFE_MAX)
      spaceResult <- space.produce(
                      Registry.registryRoot,
                      ListParWithRandom(Seq(Registry.emptyMap), rand),
                      false,
                      0
                    )(matchListPar(F, cost))
      replayResult <- replaySpace.produce(
                       Registry.registryRoot,
                       ListParWithRandom(Seq(Registry.emptyMap), rand),
                       false,
                       0
                     )(matchListPar(F, cost))
      _ <- spaceResult match {
            case None =>
              replayResult match {
                case None => F.unit
                case Some(_) =>
                  F.raiseError(
                    new SetupError("Registry insertion in replay fired continuation.")
                  )
              }
            case Some(_) =>
              F.raiseError(new SetupError("Registry insertion fired continuation."))
          }
    } yield ()
  }

  def setupRSpace[F[_]: Concurrent: ContextShift: Log: Metrics](
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType
  )(implicit scheduler: ExecutionContext): F[(RhoContext[F], RhoISpace[F], RhoReplayISpace[F])] = {
    def createSpace(
        context: RhoContext[F]
    ): F[(RhoContext[F], RhoISpace[F], RhoReplayISpace[F])] =
      for {
        space <- RSpace.create[
                  F,
                  Par,
                  BindPattern,
                  ListParWithRandom,
                  ListParWithRandom,
                  TaggedContinuation
                ](context, Branch.MASTER)
        replaySpace <- ReplayRSpace.create[
                        F,
                        Par,
                        BindPattern,
                        ListParWithRandom,
                        ListParWithRandom,
                        TaggedContinuation
                      ](context, Branch.REPLAY)
      } yield ((context, space, replaySpace))

    def checkCreateDataDir: F[Unit] =
      for {
        notexists <- Sync[F].delay(Files.notExists(dataDir))
        _         <- if (notexists) Sync[F].delay(Files.createDirectories(dataDir)) else ().pure[F]
      } yield ()

    storeType match {
      case InMem =>
        createSpace(Context.createInMemory())
      case LMDB =>
        checkCreateDataDir >> createSpace(Context.create(dataDir, mapSize, true))
      case Mixed =>
        checkCreateDataDir >> createSpace(Context.createMixed(dataDir, mapSize))
    }
  }
}
