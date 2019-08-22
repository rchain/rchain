package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats._
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.mtl.FunctorTell
import cats.temp.par
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.accounting.{noOpCostLog, _}
import coop.rchain.rholang.interpreter.errors.SetupError
import coop.rchain.rholang.interpreter.storage.ChargingRSpace
import coop.rchain.rspace.{Match, RSpace, _}
import coop.rchain.shared.Log

import scala.concurrent.ExecutionContext

class Runtime[F[_]: Sync] private (
    val reducer: Reduce[F],
    val replayReducer: Reduce[F],
    val space: RhoISpace[F],
    val replaySpace: RhoReplayISpace[F],
    val errorLog: ErrorLog[F],
    val cost: _cost[F],
    val deployParametersRef: Ref[F, DeployParameters],
    val blockData: Ref[F, BlockData],
    val invalidBlocks: Runtime.InvalidBlocks[F]
) {
  def readAndClearErrorVector(): F[Vector[Throwable]] = errorLog.readAndClearErrorVector()
  def close(): F[Unit] =
    for {
      _ <- space.close()
      _ <- replaySpace.close()
    } yield ()
}

object Runtime {

  type RhoTuplespace[F[_]]   = TCPAK[F, Tuplespace]
  type RhoISpace[F[_]]       = TCPAK[F, ISpace]
  type RhoReplayISpace[F[_]] = TCPAK[F, IReplaySpace]

  type RhoDispatch[F[_]]    = Dispatch[F, ListParWithRandom, TaggedContinuation]
  type RhoSysFunction[F[_]] = (Seq[ListParWithRandom], Int) => F[Unit]
  type RhoDispatchMap[F[_]] = Map[Long, RhoSysFunction[F]]

  type CPAK[M[_], F[_[_], _, _, _, _]] =
    F[M, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type TCPAK[M[_], F[_[_], _, _, _, _]] =
    F[
      M,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type BodyRef   = Long

  final case class BlockData(timeStamp: Long, blockNumber: Long)
  object BlockData {
    def empty: BlockData = BlockData(0, 0)
  }

  class InvalidBlocks[F[_]](val invalidBlocks: Ref[F, Par]) {
    def setParams(invalidBlocks: Par): F[Unit] =
      this.invalidBlocks.set(invalidBlocks)
  }

  object InvalidBlocks {
    def apply[F[_]]()(implicit F: Sync[F]): F[InvalidBlocks[F]] =
      for {
        invalidBlocks <- Ref[F].of(Par())
      } yield new InvalidBlocks[F](invalidBlocks)

    def unsafe[F[_]]()(implicit F: Sync[F]): InvalidBlocks[F] =
      new InvalidBlocks(Ref.unsafe[F, Par](Par()))
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
    val GET_BLOCK_DATA: Long               = 23L
    val GET_INVALID_BLOCKS: Long           = 24L
    val REV_ADDRESS: Long                  = 25L
    val DEPLOYER_ID_OPS: Long              = 26L
  }

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  object FixedChannels {
    val STDOUT: Par             = byteName(0)
    val STDOUT_ACK: Par         = byteName(1)
    val STDERR: Par             = byteName(2)
    val STDERR_ACK: Par         = byteName(3)
    val ED25519_VERIFY: Par     = GString("ed25519Verify")
    val SHA256_HASH: Par        = GString("sha256Hash")
    val KECCAK256_HASH: Par     = GString("keccak256Hash")
    val BLAKE2B256_HASH: Par    = GString("blake2b256Hash")
    val SECP256K1_VERIFY: Par   = GString("secp256k1Verify")
    val REG_LOOKUP: Par         = byteName(9)
    val REG_INSERT_RANDOM: Par  = byteName(10)
    val REG_INSERT_SIGNED: Par  = byteName(11)
    val GET_DEPLOY_PARAMS: Par  = byteName(12)
    val GET_BLOCK_DATA: Par     = byteName(13)
    val GET_INVALID_BLOCKS: Par = byteName(14)
    val REV_ADDRESS: Par        = byteName(15)
    val DEPLOYER_ID_OPS: Par    = byteName(16)
  }

  private def introduceSystemProcesses[F[_]: Sync: _cost: Span](
      space: RhoTuplespace[F],
      replaySpace: RhoTuplespace[F],
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
          space.install(channels, patterns, continuation),
          replaySpace.install(channels, patterns, continuation)
        )
    }.sequence

  object SystemProcess {
    final case class Context[F[_]: Concurrent: Span](
        space: RhoTuplespace[F],
        dispatcher: RhoDispatch[F],
        registry: Registry[F],
        deployParametersRef: Ref[F, DeployParameters],
        blockData: Ref[F, BlockData],
        invalidBlocks: InvalidBlocks[F]
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
      "rho:registry:insertSigned:secp256k1",
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
      "rho:block:data",
      FixedChannels.GET_BLOCK_DATA,
      1,
      BodyRefs.GET_BLOCK_DATA, { ctx =>
        ctx.systemProcesses.getBlockData(ctx.blockData)
      }
    ),
    SystemProcess.Definition[F](
      "rho:casper:invalidBlocks",
      FixedChannels.GET_INVALID_BLOCKS,
      1,
      BodyRefs.GET_INVALID_BLOCKS, { ctx =>
        ctx.systemProcesses.invalidBlocks(ctx.invalidBlocks)
      }
    ),
    SystemProcess.Definition[F](
      "rho:rev:address",
      FixedChannels.REV_ADDRESS,
      3,
      BodyRefs.REV_ADDRESS, { ctx =>
        ctx.systemProcesses.revAddress
      }
    ),
    SystemProcess.Definition[F](
      "rho:rchain:deployerId:ops",
      FixedChannels.DEPLOYER_ID_OPS,
      3,
      BodyRefs.DEPLOYER_ID_OPS, { ctx =>
        ctx.systemProcesses.deployerIdOps
      }
    )
  )

  def createWithEmptyCost[F[_]: ContextShift: Concurrent: Log: Metrics: Span: par.Par](
      dataDir: Path,
      mapSize: Long,
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit
      executionContext: ExecutionContext
  ): F[Runtime[F]] = {
    implicit val P = par.Par[F].parallel
    createWithEmptyCost_(dataDir, mapSize, extraSystemProcesses)
  }

  private def createWithEmptyCost_[F[_]: ContextShift: Concurrent: Log: Metrics: Span, M[_]](
      dataDir: Path,
      mapSize: Long,
      extraSystemProcesses: Seq[SystemProcess.Definition[F]]
  )(
      implicit
      P: Parallel[F, M],
      executionContext: ExecutionContext
  ): F[Runtime[F]] =
    for {
      cost <- CostAccounting.emptyCost[F]
      runtime <- {
        implicit val c = cost
        create(dataDir, mapSize, extraSystemProcesses)
      }
    } yield runtime

  def create[F[_]: ContextShift: Concurrent: Log: Metrics: Span, M[_]](
      dataDir: Path,
      mapSize: Long,
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit P: Parallel[F, M],
      executionContext: ExecutionContext,
      cost: _cost[F]
  ): F[Runtime[F]] = {
    val errorLog                               = new ErrorLog[F]()
    implicit val ft: FunctorTell[F, Throwable] = errorLog

    def dispatchTableCreator(
        space: RhoTuplespace[F],
        dispatcher: RhoDispatch[F],
        registry: Registry[F],
        deployParametersRef: Ref[F, DeployParameters],
        blockData: Ref[F, BlockData],
        invalidBlocks: InvalidBlocks[F]
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
              SystemProcess
                .Context(space, dispatcher, registry, deployParametersRef, blockData, invalidBlocks)
            )
          )
    }

    val urnMap: Map[String, Par] = Map[String, Par](
      "rho:registry:lookup" -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true)
    ) ++ (stdSystemProcesses[F] ++ extraSystemProcesses).map(_.toUrnMap)

    val invalidBlocks = InvalidBlocks.unsafe[F]()

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
      setup                <- setupRSpace[F](dataDir, mapSize)
      deployParametersRef  <- Ref.of(DeployParameters.empty)
      blockDataRef         <- Ref.of(BlockData.empty)
      (space, replaySpace) = setup
      (reducer, replayReducer) = {

        val chargingReplaySpace = ChargingRSpace.chargingRSpace[F](replaySpace)
        lazy val replayDispatchTable: RhoDispatchMap[F] =
          dispatchTableCreator(
            chargingReplaySpace,
            replayDispatcher,
            replayRegistry,
            deployParametersRef,
            blockDataRef,
            invalidBlocks
          )

        lazy val (replayDispatcher, replayReducer, replayRegistry) =
          RholangAndScalaDispatcher.create(
            chargingReplaySpace,
            replayDispatchTable,
            urnMap
          )

        val chargingRSpace = ChargingRSpace.chargingRSpace[F](space)
        lazy val dispatchTable: RhoDispatchMap[F] =
          dispatchTableCreator(
            chargingRSpace,
            dispatcher,
            registry,
            deployParametersRef,
            blockDataRef,
            invalidBlocks
          )

        lazy val (dispatcher, reducer, registry) =
          RholangAndScalaDispatcher.create(chargingRSpace, dispatchTable, urnMap)

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
        deployParametersRef,
        blockDataRef,
        invalidBlocks
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
      spaceResult <- space.produce(
                      Registry.registryRoot,
                      ListParWithRandom(Seq(Registry.emptyMap), rand),
                      false,
                      0
                    )
      replayResult <- replaySpace.produce(
                       Registry.registryRoot,
                       ListParWithRandom(Seq(Registry.emptyMap), rand),
                       false,
                       0
                     )
      _ <- spaceResult match {
            case None =>
              replayResult match {
                case None => F.unit
                case Some(_) =>
                  SetupError("Registry insertion in replay fired continuation.").raiseError[F, Unit]
              }
            case Some(_) =>
              SetupError("Registry insertion fired continuation.").raiseError[F, Unit]
          }
    } yield ()
  }

  private def setupRSpace[F[_]: Concurrent: ContextShift: par.Par: Log: Metrics: Span](
      dataDir: Path,
      mapSize: Long
  )(implicit scheduler: ExecutionContext): F[(RhoISpace[F], RhoReplayISpace[F])] = {

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

    def checkCreateDataDir: F[Unit] =
      for {
        notexists <- Sync[F].delay(Files.notExists(dataDir))
        _ <- if (notexists) Sync[F].delay(Files.createDirectories(dataDir)) >> ().pure[F]
            else ().pure[F]
      } yield ()

    checkCreateDataDir >> RSpace.createWithReplay[
      F,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ](dataDir, mapSize)
  }
}
