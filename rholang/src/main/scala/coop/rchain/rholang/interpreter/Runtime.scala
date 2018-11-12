package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl.FunctorTell
import cats.{Applicative, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.ShortLeashParams.ShortLeashParameters
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.{OutOfPhlogistonsError, SetupError}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.{Match, _}
import coop.rchain.shared.StoreType
import coop.rchain.shared.StoreType._

import scala.collection.immutable

class Runtime[F[_]: Sync] private (
    val reducer: ChargingReducer[F],
    val replayReducer: ChargingReducer[F],
    val space: RhoISpace[F],
    val replaySpace: RhoReplayISpace[F],
    val errorLog: ErrorLog[F],
    val context: RhoContext,
    val shortLeashParams: Runtime.ShortLeashParams[F],
    val blockTime: Runtime.BlockTime[F]
) {
  def readAndClearErrorVector(): F[Vector[Throwable]] = errorLog.readAndClearErrorVector()
  def close(): F[Unit] =
    for {
      _ <- space.close()
      _ <- replaySpace.close()
    } yield context.close()
}

object Runtime {

  type RhoISpace[F[_]]       = TCPARK[F, ISpace]
  type RhoPureSpace[F[_]]    = TCPARK[F, PureRSpace]
  type RhoReplayISpace[F[_]] = TCPARK[F, IReplaySpace]

  type RhoIStore  = CPAK[IStore]
  type RhoContext = CPAK[Context]

  type RhoDispatch[F[_]]    = Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  type RhoSysFunction[F[_]] = Seq[ListParWithRandomAndPhlos] => F[Unit]
  type RhoDispatchMap[F[_]] = Map[Long, RhoSysFunction[F]]

  type CPAK[F[_, _, _, _]] =
    F[Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type CPARK[F[_, _, _, _, _, _]] =
    F[
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ]

  type TCPARK[M[_], F[_[_], _, _, _, _, _, _]] =
    F[
      M,
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type BodyRef   = Long

  class ShortLeashParams[F[_]] private (private val params: Ref[F, ShortLeashParameters]) {
    def setParams(codeHash: Par, phloRate: Par, userId: Par, timestamp: Par): F[Unit] =
      params.set(ShortLeashParameters(codeHash, phloRate, userId, timestamp))

    def getParams: F[ShortLeashParameters] = params.get
  }

  object ShortLeashParams {
    final case class ShortLeashParameters(codeHash: Par, phloRate: Par, userId: Par, timestamp: Par)
    object ShortLeashParameters {
      val empty: ShortLeashParameters = ShortLeashParameters(Par(), Par(), Par(), Par())
    }
    def apply[F[_]: Sync]: F[ShortLeashParams[F]] =
      Ref
        .of[F, Runtime.ShortLeashParams.ShortLeashParameters](ShortLeashParameters.empty)
        .map(new ShortLeashParams[F](_))
  }

  class BlockTime[F[_]](val timestamp: Ref[F, Par]) {
    def setParams(timestamp: Par)(implicit F: Sync[F]): F[Unit] =
      for {
        _ <- this.timestamp.set(timestamp)
      } yield ()
  }

  object BlockTime {
    def apply[F[_]]()(implicit F: Sync[F]): F[BlockTime[F]] =
      Ref[F].of(Par()).map(new BlockTime[F](_))
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
  }

  // because only we do installs
  private val MATCH_UNLIMITED_PHLOS = matchListPar(Cost(Integer.MAX_VALUE))

  private def introduceSystemProcesses[F[_]: Applicative](
      space: RhoISpace[F],
      replaySpace: RhoISpace[F],
      processes: List[(Name, Arity, Remainder, BodyRef)]
  ): F[List[Option[(TaggedContinuation, immutable.Seq[ListParWithRandomAndPhlos])]]] =
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
          space.install(channels, patterns, continuation)(MATCH_UNLIMITED_PHLOS),
          replaySpace.install(channels, patterns, continuation)(MATCH_UNLIMITED_PHLOS)
        )
    }.sequence

  // TODO: remove default store type
  def create[M[_], F[_]](dataDir: Path, mapSize: Long, storeType: StoreType = LMDB)(
      implicit syncM: Sync[M],
      parallelMF: Parallel[M, F]
  ): M[Runtime[M]] = {
    def dispatchTableCreator(
        space: RhoISpace[M],
        dispatcher: RhoDispatch[M],
        registry: Registry[M],
        shortLeashParams: ShortLeashParams[M],
        blockTime: BlockTime[M]
    ): RhoDispatchMap[M] = {

      import BodyRefs._
      Map(
        STDOUT                       -> SystemProcesses.stdout,
        STDOUT_ACK                   -> SystemProcesses.stdoutAck(space, dispatcher),
        STDERR                       -> SystemProcesses.stderr,
        STDERR_ACK                   -> SystemProcesses.stderrAck(space, dispatcher),
        ED25519_VERIFY               -> SystemProcesses.ed25519Verify(space, dispatcher),
        SHA256_HASH                  -> SystemProcesses.sha256Hash(space, dispatcher),
        KECCAK256_HASH               -> SystemProcesses.keccak256Hash(space, dispatcher),
        BLAKE2B256_HASH              -> SystemProcesses.blake2b256Hash(space, dispatcher),
        SECP256K1_VERIFY             -> SystemProcesses.secp256k1Verify(space, dispatcher),
        REG_LOOKUP                   -> registry.lookup,
        REG_LOOKUP_CALLBACK          -> registry.lookupCallback,
        REG_INSERT                   -> registry.insert,
        REG_INSERT_CALLBACK          -> registry.insertCallback,
        REG_REGISTER_INSERT_CALLBACK -> registry.registerInsertCallback,
        REG_DELETE                   -> registry.delete,
        REG_DELETE_ROOT_CALLBACK     -> registry.deleteRootCallback,
        REG_DELETE_CALLBACK          -> registry.deleteCallback,
        REG_PUBLIC_LOOKUP            -> registry.publicLookup,
        REG_PUBLIC_REGISTER_RANDOM   -> registry.publicRegisterRandom,
        REG_PUBLIC_REGISTER_SIGNED   -> registry.publicRegisterSigned,
        REG_NONCE_INSERT_CALLBACK    -> registry.nonceInsertCallback,
        GET_DEPLOY_PARAMS            -> SystemProcesses.getDeployParams(space, dispatcher, shortLeashParams),
        GET_TIMESTAMP                -> SystemProcesses.blockTime(space, dispatcher, blockTime)
      )
    }

    val urnMap: Map[String, Par] = Map(
      "rho:io:stdout"                -> Bundle(FixedChannels.STDOUT, writeFlag = true),
      "rho:io:stdoutAck"             -> Bundle(FixedChannels.STDOUT_ACK, writeFlag = true),
      "rho:io:stderr"                -> Bundle(FixedChannels.STDERR, writeFlag = true),
      "rho:io:stderrAck"             -> Bundle(FixedChannels.STDERR_ACK, writeFlag = true),
      "rho:registry:lookup"          -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true),
      "rho:registry:insertArbitrary" -> Bundle(FixedChannels.REG_INSERT_RANDOM, writeFlag = true),
      "rho:registry:insertSigned:ed25519" -> Bundle(
        FixedChannels.REG_INSERT_SIGNED,
        writeFlag = true
      ),
      "rho:deploy:params"   -> Bundle(FixedChannels.GET_DEPLOY_PARAMS, writeFlag = true),
      "rho:block:timestamp" -> Bundle(FixedChannels.GET_TIMESTAMP, writeFlag = true)
    )

    val procDefs: List[(Name, Arity, Remainder, BodyRef)] = {
      import BodyRefs._
      List(
        (FixedChannels.STDOUT, 1, None, STDOUT),
        (FixedChannels.STDOUT_ACK, 2, None, STDOUT_ACK),
        (FixedChannels.STDERR, 1, None, STDERR),
        (FixedChannels.STDERR_ACK, 2, None, STDERR_ACK),
        (FixedChannels.ED25519_VERIFY, 4, None, ED25519_VERIFY),
        (FixedChannels.SHA256_HASH, 2, None, SHA256_HASH),
        (FixedChannels.KECCAK256_HASH, 2, None, KECCAK256_HASH),
        (FixedChannels.BLAKE2B256_HASH, 2, None, BLAKE2B256_HASH),
        (FixedChannels.SECP256K1_VERIFY, 4, None, SECP256K1_VERIFY),
        (FixedChannels.REG_LOOKUP, 2, None, REG_PUBLIC_LOOKUP),
        (FixedChannels.REG_INSERT_RANDOM, 2, None, REG_PUBLIC_REGISTER_RANDOM),
        (FixedChannels.REG_INSERT_SIGNED, 4, None, REG_PUBLIC_REGISTER_SIGNED),
        (FixedChannels.GET_DEPLOY_PARAMS, 1, None, GET_DEPLOY_PARAMS),
        (FixedChannels.GET_TIMESTAMP, 1, None, GET_TIMESTAMP)
      )
    }

    for {
      shortLeashParams              <- ShortLeashParams[M](syncM)
      blockTime                     <- BlockTime[M]()
      errorLog                      <- ErrorLog.create[M](syncM)
      setup                         <- setupRSpace[M](dataDir, mapSize, storeType)
      (context, space, replaySpace) = setup
      res                           <- introduceSystemProcesses(space, replaySpace, procDefs)
    } yield {
      assert(res.forall(_.isEmpty))

      implicit val ft: FunctorTell[M, Throwable] = errorLog

      val (reducer, replayReducer) = {
        lazy val replayDispatchTable: RhoDispatchMap[M] =
          dispatchTableCreator(
            replaySpace,
            replayDispatcher,
            replayRegistry,
            shortLeashParams,
            blockTime
          )

        lazy val dispatchTable: RhoDispatchMap[M] =
          dispatchTableCreator(space, dispatcher, registry, shortLeashParams, blockTime)

        lazy val (dispatcher, reducer, registry) =
          RholangAndScalaDispatcher.create(space, dispatchTable, urnMap)

        lazy val (replayDispatcher, replayReducer, replayRegistry) =
          RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable, urnMap)
        (reducer, replayReducer)
      }

      new Runtime[M](
        reducer,
        replayReducer,
        space,
        replaySpace,
        errorLog,
        context,
        shortLeashParams,
        blockTime
      )
    }
  }

  def injectEmptyRegistryRoot[F[_]: Sync](
      space: RhoISpace[F],
      replaySpace: RhoReplayISpace[F]
  ): F[Unit] = {
    // This random value stays dead in the tuplespace, so we can have some fun.
    // This is from Jeremy Bentham's "Defence of Usury"
    val rand = Blake2b512Random(
      ("there can be no such thing as usury: " +
        "for what rate of interest is there that can naturally be more proper than another?")
        .getBytes()
    )
    implicit val MATCH_UNLIMITED_PHLOS: Match[
      BindPattern,
      errors.OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos
    ] = matchListPar(Cost(Integer.MAX_VALUE))
    for {
      spaceResult <- space.produce(
                      Registry.registryRoot,
                      ListParWithRandom(Seq(Registry.emptyMap), rand),
                      persist = false
                    )
      replayResult <- replaySpace.produce(
                       Registry.registryRoot,
                       ListParWithRandom(Seq(Registry.emptyMap), rand),
                       persist = false
                     )
      _ <- spaceResult match {
            case Right(None) =>
              replayResult match {
                case Right(None) => Sync[F].unit
                case Right(Some(_)) =>
                  Sync[F].raiseError(
                    SetupError("Registry insertion in replay fired continuation.")
                  )
                case Left(err) => Sync[F].raiseError(err)
              }
            case Right(Some(_)) =>
              Sync[F].raiseError(SetupError("Registry insertion fired continuation."))
            case Left(err) => Sync[F].raiseError(err)
          }
    } yield ()
  }

  def setupRSpace[F[_]: Sync](
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType
  ): F[(RhoContext, RhoISpace[F], RhoReplayISpace[F])] = {
    def createSpace(
        context: RhoContext
    ): F[(RhoContext, RhoISpace[F], RhoReplayISpace[F])] =
      for {
        space <- RSpace.create[
                  F,
                  Par,
                  BindPattern,
                  OutOfPhlogistonsError.type,
                  ListParWithRandom,
                  ListParWithRandomAndPhlos,
                  TaggedContinuation
                ](context, Branch.MASTER)
        replaySpace <- ReplayRSpace.create[
                        F,
                        Par,
                        BindPattern,
                        OutOfPhlogistonsError.type,
                        ListParWithRandom,
                        ListParWithRandomAndPhlos,
                        TaggedContinuation
                      ](context, Branch.REPLAY)
      } yield (context, space, replaySpace)
    storeType match {
      case InMem =>
        createSpace(Context.createInMemory())
      case LMDB =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createSpace(Context.create(dataDir, mapSize, noTls = true))
      case Mixed =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createSpace(Context.createMixed(dataDir, mapSize))
    }
  }
}
